/*
 * Copyright 2014, General Dynamics C4 Systems
 *
 * This software may be distributed and modified according to the terms of
 * the GNU General Public License version 2. Note that NO WARRANTY is provided.
 * See "LICENSE_GPLv2.txt" for details.
 *
 * @TAG(GD_GPL)
 */

#include <config.h>
#include <kernel/boot.h>
#include <machine/io.h>
#include <model/statedata.h>
#include <object/interrupt.h>
#include <arch/object/interrupt.h>
#include <arch/machine.h>
#include <arch/kernel/apic.h>
#include <arch/kernel/boot.h>
#include <arch/kernel/boot_sys.h>
#include <arch/kernel/tsc.h>
#include <arch/kernel/vspace.h>
#include <arch/machine/fpu.h>
#include <arch/object/ioport.h>
#include <arch/object/vtx.h>
#include <arch/linker.h>
#include <util.h>

#include <plat/machine/intel-vtd.h>

/* functions exactly corresponding to abstract specification */

BOOT_CODE static void
init_irqs(cap_t root_cnode_cap)
{
    irq_t i;

    for (i = 0; i <= maxIRQ; i++) {
        if (i == irq_timer) {
            setIRQState(IRQTimer, i);
        } else if (i == irq_iommu) {
            setIRQState(IRQReserved, i);
        } else if (i == 2 && config_set(CONFIG_IRQ_PIC)) {
            /* cascaded legacy PIC */
            setIRQState(IRQReserved, i);
        } else if (i >= irq_isa_min && i <= irq_isa_max) {
            if (config_set(CONFIG_IRQ_PIC)) {
                setIRQState(IRQInactive, i);
            } else {
                setIRQState(IRQReserved, i);
            }
        } else if (i >= irq_user_min && i <= irq_user_max) {
            if (config_set(CONFIG_IRQ_IOAPIC)) {
                setIRQState(IRQInactive, i);
            } else {
                setIRQState(IRQReserved, i);
            }
        } else {
            setIRQState(IRQReserved, i);
        }
    }
    Arch_irqStateInit();
    /* provide the IRQ control cap */
    write_slot(SLOT_PTR(pptr_of_cap(root_cnode_cap), seL4_CapIRQControl), cap_irq_control_cap_new());
}

/* The maximum number of reserved regions we have is 1 for each physical memory region (+ MAX_NUM_FREEMEM_REG)
 * plus 1 for each kernel device. For kernel devices we have the ioapics (+ CONFIG_MAX_NUM_IOAPIC),
 * iommus (+ MAX_NUM_DRHU), apic (+ 1) and the reserved MSI region (+ 1) */
#define NUM_RESERVED_REGIONS    (MAX_NUM_FREEMEM_REG + CONFIG_MAX_NUM_IOAPIC + MAX_NUM_DRHU + 2)
typedef struct allocated_p_region {
    p_region_t  regs[NUM_RESERVED_REGIONS];
    word_t      cur_pos;
} allocated_p_region_t;

BOOT_DATA static allocated_p_region_t allocated_p_regions;

BOOT_CODE static void
merge_regions(void)
{
    unsigned int i, j;
    /* Walk through all the regions and see if any can get merged */
    for (i = 1; i < allocated_p_regions.cur_pos;) {
        if (allocated_p_regions.regs[i - 1].end == allocated_p_regions.regs[i].start) {
            /* move this down */
            allocated_p_regions.regs[i - 1].end = allocated_p_regions.regs[i].end;
            /* fill the rest down */
            for (j = i; j < allocated_p_regions.cur_pos - 1; j++) {
                allocated_p_regions.regs[j] = allocated_p_regions.regs[j + 1];
            }
            allocated_p_regions.cur_pos--;
            /* don't increment 'i' since we want to recheck that the
             * region we just moved to this slot doesn't also need merging */
        } else {
            i++;
        }
    }
}

static UNUSED BOOT_CODE bool_t p_region_overlaps(p_region_t reg)
{
    unsigned int i;
    for (i = 0; i < allocated_p_regions.cur_pos; i++) {
        if (allocated_p_regions.regs[i].start < reg.end &&
                allocated_p_regions.regs[i].end > reg.start) {
            return true;
        }
    }
    return false;
}

BOOT_CODE bool_t
add_allocated_p_region(p_region_t reg)
{
    unsigned int i, j;

    assert(reg.start <= reg.end);
    assert(!p_region_overlaps(reg));

    /* Walk the existing regions and see if we can merge with an existing
     * region, or insert in order */
    for (i = 0; i < allocated_p_regions.cur_pos; i++) {
        /* see if we can merge before or after this region */
        if (allocated_p_regions.regs[i].end == reg.start) {
            allocated_p_regions.regs[i].end = reg.end;
            merge_regions();
            return true;
        }
        if (allocated_p_regions.regs[i].start == reg.end) {
            allocated_p_regions.regs[i].start = reg.start;
            merge_regions();
            return true;
        }
        /* see if this new one should be inserted before */
        if (reg.end < allocated_p_regions.regs[i].start) {
            /* ensure there's space to bump the regions up */
            if (allocated_p_regions.cur_pos + 1 == NUM_RESERVED_REGIONS) {
                printf("Ran out of reserved physical regions\n");
                return false;
            }
            /* Copy the regions up to make a gap */
            for (j = allocated_p_regions.cur_pos; j != i; j--) {
                allocated_p_regions.regs[j] = allocated_p_regions.regs[j - 1];
            }
            /* Put this region in the gap */
            allocated_p_regions.regs[i] = reg;
            allocated_p_regions.cur_pos++;
            return true;
        }
    }

    /* nothing else matched, put this one at the end */
    if (i + 1 == NUM_RESERVED_REGIONS) {
        printf("Ran out of reserved physical regions\n");
        return false;
    }
    allocated_p_regions.regs[i] = reg;
    allocated_p_regions.cur_pos = i + 1;
    return true;
}

BOOT_CODE void
init_allocated_p_regions()
{
    allocated_p_regions.cur_pos = 0;
}

BOOT_CODE static bool_t
create_untypeds(
    cap_t root_cnode_cap,
    region_t boot_mem_reuse_reg)
{
    seL4_SlotPos     slot_pos_before;
    seL4_SlotPos     slot_pos_after;
    word_t      i;

    paddr_t     start = 0;

    slot_pos_before = ndks_boot.slot_pos_cur;
    create_kernel_untypeds(root_cnode_cap, boot_mem_reuse_reg, slot_pos_before);

    for (i = 0; i < allocated_p_regions.cur_pos; i++) {
        if (start != allocated_p_regions.regs[i].start) {
            if (!create_untypeds_for_region(root_cnode_cap, true,
            paddr_to_pptr_reg((p_region_t) {
            start, allocated_p_regions.regs[i].start
            }),
                                            slot_pos_before)) {
                return false;
            }
        }
        start = allocated_p_regions.regs[i].end;
    }

    if (start != PADDR_USER_DEVICE_TOP) {
        if (!create_untypeds_for_region(root_cnode_cap, true,
        paddr_to_pptr_reg((p_region_t) {
        start, PADDR_USER_DEVICE_TOP
    }),
                                    slot_pos_before)) {
            return false;
        }
    }

    slot_pos_after = ndks_boot.slot_pos_cur;
    ndks_boot.bi_frame->untyped = (seL4_SlotRegion) {
        slot_pos_before, slot_pos_after
    };
    return true;
}

BOOT_CODE static void
init_freemem(p_region_t ui_p_reg, mem_p_regs_t mem_p_regs)
{
    word_t i;
    /* we are guaranteed that we started loading the user image after the kernel
     * so we only include addresses above ui_info.p_reg.end */
    pptr_t floor = ui_p_reg.end;
    for (i = 0; i < MAX_NUM_FREEMEM_REG; i++) {
        ndks_boot.freemem[i] = REG_EMPTY;
    }
    for (i = 0; i < mem_p_regs.count; i++) {
        pptr_t start = mem_p_regs.list[i].start;
        pptr_t end = mem_p_regs.list[i].end;
        if (start < floor) {
            start = floor;
        }
        if (end < floor) {
            end = floor;
        }
        insert_region(paddr_to_pptr_reg((p_region_t) {
            start, end
        }));
    }
}

/* This function initialises a node's kernel state. It does NOT initialise the CPU. */

BOOT_CODE bool_t
init_sys_state(
    cpu_id_t      cpu_id,
    mem_p_regs_t  mem_p_regs,
    ui_info_t     ui_info,
    p_region_t    boot_mem_reuse_p_reg,
    /* parameters below not modeled in abstract specification */
    uint32_t      num_drhu,
    paddr_t*      drhu_list,
    acpi_rmrr_list_t *rmrr_list
)
{
    cap_t         root_cnode_cap;
    vptr_t        bi_frame_vptr;
    vptr_t        ipcbuf_vptr;
    cap_t         it_vspace_cap;
    cap_t         it_ap_cap;
    cap_t         ipcbuf_cap;
    pptr_t        bi_frame_pptr;
    create_frames_of_region_ret_t create_frames_ret;
#ifdef CONFIG_BENCHMARK_USE_KERNEL_LOG_BUFFER
    vm_attributes_t buffer_attr = {{ 0 }};
    word_t paddr;
    pde_t pde;
#endif /* CONFIG_BENCHMARK_USE_KERNEL_LOG_BUFFER */

    /* convert from physical addresses to kernel pptrs */
    region_t ui_reg             = paddr_to_pptr_reg(ui_info.p_reg);
    region_t boot_mem_reuse_reg = paddr_to_pptr_reg(boot_mem_reuse_p_reg);

    /* convert from physical addresses to userland vptrs */
    v_region_t ui_v_reg;
    v_region_t it_v_reg;
    ui_v_reg.start = ui_info.p_reg.start - ui_info.pv_offset;
    ui_v_reg.end   = ui_info.p_reg.end   - ui_info.pv_offset;

    ipcbuf_vptr = ui_v_reg.end;
    bi_frame_vptr = ipcbuf_vptr + BIT(PAGE_BITS);

    /* The region of the initial thread is the user image + ipcbuf and boot info */
    it_v_reg.start = ui_v_reg.start;
    it_v_reg.end = bi_frame_vptr + BIT(PAGE_BITS);

    init_freemem(ui_info.p_reg, mem_p_regs);

#ifdef CONFIG_BENCHMARK_USE_KERNEL_LOG_BUFFER
    /* allocate and create the log buffer */
    buffer_attr.words[0] = IA32_PAT_MT_WRITE_THROUGH;

    paddr = pptr_to_paddr((void *) alloc_region(pageBitsForSize(X86_LargePage)));

    /* allocate a large frame for logging */
    pde = x86_make_pde_mapping(paddr, buffer_attr);
#ifdef X86_64
#ifdef CONFIG_HUGE_PAGE
    x64KSGlobalPD[BIT(PD_BITS) - 1] = pde;
#else
#endif /* CONFIG_HUGE_PAGE */
    x64KSGlobalPDs[BIT(PDPT_BITS) - 1][BIT(PD_BITS) - 1] = pde;
#else
    ia32KSGlobalPD[IA32_KSLOG_IDX] = pde;
#endif /* X86_64 */


    /* flush the tlb */
    invalidateTranslationAll();

    /* if we crash here, the log isn't working */
#ifdef CONFIG_DEBUG_BUILD
#if CONFIG_MAX_NUM_TRACE_POINTS > 0
    printf("Testing log\n");
    ksLog[0].data = 0xdeadbeef;
    printf("Wrote to ksLog %x\n", ksLog[0].data);
    assert(ksLog[0].data == 0xdeadbeef);
#endif /* CONFIG_MAX_NUM_TRACE_POINTS */
#endif /* CONFIG_DEBUG_BUILD */
#endif /* CONFIG_BENCHMARK_USE_KERNEL_LOG_BUFFER */

    /* create the root cnode */
    root_cnode_cap = create_root_cnode();

    /* create the IO port cap */
    write_slot(
        SLOT_PTR(pptr_of_cap(root_cnode_cap), seL4_CapIOPort),
        cap_io_port_cap_new(
            0,                /* first port */
            NUM_IO_PORTS - 1 /* last port  */
        )
    );

    /* create the IRQ CNode */
    if (!create_irq_cnode()) {
        return false;
    }

    /* initialise the IRQ states and provide the IRQ control cap */
    init_irqs(root_cnode_cap);

    /* create sched control cap */
    write_slot(SLOT_PTR(pptr_of_cap(root_cnode_cap), seL4_CapSchedControl),
               cap_sched_control_cap_new());

    /* create the bootinfo frame */
    bi_frame_pptr = allocate_bi_frame(0, 1, ipcbuf_vptr);
    if (!bi_frame_pptr) {
        return false;
    }

    /* Construct an initial address space with enough virtual addresses
     * to cover the user image + ipc buffer and bootinfo frames */
    it_vspace_cap = create_it_address_space(root_cnode_cap, it_v_reg);
    if (cap_get_capType(it_vspace_cap) == cap_null_cap) {
        return false;
    }

    /* Create and map bootinfo frame cap */
    create_bi_frame_cap(
        root_cnode_cap,
        it_vspace_cap,
        bi_frame_pptr,
        bi_frame_vptr
    );

    /* create the initial thread's IPC buffer */
    ipcbuf_cap = create_ipcbuf_frame(root_cnode_cap, it_vspace_cap, ipcbuf_vptr);
    if (cap_get_capType(ipcbuf_cap) == cap_null_cap) {
        return false;
    }

    /* create all userland image frames */
    create_frames_ret =
        create_frames_of_region(
            root_cnode_cap,
            it_vspace_cap,
            ui_reg,
            true,
            ui_info.pv_offset
        );
    if (!create_frames_ret.success) {
        return false;
    }
    ndks_boot.bi_frame->userImageFrames = create_frames_ret.region;

    /* create the initial thread's ASID pool */
    it_ap_cap = create_it_asid_pool(root_cnode_cap);
    if (cap_get_capType(it_ap_cap) == cap_null_cap) {
        return false;
    }
    write_it_asid_pool(it_ap_cap, it_vspace_cap);

    x86KSfpuOwner = NULL;
    x86KStscMhz = tsc_init();
    ndks_boot.bi_frame->archInfo = x86KStscMhz;

    if (x86KStscMhz == 0) {
        return false;
    }

    /* create the idle thread */
    if (!create_idle_thread()) {
        return false;
    }

    /* create the initial thread */
    if (!create_initial_thread(
                root_cnode_cap,
                it_vspace_cap,
                ui_info.v_entry,
                bi_frame_vptr,
                ipcbuf_vptr,
                ipcbuf_cap
            )) {
        return false;
    }

    if (config_set(CONFIG_IOMMU)) {
        /* initialise VTD-related data structures and the IOMMUs */
        if (!vtd_init(cpu_id, num_drhu, rmrr_list)) {
            return false;
        }

        /* write number of IOMMU PT levels into bootinfo */
        ndks_boot.bi_frame->numIOPTLevels = x86KSnumIOPTLevels;

        /* write IOSpace master cap */
        write_slot(SLOT_PTR(pptr_of_cap(root_cnode_cap), seL4_CapIOSpace), master_iospace_cap());
    } else {
        ndks_boot.bi_frame->numIOPTLevels = -1;
    }

    /* create all of the untypeds. Both devices and kernel window memory */
    if (!create_untypeds(root_cnode_cap, boot_mem_reuse_reg)) {
        return false;
    }
    /* WARNING: alloc_region() must not be called anymore after here! */

    /* finalise the bootinfo frame */
    bi_finalise();

    return true;
}

/* This function initialises the CPU. It does NOT initialise any kernel state. */

BOOT_CODE bool_t
init_cpu(
    bool_t   mask_legacy_irqs
)
{
    /* Initialize CPUID stuff asap, so that other subsystems can do feature
     * detection properly.
     */
    if (!x86_cpuid_initialize()) {
        /* If it returned false because of the vendor, print a message
         * and allow it to continue -- let the user experiment with seL4 on
         * x86 clones if s/he really wants to.
         */
        printf("Warning: Your x86 CPU has an unsupported vendor, '%s'.\n"
               "\tYour setup may not be able to competently run seL4 as "
               "intended.\n"
               "\tCurrently supported x86 vendors are AMD and Intel.\n",
               x86_cpuid_get_identity()->vendor_string);
    }

    /* initialise virtual-memory-related data structures */
    if (!init_vm_state()) {
        return false;
    }

    /* initialise CPU's descriptor table registers (GDTR, IDTR, LDTR, TR) */
    init_dtrs();

    if (config_set(CONFIG_SYSENTER)) {
        /* initialise MSRs (needs an initialised TSS) */
        init_sysenter_msrs();
    } else if (config_set(CONFIG_SYSCALL)) {
        init_syscall_msrs();
    } else {
        return false;
    }

    /* setup additional PAT MSR */
    if (!init_pat_msr()) {
        return false;
    }

    /* Initialize hardware breakpoints */
    if (!Arch_initHardwareBreakpoints()) {
        return false;
    }

    /* initialise floating-point unit */
    if (!Arch_initFpu()) {
        return false;
    }

    /* initialise local APIC */
    if (!apic_init(mask_legacy_irqs)) {
        return false;
    }

#ifdef CONFIG_DEBUG_DISABLE_PREFETCHERS
    if (!disablePrefetchers()) {
        return false;
    }
#endif

#ifdef CONFIG_VTX
    /* initialise Intel VT-x extensions */
    if (!vtx_init()) {
        return false;
    }
#endif

    return true;
}
