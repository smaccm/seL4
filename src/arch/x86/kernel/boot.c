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
#include <arch/kernel/vspace.h>
#include <arch/machine/fpu.h>
#include <arch/object/ioport.h>
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

BOOT_CODE static bool_t
create_device_frames(
    cap_t         root_cnode_cap,
    dev_p_regs_t* dev_p_regs
)
{
    seL4_SlotPos     slot_pos_before;
    seL4_SlotPos     slot_pos_after;
    vm_page_size_t frame_size;
    region_t       dev_reg;
    seL4_DeviceRegion   bi_dev_reg;
    cap_t          frame_cap;
    uint32_t       i;
    pptr_t         f;

    for (i = 0; i < dev_p_regs->count; i++) {
        /* write the frame caps of this device region into the root CNode and update the bootinfo */
        dev_reg = paddr_to_pptr_reg(dev_p_regs->list[i]);
        /* use large frames if possible, otherwise use 4K frames */
        if (IS_ALIGNED(dev_reg.start, LARGE_PAGE_BITS) &&
                IS_ALIGNED(dev_reg.end,   LARGE_PAGE_BITS)) {
            frame_size = X86_LargePage;
        } else {
            frame_size = X86_SmallPage;
        }

        slot_pos_before = ndks_boot.slot_pos_cur;

        /* create/provide frame caps covering the region */
        for (f = dev_reg.start; f < dev_reg.end; f += BIT(pageBitsForSize(frame_size))) {
            frame_cap = create_unmapped_it_frame_cap(f, frame_size == X86_LargePage);
            if (!provide_cap(root_cnode_cap, frame_cap)) {
                return false;
            }
        }

        slot_pos_after = ndks_boot.slot_pos_cur;

        /* add device-region entry to bootinfo */
        bi_dev_reg.basePaddr = pptr_to_paddr((void*)dev_reg.start);
        bi_dev_reg.frameSizeBits = pageBitsForSize(frame_size);
        bi_dev_reg.frames = (seL4_SlotRegion) {
            slot_pos_before, slot_pos_after
        };
        ndks_boot.bi_frame->deviceRegions[i] = bi_dev_reg;
    }

    ndks_boot.bi_frame->numDeviceRegions = dev_p_regs->count;
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
    dev_p_regs_t* dev_p_regs,
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
    ia32KSGlobalPD[IA32_KSLOG_IDX] = pde;


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

    /* create the cap for managing thread domains */
    create_domain_cap(root_cnode_cap);

    /* create the IRQ CNode */
    if (!create_irq_cnode()) {
        return false;
    }

    /* initialise the IRQ states and provide the IRQ control cap */
    init_irqs(root_cnode_cap);

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

    /* convert the remaining free memory into UT objects and provide the caps */
    if (!create_untypeds(root_cnode_cap, boot_mem_reuse_reg)) {
        return false;
    }
    /* WARNING: alloc_region() must not be called anymore after here! */

    /* create device frames */
    if (!create_device_frames(root_cnode_cap, dev_p_regs)) {
        return false;
    }

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

    /* initialise MSRs (needs an initialised TSS) */
    init_sysenter_msrs();

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

    return true;
}
