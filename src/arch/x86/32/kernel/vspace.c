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
#include <api/syscall.h>
#include <machine/io.h>
#include <kernel/boot.h>
#include <model/statedata.h>
#include <arch/kernel/vspace.h>
#include <arch/kernel/boot.h>
#include <arch/api/invocation.h>
#include <benchmark_track.h>

/* 'gdt_idt_ptr' is declared globally because of a C-subset restriction.
 * It is only used in init_drts(), which therefore is non-reentrant.
 */
gdt_idt_ptr_t gdt_idt_ptr;

/* initialise the Task State Segment (TSS) */

BOOT_CODE void
init_tss(tss_t* tss)
{
    tss_ptr_new(
        tss,
        sizeof(*tss),   /* io_map_base  */
        0,              /* trap         */
        SEL_NULL,       /* sel_ldt      */
        SEL_NULL,       /* gs           */
        SEL_NULL,       /* fs           */
        SEL_NULL,       /* ds           */
        SEL_NULL,       /* ss           */
        SEL_NULL,       /* cs           */
        SEL_NULL,       /* es           */
        0,              /* edi          */
        0,              /* esi          */
        0,              /* ebp          */
        0,              /* esp          */
        0,              /* ebx          */
        0,              /* edx          */
        0,              /* ecx          */
        0,              /* eax          */
        0,              /* eflags       */
        0,              /* eip          */
        0,              /* cr3          */
        SEL_NULL,       /* ss2          */
        0,              /* esp2         */
        SEL_NULL,       /* ss1          */
        0,              /* esp1         */
        SEL_DS_0,       /* ss0          */
        0,              /* esp0         */
        0               /* prev_task    */
    );
    memset(&x86KStss.io_map[0], 0xff, sizeof(x86KStss.io_map));
}
/* initialise Global Descriptor Table (GDT) */

BOOT_CODE void
init_gdt(gdt_entry_t* gdt, tss_t* tss)
{
    uint32_t tss_addr = (uint32_t)tss;

    /* Set the NULL descriptor */
    gdt[GDT_NULL] = gdt_entry_gdt_null_new();

    /* 4GB flat kernel code segment on ring 0 descriptor */
    gdt[GDT_CS_0] = gdt_entry_gdt_code_new(
                        0,      /* Base high 8 bits             */
                        1,      /* Granularity                  */
                        1,      /* Operation size               */
                        0,      /* Available                    */
                        0xf,    /* Segment limit high 4 bits    */
                        1,      /* Present                      */
                        0,      /* Descriptor privilege level   */
                        1,      /* readable                     */
                        1,      /* accessed                     */
                        0,      /* Base middle 8 bits           */
                        0,      /* Base low 16 bits             */
                        0xffff  /* Segment limit low 16 bits    */
                    );

    /* 4GB flat kernel data segment on ring 0 descriptor */
    gdt[GDT_DS_0] = gdt_entry_gdt_data_new(
                        0,      /* Base high 8 bits             */
                        1,      /* Granularity                  */
                        1,      /* Operation size               */
                        0,      /* Available                    */
                        0xf,    /* Segment limit high 4 bits    */
                        1,      /* Present                      */
                        0,      /* Descriptor privilege level   */
                        1,      /* writable                     */
                        1,      /* accessed                     */
                        0,      /* Base middle 8 bits           */
                        0,      /* Base low 16 bits             */
                        0xffff  /* Segment limit low 16 bits    */
                    );

    /* 4GB flat userland code segment on ring 3 descriptor */
    gdt[GDT_CS_3] = gdt_entry_gdt_code_new(
                        0,      /* Base high 8 bits             */
                        1,      /* Granularity                  */
                        1,      /* Operation size               */
                        0,      /* Available                    */
                        0xf,    /* Segment limit high 4 bits    */
                        1,      /* Present                      */
                        3,      /* Descriptor privilege level   */
                        1,      /* readable                     */
                        1,      /* accessed                     */
                        0,      /* Base middle 8 bits           */
                        0,      /* Base low 16 bits             */
                        0xffff  /* Segment limit low 16 bits    */
                    );

    /* 4GB flat userland data segment on ring 3 descriptor */
    gdt[GDT_DS_3] = gdt_entry_gdt_data_new(
                        0,      /* Base high 8 bits             */
                        1,      /* Granularity                  */
                        1,      /* Operation size               */
                        0,      /* Available                    */
                        0xf,    /* Segment limit high 4 bits    */
                        1,      /* Present                      */
                        3,      /* Descriptor privilege level   */
                        1,      /* writable                     */
                        1,      /* accessed                     */
                        0,      /* Base middle 8 bits           */
                        0,      /* Base low 16 bits             */
                        0xffff  /* Segment limit low 16 bits    */
                    );

    /* Task State Segment (TSS) descriptor */
    gdt[GDT_TSS] = gdt_entry_gdt_tss_new(
                       tss_addr >> 24,              /* base_high 8 bits     */
                       0,                           /* granularity          */
                       0,                           /* avl                  */
                       0,                           /* limit_high 4 bits    */
                       1,                           /* present              */
                       0,                           /* dpl                  */
                       0,                           /* busy                 */
                       1,                           /* always_true          */
                       (tss_addr >> 16) & 0xff,     /* base_mid 8 bits      */
                       (tss_addr & 0xffff),         /* base_low 16 bits     */
                       sizeof(tss_io_t) - 1         /* limit_low 16 bits    */
                   );

    /* pre-init the userland data segment used for TLS */
    gdt[GDT_TLS] = gdt_entry_gdt_data_new(
                       0,      /* Base high 8 bits             */
                       1,      /* Granularity                  */
                       1,      /* Operation size               */
                       0,      /* Available                    */
                       0xf,    /* Segment limit high 4 bits    */
                       1,      /* Present                      */
                       3,      /* Descriptor privilege level   */
                       1,      /* writable                     */
                       1,      /* accessed                     */
                       0,      /* Base middle 8 bits           */
                       0,      /* Base low 16 bits             */
                       0xffff  /* Segment limit low 16 bits    */
                   );

    /* pre-init the userland data segment used for the IPC buffer */
    gdt[GDT_IPCBUF] = gdt_entry_gdt_data_new(
                          0,      /* Base high 8 bits             */
                          1,      /* Granularity                  */
                          1,      /* Operation size               */
                          0,      /* Available                    */
                          0xf,    /* Segment limit high 4 bits    */
                          1,      /* Present                      */
                          3,      /* Descriptor privilege level   */
                          1,      /* writable                     */
                          1,      /* accessed                     */
                          0,      /* Base middle 8 bits           */
                          0,      /* Base low 16 bits             */
                          0xffff  /* Segment limit low 16 bits    */
                      );
}

/* initialise the Interrupt Descriptor Table (IDT) */

BOOT_CODE void
init_idt_entry(idt_entry_t* idt, interrupt_t interrupt, void(*handler)(void))
{
    uint32_t handler_addr = (uint32_t)handler;
    uint32_t dpl = 3;

    if (interrupt < int_trap_min && interrupt != int_software_break_request) {
        dpl = 0;
    }

    idt[interrupt] = idt_entry_interrupt_gate_new(
                         handler_addr >> 16,   /* offset_high  */
                         1,                    /* present      */
                         dpl,                  /* dpl          */
                         1,                    /* gate_size    */
                         SEL_CS_0,             /* seg_selector */
                         handler_addr & 0xffff /* offset_low   */
                     );
}

BOOT_CODE bool_t
map_kernel_window(
    uint32_t num_ioapic,
    paddr_t*   ioapic_paddrs,
    uint32_t   num_drhu,
    paddr_t*   drhu_list
)
{
    paddr_t  phys;
    uint32_t idx;
    pde_t    pde;
    pte_t    pte;
    unsigned int UNUSED i;

    if (config_set(CONFIG_PAE_PAGING)) {
        for (idx = 0; idx < BIT(PDPT_BITS); idx++) {
            pdpte_ptr_new(&ia32KSGlobalPDPT[idx],
                          pptr_to_paddr(&ia32KSGlobalPD[idx * BIT(PD_BITS)]),
                          0, /* avl*/
                          0, /* cache_disabled */
                          0, /* write_through */
                          1  /* present */
                         );
        }
    }

    /* Mapping of PPTR_BASE (virtual address) to kernel's PADDR_BASE
     * up to end of virtual address space except for the last large page.
     */
    phys = PADDR_BASE;
    idx = PPTR_BASE >> LARGE_PAGE_BITS;

#ifdef CONFIG_BENCHMARK_USE_KERNEL_LOG_BUFFER
    /* steal the last large for logging */
    while (idx < BIT(PD_BITS + PDPT_BITS) - 2) {
#else
    while (idx < BIT(PD_BITS + PDPT_BITS) - 1) {
#endif /* CONFIG_BENCHMARK_USE_KERNEL_LOG_BUFFER */
        pde = pde_pde_large_new(
                  phys,   /* page_base_address    */
                  0,      /* pat                  */
                  0,      /* avl                  */
                  1,      /* global               */
                  0,      /* dirty                */
                  0,      /* accessed             */
                  0,      /* cache_disabled       */
                  0,      /* write_through        */
                  0,      /* super_user           */
                  1,      /* read_write           */
                  1       /* present              */
              );
        ia32KSGlobalPD[idx] = pde;
        phys += BIT(LARGE_PAGE_BITS);
        idx++;
    }

    /* crosscheck whether we have mapped correctly so far */
    assert(phys == PADDR_TOP);

#ifdef CONFIG_BENCHMARK_USE_KERNEL_LOG_BUFFER
    /* mark the address of the log. We will map it
        * in later with the correct attributes, but we need
        * to wait until we can call alloc_region. */
    ksLog = (void *) paddr_to_pptr(phys);
    phys += BIT(LARGE_PAGE_BITS);
    assert(idx == IA32_KSLOG_IDX);
    idx++;
#endif /* CONFIG_BENCHMARK_USE_KERNEL_LOG_BUFFER */

    /* map page table of last 4M of virtual address space to page directory */
    pde = pde_pde_small_new(
              pptr_to_paddr(ia32KSGlobalPT), /* pt_base_address  */
              0,                 /* avl              */
              0,                 /* accessed         */
              0,                 /* cache_disabled   */
              0,                 /* write_through    */
              0,                 /* super_user       */
              1,                 /* read_write       */
              1                  /* present          */
          );
    ia32KSGlobalPD[idx] = pde;

    /* Start with an empty guard page preceding the stack. */
    idx = 0;
    pte = pte_new(
              0,      /* page_base_address    */
              0,      /* avl                  */
              0,      /* global               */
              0,      /* pat                  */
              0,      /* dirty                */
              0,      /* accessed             */
              0,      /* cache_disabled       */
              0,      /* write_through        */
              0,      /* super_user           */
              0,      /* read_write           */
              0       /* present              */
          );
    ia32KSGlobalPT[idx] = pte;
    idx++;

    /* null mappings up to PPTR_KDEV */

    while (idx < (PPTR_KDEV & MASK(LARGE_PAGE_BITS)) >> PAGE_BITS) {
        pte = pte_new(
                  0,      /* page_base_address    */
                  0,      /* avl                  */
                  0,      /* global               */
                  0,      /* pat                  */
                  0,      /* dirty                */
                  0,      /* accessed             */
                  0,      /* cache_disabled       */
                  0,      /* write_through        */
                  0,      /* super_user           */
                  0,      /* read_write           */
                  0       /* present              */
              );
        ia32KSGlobalPT[idx] = pte;
        idx++;
    }

    /* map kernel devices (devices only used by the kernel) */
    if (!map_kernel_window_devices(ia32KSGlobalPT, num_ioapic, ioapic_paddrs, num_drhu, drhu_list)) {
        return false;
    }

    invalidatePageStructureCache();
    return true;
}

/* Note: this function will invalidate any pointers previously returned from this function */
BOOT_CODE void*
map_temp_boot_page(void* entry, uint32_t large_pages)
{
    void* replacement_vaddr;
    unsigned int i;
    unsigned int offset_in_page;

    unsigned int phys_pg_start = (unsigned int)(entry) & ~MASK(LARGE_PAGE_BITS);
    unsigned int virt_pd_start = (PPTR_BASE >> LARGE_PAGE_BITS) - large_pages;
    unsigned int virt_pg_start = PPTR_BASE - (large_pages << LARGE_PAGE_BITS);

    for (i = 0; i < large_pages; ++i) {
        unsigned int pg_offset = i << LARGE_PAGE_BITS; // num pages since start * page size

        pde_pde_large_ptr_new(get_boot_pd() + virt_pd_start + i,
                              phys_pg_start + pg_offset, /* physical address */
                              0, /* pat            */
                              0, /* avl            */
                              1, /* global         */
                              0, /* dirty          */
                              0, /* accessed       */
                              0, /* cache_disabled */
                              0, /* write_through  */
                              0, /* super_user     */
                              1, /* read_write     */
                              1  /* present        */
                             );
        invalidateTranslationSingle(virt_pg_start + pg_offset);
    }

    // assign replacement virtual addresses page
    offset_in_page = (unsigned int)(entry) & MASK(LARGE_PAGE_BITS);
    replacement_vaddr = (void*)(virt_pg_start + offset_in_page);

    invalidatePageStructureCache();

    return replacement_vaddr;
}

/* initialise CPU's descriptor table registers (GDTR, IDTR, LDTR, TR) */

BOOT_CODE void
init_dtrs(void)
{
    /* setup the GDT pointer and limit and load into GDTR */
    gdt_idt_ptr.limit = (sizeof(gdt_entry_t) * GDT_ENTRIES) - 1;
    gdt_idt_ptr.base = (uint32_t)x86KSgdt;
    ia32_install_gdt(&gdt_idt_ptr);

    /* setup the IDT pointer and limit and load into IDTR */
    gdt_idt_ptr.limit = (sizeof(idt_entry_t) * (int_max + 1)) - 1;
    gdt_idt_ptr.base = (uint32_t)x86KSidt;
    ia32_install_idt(&gdt_idt_ptr);

    /* load NULL LDT selector into LDTR */
    ia32_install_ldt(SEL_NULL);

    /* load TSS selector into Task Register (TR) */
    ia32_install_tss(SEL_TSS);

    if (config_set(CONFIG_FSGSBASE_MSR)) {
        ia32_load_fs(SEL_TLS);
        ia32_load_gs(SEL_IPCBUF);
    }
}

static BOOT_CODE cap_t
create_it_page_table_cap(cap_t vspace_cap, pptr_t pptr, vptr_t vptr, asid_t asid)
{
    cap_t cap;
    cap = cap_page_table_cap_new(
              1,    /* capPTIsMapped      */
              asid, /* capPTMappedASID    */
              vptr, /* capPTMappedAddress */
              pptr  /* capPTBasePtr       */
          );
    if (asid != asidInvalid) {
        map_it_pt_cap(vspace_cap, cap);
    }
    return cap;
}

static BOOT_CODE cap_t
create_it_page_directory_cap(cap_t vspace_cap, pptr_t pptr, vptr_t vptr, asid_t asid)
{
    cap_t cap;
    cap = cap_page_directory_cap_new(
              true,    /* capPDIsMapped   */
              IT_ASID, /* capPDMappedASID */
              vptr,    /* capPDMappedAddress */
              pptr  /* capPDBasePtr    */
          );
    if (asid != asidInvalid && cap_get_capType(vspace_cap) != cap_null_cap) {
        map_it_pd_cap(vspace_cap, cap);
    }
    return cap;
}

/* Create an address space for the initial thread.
 * This includes page directory and page tables */
BOOT_CODE cap_t
create_it_address_space(cap_t root_cnode_cap, v_region_t it_v_reg)
{
    cap_t      vspace_cap;
    vptr_t     vptr;
    pptr_t     pptr;
    seL4_SlotPos slot_pos_before;
    seL4_SlotPos slot_pos_after;

    slot_pos_before = ndks_boot.slot_pos_cur;
    if (PDPT_BITS == 0) {
        cap_t pd_cap;
        pptr_t pd_pptr;
        /* just create single PD obj and cap */
        pd_pptr = alloc_region(seL4_PageDirBits);
        if (!pd_pptr) {
            return cap_null_cap_new();
        }
        memzero(PDE_PTR(pd_pptr), 1 << seL4_PageDirBits);
        copyGlobalMappings((vspace_root_t*)pd_pptr);
        pd_cap = create_it_page_directory_cap(cap_null_cap_new(), pd_pptr, 0, IT_ASID);
        if (!provide_cap(root_cnode_cap, pd_cap)) {
            return cap_null_cap_new();
        }
        write_slot(SLOT_PTR(pptr_of_cap(root_cnode_cap), seL4_CapInitThreadVSpace), pd_cap);
        vspace_cap = pd_cap;
    } else {
        cap_t pdpt_cap;
        pptr_t pdpt_pptr;
        unsigned int i;
        /* create a PDPT obj and cap */
        pdpt_pptr = alloc_region(seL4_PDPTBits);
        if (!pdpt_pptr) {
            return cap_null_cap_new();
        }
        memzero(PDPTE_PTR(pdpt_pptr), 1 << seL4_PDPTBits);
        pdpt_cap = cap_pdpt_cap_new(
                       true,       /* capPDPTISMapped */
                       IT_ASID,    /* capPDPTMappedASID */
                       pdpt_pptr        /* capPDPTBasePtr */
                   );
        /* create all PD objs and caps necessary to cover userland image. For simplicity
         * to ensure we also cover the kernel window we create all PDs */
        for (i = 0; i < BIT(PDPT_BITS); i++) {
            /* The compiler is under the mistaken belief here that this shift could be
             * undefined. However, in the case that it would be undefined this code path
             * is not reachable because PDPT_BITS == 0 (see if statement at the top of
             * this function), so to work around it we must both put in a redundant
             * if statement AND place the shift in a variable. While the variable
             * will get compiled away it prevents the compiler from evaluating
             * the 1 << 32 as a constant when it shouldn't
             * tl;dr gcc evaluates constants even if code is unreachable */
            int shift = (PD_BITS + PT_BITS + PAGE_BITS);
            if (shift != 32) {
                vptr = i << shift;
            } else {
                return cap_null_cap_new();
            }

            pptr = alloc_region(seL4_PageDirBits);
            if (!pptr) {
                return cap_null_cap_new();
            }
            memzero(PDE_PTR(pptr), 1 << seL4_PageDirBits);
            if (!provide_cap(root_cnode_cap,
                             create_it_page_directory_cap(pdpt_cap, pptr, vptr, IT_ASID))
               ) {
                return cap_null_cap_new();
            }
        }
        /* now that PDs exist we can copy the global mappings */
        copyGlobalMappings((vspace_root_t*)pdpt_pptr);
        write_slot(SLOT_PTR(pptr_of_cap(root_cnode_cap), seL4_CapInitThreadVSpace), pdpt_cap);
        vspace_cap = pdpt_cap;
    }

    /* create all PT objs and caps necessary to cover userland image */

    for (vptr = ROUND_DOWN(it_v_reg.start, PT_BITS + PAGE_BITS);
            vptr < it_v_reg.end;
            vptr += BIT(PT_BITS + PAGE_BITS)) {
        pptr = alloc_region(seL4_PageTableBits);
        if (!pptr) {
            return cap_null_cap_new();
        }
        memzero(PTE_PTR(pptr), 1 << seL4_PageTableBits);
        if (!provide_cap(root_cnode_cap,
                         create_it_page_table_cap(vspace_cap, pptr, vptr, IT_ASID))
           ) {
            return cap_null_cap_new();
        }
    }

    slot_pos_after = ndks_boot.slot_pos_cur;
    ndks_boot.bi_frame->userImagePaging = (seL4_SlotRegion) {
        slot_pos_before, slot_pos_after
    };

    return vspace_cap;
}

static BOOT_CODE cap_t
create_it_frame_cap(pptr_t pptr, vptr_t vptr, asid_t asid, bool_t use_large, seL4_Word map_type)
{
    vm_page_size_t frame_size;

    if (use_large) {
        frame_size = X86_LargePage;
    } else {
        frame_size = X86_SmallPage;
    }

    return
        cap_frame_cap_new(
            frame_size,                    /* capFSize           */
            ASID_LOW(asid),                /* capFMappedASIDLow  */
            vptr,                          /* capFMappedAddress  */
            map_type,                      /* capFMapType        */
            false,                         /* capFIsDevice       */
            ASID_HIGH(asid),               /* capFMappedASIDHigh */
            wordFromVMRights(VMReadWrite), /* capFVMRights       */
            pptr                           /* capFBasePtr        */
        );
}

BOOT_CODE cap_t
create_unmapped_it_frame_cap(pptr_t pptr, bool_t use_large)
{
    return create_it_frame_cap(pptr, 0, asidInvalid, use_large, X86_MAPPING_NONE);
}

BOOT_CODE cap_t
create_mapped_it_frame_cap(cap_t vspace_cap, pptr_t pptr, vptr_t vptr, asid_t asid, bool_t use_large, bool_t executable UNUSED)
{
    cap_t cap = create_it_frame_cap(pptr, vptr, asid, use_large, X86_MAPPING_VSPACE);
    map_it_frame_cap(vspace_cap, cap);
    return cap;
}

/* ==================== BOOT CODE FINISHES HERE ==================== */

pde_t CONST makeUserPDELargePage(paddr_t paddr, vm_attributes_t vm_attr, vm_rights_t vm_rights)
{
    return pde_pde_large_new(
               paddr,                                          /* page_base_address    */
               vm_attributes_get_x86PATBit(vm_attr),           /* pat                  */
               0,                                              /* avl                  */
               0,                                              /* global               */
               0,                                              /* dirty                */
               0,                                              /* accessed             */
               vm_attributes_get_x86PCDBit(vm_attr),           /* cache_disabled       */
               vm_attributes_get_x86PWTBit(vm_attr),           /* write_through        */
               SuperUserFromVMRights(vm_rights),               /* super_user           */
               WritableFromVMRights(vm_rights),                /* read_write           */
               1                                               /* present              */
           );
}

pde_t CONST makeUserPDEPageTable(paddr_t paddr, vm_attributes_t vm_attr)
{
    return pde_pde_small_new(
               paddr,                                      /* pt_base_address  */
               0,                                          /* avl              */
               0,                                          /* accessed         */
               vm_attributes_get_x86PCDBit(vm_attr),       /* cache_disabled   */
               vm_attributes_get_x86PWTBit(vm_attr),       /* write_through    */
               1,                                          /* super_user       */
               1,                                          /* read_write       */
               1                                           /* present          */
           );
}

pde_t CONST makeUserPDELargePageInvalid(void)
{
    return pde_pde_large_new(
               0,          /* page_base_address    */
               0,          /* pat                  */
               0,          /* avl                  */
               0,          /* global               */
               0,          /* dirty                */
               0,          /* accessed             */
               0,          /* cache_disabled       */
               0,          /* write_through        */
               0,          /* super_user           */
               0,          /* read_write           */
               0           /* present              */
           );
}

pde_t CONST makeUserPDEPageTableInvalid(void)
{
    return pde_pde_small_new(
               0,  /* pt_base_address  */
               0,  /* avl              */
               0,  /* accessed         */
               0,  /* cache_disabled   */
               0,  /* write_through    */
               0,  /* super_user       */
               0,  /* read_write       */
               0   /* present          */
           );
}

pte_t CONST makeUserPTE(paddr_t paddr, vm_attributes_t vm_attr, vm_rights_t vm_rights)
{
    return pte_new(
               paddr,                                          /* page_base_address    */
               0,                                              /* avl                  */
               0,                                              /* global               */
               vm_attributes_get_x86PATBit(vm_attr),           /* pat                  */
               0,                                              /* dirty                */
               0,                                              /* accessed             */
               vm_attributes_get_x86PCDBit(vm_attr),           /* cache_disabled       */
               vm_attributes_get_x86PWTBit(vm_attr),           /* write_through        */
               SuperUserFromVMRights(vm_rights),               /* super_user           */
               WritableFromVMRights(vm_rights),                /* read_write           */
               1                                               /* present              */
           );
}


pte_t CONST makeUserPTEInvalid(void)
{
    return pte_new(
               0,                   /* page_base_address    */
               0,                   /* avl                  */
               0,                   /* global               */
               0,                   /* pat                  */
               0,                   /* dirty                */
               0,                   /* accessed             */
               0,                   /* cache_disabled       */
               0,                   /* write_through        */
               0,                   /* super_user           */
               0,                   /* read_write           */
               0                    /* present              */
           );
}

void setVMRoot(tcb_t* tcb)
{
    cap_t               threadRoot;
    vspace_root_t *vspace_root;
    asid_t              asid;
    findVSpaceForASID_ret_t find_ret;

    threadRoot = TCB_PTR_CTE_PTR(tcb, tcbVTable)->cap;

    vspace_root = getValidNativeRoot(threadRoot);
    if (!vspace_root) {
        if (config_set(CONFIG_PAE_PAGING)) {
            setCurrentPD(pptr_to_paddr(ia32KSGlobalPDPT));
        } else {
            setCurrentPD(pptr_to_paddr(ia32KSGlobalPD));
        }
        return;
    }

    asid = cap_get_capMappedASID(threadRoot);
    find_ret = findVSpaceForASID(asid);
    if (find_ret.status != EXCEPTION_NONE || find_ret.vspace_root != vspace_root) {
        if (config_set(CONFIG_PAE_PAGING)) {
            setCurrentPD(pptr_to_paddr(ia32KSGlobalPDPT));
        } else {
            setCurrentPD(pptr_to_paddr(ia32KSGlobalPD));
        }
        return;
    }

    /* only set PD if we change it, otherwise we flush the TLB needlessly */
    if (getCurrentPD() != pptr_to_paddr(vspace_root)) {
        setCurrentPD(pptr_to_paddr(vspace_root));
    }
}

void hwASIDInvalidate(asid_t asid)
{
    /* 32-bit does not have PCID */
    return;
}

exception_t
decodeX86ModeMMUInvocation(
    word_t invLabel,
    word_t length,
    cptr_t cptr,
    cte_t* cte,
    cap_t cap,
    extra_caps_t excaps,
    word_t* buffer
)
{
    switch (cap_get_capType(cap)) {
    case cap_pdpt_cap:
        current_syscall_error.type = seL4_IllegalOperation;
        return EXCEPTION_SYSCALL_ERROR;

    case cap_page_directory_cap:
        return decodeIA32PageDirectoryInvocation(invLabel, length, cte, cap, excaps, buffer);

    default:
        fail("Invalid arch cap type");
    }
}


void modeUnmapPage(vm_page_size_t page_size, vspace_root_t *vroot, vptr_t vaddr, void *pptr)
{
    fail("Invalid page type");

}

exception_t decodeX86ModeMapRemapPage(word_t invLabel, vm_page_size_t page_size, cte_t *cte, cap_t cap, vspace_root_t *vroot, vptr_t vaddr, paddr_t paddr, vm_rights_t vm_rights, vm_attributes_t vm_attr)
{
    fail("Invalid Page type");
}
