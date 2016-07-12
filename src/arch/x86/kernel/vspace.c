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
#include <arch/api/invocation.h>
#include <arch/object/vtx.h>


static inline bool_t
checkVPAlignment(vm_page_size_t sz, word_t w)
{
    return IS_ALIGNED(w, pageBitsForSize(sz));
}


static exception_t
performPageGetAddress(void *vbase_ptr)
{
    paddr_t capFBasePtr;

    /* Get the physical address of this frame. */
    capFBasePtr = pptr_to_paddr(vbase_ptr);

    /* return it in the first message register */
    setRegister(ksCurThread, msgRegisters[0], capFBasePtr);
    setRegister(ksCurThread, msgInfoRegister,
                wordFromMessageInfo(seL4_MessageInfo_new(0, 0, 0, 1)));

    return EXCEPTION_NONE;
}


void deleteASIDPool(asid_t asid_base, asid_pool_t* pool)
{
    /* Haskell error: "ASID pool's base must be aligned" */
    assert(IS_ALIGNED(asid_base, asidLowBits));

    if (x86KSASIDTable[asid_base >> asidLowBits] == pool) {
        x86KSASIDTable[asid_base >> asidLowBits] = NULL;
        setVMRoot(ksCurThread);
    }
}

exception_t performASIDControlInvocation(void* frame, cte_t* slot, cte_t* parent, asid_t asid_base)
{
    cap_untyped_cap_ptr_set_capFreeIndex(&(parent->cap),
                                         MAX_FREE_INDEX(cap_untyped_cap_get_capBlockSize(parent->cap)));

    memzero(frame, 1 << pageBitsForSize(X86_SmallPage));
    cteInsert(
        cap_asid_pool_cap_new(
            asid_base,          /* capASIDBase  */
            WORD_REF(frame)     /* capASIDPool  */
        ),
        parent,
        slot
    );
    /* Haskell error: "ASID pool's base must be aligned" */
    assert((asid_base & MASK(asidLowBits)) == 0);
    x86KSASIDTable[asid_base >> asidLowBits] = (asid_pool_t*)frame;

    return EXCEPTION_NONE;
}

void deleteASID(asid_t asid, vspace_root_t *vspace)
{
    asid_pool_t* poolPtr;

    poolPtr = x86KSASIDTable[asid >> asidLowBits];
    hwASIDInvalidate(asid);
    if (poolPtr != NULL && poolPtr->array[asid & MASK(asidLowBits)] == vspace) {
        poolPtr->array[asid & MASK(asidLowBits)] = NULL;
        setVMRoot(ksCurThread);
    }
}

word_t* PURE lookupIPCBuffer(bool_t isReceiver, tcb_t *thread)
{
    word_t      w_bufferPtr;
    cap_t       bufferCap;
    vm_rights_t vm_rights;

    w_bufferPtr = thread->tcbIPCBuffer;
    bufferCap = TCB_PTR_CTE_PTR(thread, tcbBuffer)->cap;

    if (cap_get_capType(bufferCap) != cap_frame_cap) {
        return NULL;
    }

    vm_rights = cap_frame_cap_get_capFVMRights(bufferCap);
    if (vm_rights == VMReadWrite || (!isReceiver && vm_rights == VMReadOnly)) {
        word_t basePtr;
        unsigned int pageBits;

        basePtr = cap_frame_cap_get_capFBasePtr(bufferCap);
        pageBits = pageBitsForSize(cap_frame_cap_get_capFSize(bufferCap));
        return (word_t *)(basePtr + (w_bufferPtr & MASK(pageBits)));
    } else {
        return NULL;
    }
}

bool_t CONST isValidVTableRoot(cap_t cap)
{
    return isValidNativeRoot(cap);
}


BOOT_CODE bool_t map_kernel_window_devices(pte_t *pt, uint32_t num_ioapic, paddr_t* ioapic_paddrs, uint32_t num_drhu, paddr_t* drhu_list)
{
    word_t idx = (PPTR_KDEV & MASK(LARGE_PAGE_BITS)) >> PAGE_BITS;
    paddr_t phys;
    pte_t pte;
    unsigned int i;
    /* map kernel devices: APIC */
    phys = apic_get_base_paddr();
    if (!phys) {
        return false;
    }
    if (!add_allocated_p_region((p_region_t) {
    phys, phys + 0x1000
})) {
        return false;
    }
    pte = x86_make_device_pte(phys);

    assert(idx == (PPTR_APIC & MASK(LARGE_PAGE_BITS)) >> PAGE_BITS);
    pt[idx] = pte;
    idx++;
    for (i = 0; i < num_ioapic; i++) {
        phys = ioapic_paddrs[i];
        if (!add_allocated_p_region((p_region_t) {
        phys, phys + 0x1000
    })) {
            return false;
        }
        pte = x86_make_device_pte(phys);
        assert(idx == ( (PPTR_IOAPIC_START + i * BIT(PAGE_BITS)) & MASK(LARGE_PAGE_BITS)) >> PAGE_BITS);
        pt[idx] = pte;
        idx++;
        if (idx == BIT(PT_BITS)) {
            return false;
        }
    }
    /* put in null mappings for any extra IOAPICs */
    for (; i < CONFIG_MAX_NUM_IOAPIC; i++) {
        pte = x86_make_empty_pte();
        assert(idx == ( (PPTR_IOAPIC_START + i * BIT(PAGE_BITS)) & MASK(LARGE_PAGE_BITS)) >> PAGE_BITS);
        pt[idx] = pte;
        idx++;
    }

    /* map kernel devices: IOMMUs */
    for (i = 0; i < num_drhu; i++) {
        phys = (paddr_t)drhu_list[i];
        if (!add_allocated_p_region((p_region_t) {
        phys, phys + 0x1000
    })) {
            return false;
        }
        pte = x86_make_device_pte(phys);

        assert(idx == ((PPTR_DRHU_START + i * BIT(PAGE_BITS)) & MASK(LARGE_PAGE_BITS)) >> PAGE_BITS);
        pt[idx] = pte;
        idx++;
        if (idx == BIT(PT_BITS)) {
            return false;
        }
    }

    /* mark unused kernel-device pages as 'not present' */
    while (idx < BIT(PT_BITS)) {
        pte = x86_make_empty_pte();
        pt[idx] = pte;
        idx++;
    }

    /* Check we haven't added too many kernel-device mappings.*/
    assert(idx == BIT(PT_BITS));
    return true;
}

BOOT_CODE static void
init_idt(idt_entry_t* idt)
{
    init_idt_entry(idt, 0x00, int_00);
    init_idt_entry(idt, 0x01, int_01);
    init_idt_entry(idt, 0x02, int_02);
    init_idt_entry(idt, 0x03, int_03);
    init_idt_entry(idt, 0x04, int_04);
    init_idt_entry(idt, 0x05, int_05);
    init_idt_entry(idt, 0x06, int_06);
    init_idt_entry(idt, 0x07, int_07);
    init_idt_entry(idt, 0x08, int_08);
    init_idt_entry(idt, 0x09, int_09);
    init_idt_entry(idt, 0x0a, int_0a);
    init_idt_entry(idt, 0x0b, int_0b);
    init_idt_entry(idt, 0x0c, int_0c);
    init_idt_entry(idt, 0x0d, int_0d);
    init_idt_entry(idt, 0x0e, int_0e);
    init_idt_entry(idt, 0x0f, int_0f);

    init_idt_entry(idt, 0x10, int_10);
    init_idt_entry(idt, 0x11, int_11);
    init_idt_entry(idt, 0x12, int_12);
    init_idt_entry(idt, 0x13, int_13);
    init_idt_entry(idt, 0x14, int_14);
    init_idt_entry(idt, 0x15, int_15);
    init_idt_entry(idt, 0x16, int_16);
    init_idt_entry(idt, 0x17, int_17);
    init_idt_entry(idt, 0x18, int_18);
    init_idt_entry(idt, 0x19, int_19);
    init_idt_entry(idt, 0x1a, int_1a);
    init_idt_entry(idt, 0x1b, int_1b);
    init_idt_entry(idt, 0x1c, int_1c);
    init_idt_entry(idt, 0x1d, int_1d);
    init_idt_entry(idt, 0x1e, int_1e);
    init_idt_entry(idt, 0x1f, int_1f);

    init_idt_entry(idt, 0x20, int_20);
    init_idt_entry(idt, 0x21, int_21);
    init_idt_entry(idt, 0x22, int_22);
    init_idt_entry(idt, 0x23, int_23);
    init_idt_entry(idt, 0x24, int_24);
    init_idt_entry(idt, 0x25, int_25);
    init_idt_entry(idt, 0x26, int_26);
    init_idt_entry(idt, 0x27, int_27);
    init_idt_entry(idt, 0x28, int_28);
    init_idt_entry(idt, 0x29, int_29);
    init_idt_entry(idt, 0x2a, int_2a);
    init_idt_entry(idt, 0x2b, int_2b);
    init_idt_entry(idt, 0x2c, int_2c);
    init_idt_entry(idt, 0x2d, int_2d);
    init_idt_entry(idt, 0x2e, int_2e);
    init_idt_entry(idt, 0x2f, int_2f);

    init_idt_entry(idt, 0x30, int_30);
    init_idt_entry(idt, 0x31, int_31);
    init_idt_entry(idt, 0x32, int_32);
    init_idt_entry(idt, 0x33, int_33);
    init_idt_entry(idt, 0x34, int_34);
    init_idt_entry(idt, 0x35, int_35);
    init_idt_entry(idt, 0x36, int_36);
    init_idt_entry(idt, 0x37, int_37);
    init_idt_entry(idt, 0x38, int_38);
    init_idt_entry(idt, 0x39, int_39);
    init_idt_entry(idt, 0x3a, int_3a);
    init_idt_entry(idt, 0x3b, int_3b);
    init_idt_entry(idt, 0x3c, int_3c);
    init_idt_entry(idt, 0x3d, int_3d);
    init_idt_entry(idt, 0x3e, int_3e);
    init_idt_entry(idt, 0x3f, int_3f);

    init_idt_entry(idt, 0x40, int_40);
    init_idt_entry(idt, 0x41, int_41);
    init_idt_entry(idt, 0x42, int_42);
    init_idt_entry(idt, 0x43, int_43);
    init_idt_entry(idt, 0x44, int_44);
    init_idt_entry(idt, 0x45, int_45);
    init_idt_entry(idt, 0x46, int_46);
    init_idt_entry(idt, 0x47, int_47);
    init_idt_entry(idt, 0x48, int_48);
    init_idt_entry(idt, 0x49, int_49);
    init_idt_entry(idt, 0x4a, int_4a);
    init_idt_entry(idt, 0x4b, int_4b);
    init_idt_entry(idt, 0x4c, int_4c);
    init_idt_entry(idt, 0x4d, int_4d);
    init_idt_entry(idt, 0x4e, int_4e);
    init_idt_entry(idt, 0x4f, int_4f);

    init_idt_entry(idt, 0x50, int_50);
    init_idt_entry(idt, 0x51, int_51);
    init_idt_entry(idt, 0x52, int_52);
    init_idt_entry(idt, 0x53, int_53);
    init_idt_entry(idt, 0x54, int_54);
    init_idt_entry(idt, 0x55, int_55);
    init_idt_entry(idt, 0x56, int_56);
    init_idt_entry(idt, 0x57, int_57);
    init_idt_entry(idt, 0x58, int_58);
    init_idt_entry(idt, 0x59, int_59);
    init_idt_entry(idt, 0x5a, int_5a);
    init_idt_entry(idt, 0x5b, int_5b);
    init_idt_entry(idt, 0x5c, int_5c);
    init_idt_entry(idt, 0x5d, int_5d);
    init_idt_entry(idt, 0x5e, int_5e);
    init_idt_entry(idt, 0x5f, int_5f);

    init_idt_entry(idt, 0x60, int_60);
    init_idt_entry(idt, 0x61, int_61);
    init_idt_entry(idt, 0x62, int_62);
    init_idt_entry(idt, 0x63, int_63);
    init_idt_entry(idt, 0x64, int_64);
    init_idt_entry(idt, 0x65, int_65);
    init_idt_entry(idt, 0x66, int_66);
    init_idt_entry(idt, 0x67, int_67);
    init_idt_entry(idt, 0x68, int_68);
    init_idt_entry(idt, 0x69, int_69);
    init_idt_entry(idt, 0x6a, int_6a);
    init_idt_entry(idt, 0x6b, int_6b);
    init_idt_entry(idt, 0x6c, int_6c);
    init_idt_entry(idt, 0x6d, int_6d);
    init_idt_entry(idt, 0x6e, int_6e);
    init_idt_entry(idt, 0x6f, int_6f);

    init_idt_entry(idt, 0x70, int_70);
    init_idt_entry(idt, 0x71, int_71);
    init_idt_entry(idt, 0x72, int_72);
    init_idt_entry(idt, 0x73, int_73);
    init_idt_entry(idt, 0x74, int_74);
    init_idt_entry(idt, 0x75, int_75);
    init_idt_entry(idt, 0x76, int_76);
    init_idt_entry(idt, 0x77, int_77);
    init_idt_entry(idt, 0x78, int_78);
    init_idt_entry(idt, 0x79, int_79);
    init_idt_entry(idt, 0x7a, int_7a);
    init_idt_entry(idt, 0x7b, int_7b);
    init_idt_entry(idt, 0x7c, int_7c);
    init_idt_entry(idt, 0x7d, int_7d);
    init_idt_entry(idt, 0x7e, int_7e);
    init_idt_entry(idt, 0x7f, int_7f);

    init_idt_entry(idt, 0x80, int_80);
    init_idt_entry(idt, 0x81, int_81);
    init_idt_entry(idt, 0x82, int_82);
    init_idt_entry(idt, 0x83, int_83);
    init_idt_entry(idt, 0x84, int_84);
    init_idt_entry(idt, 0x85, int_85);
    init_idt_entry(idt, 0x86, int_86);
    init_idt_entry(idt, 0x87, int_87);
    init_idt_entry(idt, 0x88, int_88);
    init_idt_entry(idt, 0x89, int_89);
    init_idt_entry(idt, 0x8a, int_8a);
    init_idt_entry(idt, 0x8b, int_8b);
    init_idt_entry(idt, 0x8c, int_8c);
    init_idt_entry(idt, 0x8d, int_8d);
    init_idt_entry(idt, 0x8e, int_8e);
    init_idt_entry(idt, 0x8f, int_8f);

    init_idt_entry(idt, 0x90, int_90);
    init_idt_entry(idt, 0x91, int_91);
    init_idt_entry(idt, 0x92, int_92);
    init_idt_entry(idt, 0x93, int_93);
    init_idt_entry(idt, 0x94, int_94);
    init_idt_entry(idt, 0x95, int_95);
    init_idt_entry(idt, 0x96, int_96);
    init_idt_entry(idt, 0x97, int_97);
    init_idt_entry(idt, 0x98, int_98);
    init_idt_entry(idt, 0x99, int_99);
    init_idt_entry(idt, 0x9a, int_9a);
    init_idt_entry(idt, 0x9b, int_9b);
    init_idt_entry(idt, 0x9c, int_9c);
    init_idt_entry(idt, 0x9d, int_9d);
    init_idt_entry(idt, 0x9e, int_9e);
    init_idt_entry(idt, 0x9f, int_9f);

    init_idt_entry(idt, 0xa0, int_a0);
    init_idt_entry(idt, 0xa1, int_a1);
    init_idt_entry(idt, 0xa2, int_a2);
    init_idt_entry(idt, 0xa3, int_a3);
    init_idt_entry(idt, 0xa4, int_a4);
    init_idt_entry(idt, 0xa5, int_a5);
    init_idt_entry(idt, 0xa6, int_a6);
    init_idt_entry(idt, 0xa7, int_a7);
    init_idt_entry(idt, 0xa8, int_a8);
    init_idt_entry(idt, 0xa9, int_a9);
    init_idt_entry(idt, 0xaa, int_aa);
    init_idt_entry(idt, 0xab, int_ab);
    init_idt_entry(idt, 0xac, int_ac);
    init_idt_entry(idt, 0xad, int_ad);
    init_idt_entry(idt, 0xae, int_ae);
    init_idt_entry(idt, 0xaf, int_af);

    init_idt_entry(idt, 0xb0, int_b0);
    init_idt_entry(idt, 0xb1, int_b1);
    init_idt_entry(idt, 0xb2, int_b2);
    init_idt_entry(idt, 0xb3, int_b3);
    init_idt_entry(idt, 0xb4, int_b4);
    init_idt_entry(idt, 0xb5, int_b5);
    init_idt_entry(idt, 0xb6, int_b6);
    init_idt_entry(idt, 0xb7, int_b7);
    init_idt_entry(idt, 0xb8, int_b8);
    init_idt_entry(idt, 0xb9, int_b9);
    init_idt_entry(idt, 0xba, int_ba);
    init_idt_entry(idt, 0xbb, int_bb);
    init_idt_entry(idt, 0xbc, int_bc);
    init_idt_entry(idt, 0xbd, int_bd);
    init_idt_entry(idt, 0xbe, int_be);
    init_idt_entry(idt, 0xbf, int_bf);

    init_idt_entry(idt, 0xc0, int_c0);
    init_idt_entry(idt, 0xc1, int_c1);
    init_idt_entry(idt, 0xc2, int_c2);
    init_idt_entry(idt, 0xc3, int_c3);
    init_idt_entry(idt, 0xc4, int_c4);
    init_idt_entry(idt, 0xc5, int_c5);
    init_idt_entry(idt, 0xc6, int_c6);
    init_idt_entry(idt, 0xc7, int_c7);
    init_idt_entry(idt, 0xc8, int_c8);
    init_idt_entry(idt, 0xc9, int_c9);
    init_idt_entry(idt, 0xca, int_ca);
    init_idt_entry(idt, 0xcb, int_cb);
    init_idt_entry(idt, 0xcc, int_cc);
    init_idt_entry(idt, 0xcd, int_cd);
    init_idt_entry(idt, 0xce, int_ce);
    init_idt_entry(idt, 0xcf, int_cf);

    init_idt_entry(idt, 0xd0, int_d0);
    init_idt_entry(idt, 0xd1, int_d1);
    init_idt_entry(idt, 0xd2, int_d2);
    init_idt_entry(idt, 0xd3, int_d3);
    init_idt_entry(idt, 0xd4, int_d4);
    init_idt_entry(idt, 0xd5, int_d5);
    init_idt_entry(idt, 0xd6, int_d6);
    init_idt_entry(idt, 0xd7, int_d7);
    init_idt_entry(idt, 0xd8, int_d8);
    init_idt_entry(idt, 0xd9, int_d9);
    init_idt_entry(idt, 0xda, int_da);
    init_idt_entry(idt, 0xdb, int_db);
    init_idt_entry(idt, 0xdc, int_dc);
    init_idt_entry(idt, 0xdd, int_dd);
    init_idt_entry(idt, 0xde, int_de);
    init_idt_entry(idt, 0xdf, int_df);

    init_idt_entry(idt, 0xe0, int_e0);
    init_idt_entry(idt, 0xe1, int_e1);
    init_idt_entry(idt, 0xe2, int_e2);
    init_idt_entry(idt, 0xe3, int_e3);
    init_idt_entry(idt, 0xe4, int_e4);
    init_idt_entry(idt, 0xe5, int_e5);
    init_idt_entry(idt, 0xe6, int_e6);
    init_idt_entry(idt, 0xe7, int_e7);
    init_idt_entry(idt, 0xe8, int_e8);
    init_idt_entry(idt, 0xe9, int_e9);
    init_idt_entry(idt, 0xea, int_ea);
    init_idt_entry(idt, 0xeb, int_eb);
    init_idt_entry(idt, 0xec, int_ec);
    init_idt_entry(idt, 0xed, int_ed);
    init_idt_entry(idt, 0xee, int_ee);
    init_idt_entry(idt, 0xef, int_ef);

    init_idt_entry(idt, 0xf0, int_f0);
    init_idt_entry(idt, 0xf1, int_f1);
    init_idt_entry(idt, 0xf2, int_f2);
    init_idt_entry(idt, 0xf3, int_f3);
    init_idt_entry(idt, 0xf4, int_f4);
    init_idt_entry(idt, 0xf5, int_f5);
    init_idt_entry(idt, 0xf6, int_f6);
    init_idt_entry(idt, 0xf7, int_f7);
    init_idt_entry(idt, 0xf8, int_f8);
    init_idt_entry(idt, 0xf9, int_f9);
    init_idt_entry(idt, 0xfa, int_fa);
    init_idt_entry(idt, 0xfb, int_fb);
    init_idt_entry(idt, 0xfc, int_fc);
    init_idt_entry(idt, 0xfd, int_fd);
    init_idt_entry(idt, 0xfe, int_fe);
    init_idt_entry(idt, 0xff, int_ff);
}

BOOT_CODE bool_t
init_vm_state(void)
{
    x86KScacheLineSizeBits = getCacheLineSizeBits();
    if (!x86KScacheLineSizeBits) {
        return false;
    }
    init_tss(&x86KStss.tss);
    init_gdt(x86KSgdt, &x86KStss.tss);
    init_idt(x86KSidt);
    return true;
}

BOOT_CODE bool_t
init_pat_msr(void)
{
    x86_pat_msr_t pat_msr;
    /* First verify PAT is supported by the machine.
     *      See section 11.12.1 of Volume 3 of the Intel manual */
    if ( (x86_cpuid_edx(0x1, 0x0) & BIT(16)) == 0) {
        printf("PAT support not found\n");
        return false;
    }
    pat_msr.words[0] = x86_rdmsr_low(IA32_PAT_MSR);
    pat_msr.words[1] = x86_rdmsr_high(IA32_PAT_MSR);
    /* Set up the PAT MSR to the Intel defaults, just in case
     * they have been changed but a bootloader somewhere along the way */
    x86_pat_msr_ptr_set_pa0(&pat_msr, IA32_PAT_MT_WRITE_BACK);
    x86_pat_msr_ptr_set_pa1(&pat_msr, IA32_PAT_MT_WRITE_THROUGH);
    x86_pat_msr_ptr_set_pa2(&pat_msr, IA32_PAT_MT_UNCACHED);
    x86_pat_msr_ptr_set_pa3(&pat_msr, IA32_PAT_MT_UNCACHEABLE);
    /* Add the WriteCombining cache type to the PAT */
    x86_pat_msr_ptr_set_pa4(&pat_msr, IA32_PAT_MT_WRITE_COMBINING);
    x86_wrmsr(IA32_PAT_MSR, ((uint64_t)pat_msr.words[1]) << 32 | pat_msr.words[0]);
    return true;
}

BOOT_CODE void
write_it_asid_pool(cap_t it_ap_cap, cap_t it_vspace_cap)
{
    asid_pool_t* ap = ASID_POOL_PTR(pptr_of_cap(it_ap_cap));
    ap->array[IT_ASID] = PDE_PTR(pptr_of_cap(it_vspace_cap));
    x86KSASIDTable[IT_ASID >> asidLowBits] = ap;
}

findVSpaceForASID_ret_t findVSpaceForASID(asid_t asid)
{
    findVSpaceForASID_ret_t ret;
    asid_pool_t*        poolPtr;
    vspace_root_t *     vspace_root;

    poolPtr = x86KSASIDTable[asid >> asidLowBits];
    if (!poolPtr) {
        current_lookup_fault = lookup_fault_invalid_root_new();

        ret.vspace_root = NULL;
        ret.status = EXCEPTION_LOOKUP_FAULT;
        return ret;
    }

    vspace_root = poolPtr->array[asid & MASK(asidLowBits)];
    if (!vspace_root) {
        current_lookup_fault = lookup_fault_invalid_root_new();

        ret.vspace_root = NULL;
        ret.status = EXCEPTION_LOOKUP_FAULT;
        return ret;
    }

    ret.vspace_root = vspace_root;
    ret.status = EXCEPTION_NONE;
    return ret;
}

exception_t handleVMFault(tcb_t* thread, vm_fault_type_t vm_faultType)
{
    uint32_t addr;
    uint32_t fault;

    addr = getFaultAddr();
    fault = getRegister(thread, Error);

    switch (vm_faultType) {
    case X86DataFault:
        current_fault = fault_vm_fault_new(addr, fault, false);
        return EXCEPTION_FAULT;

    case X86InstructionFault:
        current_fault = fault_vm_fault_new(addr, fault, true);
        return EXCEPTION_FAULT;

    default:
        fail("Invalid VM fault type");
    }
}

uint32_t CONST WritableFromVMRights(vm_rights_t vm_rights)
{
    switch (vm_rights) {
    case VMReadOnly:
        return 0;

    case VMKernelOnly:
    case VMReadWrite:
        return 1;

    default:
        fail("Invalid VM rights");
    }
}

uint32_t CONST SuperUserFromVMRights(vm_rights_t vm_rights)
{
    switch (vm_rights) {
    case VMKernelOnly:
        return 0;

    case VMReadOnly:
    case VMReadWrite:
        return 1;

    default:
        fail("Invalid VM rights");
    }
}

lookupPTSlot_ret_t lookupPTSlot(vspace_root_t *vspace, vptr_t vptr)
{
    lookupPTSlot_ret_t ret;
    lookupPDSlot_ret_t pdSlot;

    pdSlot = lookupPDSlot(vspace, vptr);
    if (pdSlot.status != EXCEPTION_NONE) {
        ret.ptSlot = NULL;
        ret.status = pdSlot.status;
        return ret;
    }
    if ((pde_ptr_get_page_size(pdSlot.pdSlot) != pde_pde_small) ||
            !pde_pde_small_ptr_get_present(pdSlot.pdSlot)) {
        current_lookup_fault = lookup_fault_missing_capability_new(PAGE_BITS + PT_BITS);
        ret.ptSlot = NULL;
        ret.status = EXCEPTION_LOOKUP_FAULT;
        return ret;
    } else {
        pte_t* pt;
        pte_t* ptSlot;
        word_t ptIndex;

        pt = paddr_to_pptr(pde_pde_small_ptr_get_pt_base_address(pdSlot.pdSlot));
        ptIndex = (vptr >> PAGE_BITS) & MASK(PT_BITS);
        ptSlot = pt + ptIndex;

        ret.ptSlot = ptSlot;
        ret.status = EXCEPTION_NONE;
        return ret;
    }
}

exception_t checkValidIPCBuffer(vptr_t vptr, cap_t cap)
{
    if (cap_get_capType(cap) != cap_frame_cap) {
        userError("IPC Buffer is an invalid cap.");
        current_syscall_error.type = seL4_IllegalOperation;
        return EXCEPTION_SYSCALL_ERROR;
    }
    if (unlikely(cap_frame_cap_get_capFIsDevice(cap))) {
        userError("Specifying a device frame as an IPC buffer is not permitted.");
        current_syscall_error.type = seL4_IllegalOperation;
        return EXCEPTION_SYSCALL_ERROR;
    }

    if (!IS_ALIGNED(vptr, 9)) {
        userError("IPC Buffer vaddr 0x%x is not aligned.", (int)vptr);
        current_syscall_error.type = seL4_AlignmentError;
        return EXCEPTION_SYSCALL_ERROR;
    }

    return EXCEPTION_NONE;
}

vm_rights_t CONST maskVMRights(vm_rights_t vm_rights, cap_rights_t cap_rights_mask)
{
    if (vm_rights == VMReadOnly && cap_rights_get_capAllowRead(cap_rights_mask)) {
        return VMReadOnly;
    }
    if (vm_rights == VMReadWrite && cap_rights_get_capAllowRead(cap_rights_mask)) {
        if (!cap_rights_get_capAllowWrite(cap_rights_mask)) {
            return VMReadOnly;
        } else {
            return VMReadWrite;
        }
    }
    return VMKernelOnly;
}

void flushTable(vspace_root_t *vspace, word_t vptr, pte_t* pt, asid_t asid)
{
    word_t i;
    cap_t        threadRoot;

    assert(IS_ALIGNED(vptr, PT_BITS + PAGE_BITS));

    /* check if page table belongs to current address space */
    threadRoot = TCB_PTR_CTE_PTR(ksCurThread, tcbVTable)->cap;
    /* find valid mappings */
    for (i = 0; i < BIT(PT_BITS); i++) {
        if (pte_get_present(pt[i])) {
            if (config_set(CONFIG_SUPPORT_PCID) || (isValidNativeRoot(threadRoot) && (vspace_root_t*)pptr_of_cap(threadRoot) == vspace)) {
                invalidateTranslationSingleASID(vptr + (i << PAGE_BITS), asid);
            }
        }
    }
}


void unmapPage(vm_page_size_t page_size, asid_t asid, vptr_t vptr, void *pptr)
{
    findVSpaceForASID_ret_t find_ret;
    lookupPTSlot_ret_t  lu_ret;
    cap_t               threadRoot;
    lookupPDSlot_ret_t  pd_ret;
    pde_t               *pde;

    find_ret = findVSpaceForASID(asid);
    if (find_ret.status != EXCEPTION_NONE) {
        return;
    }

    switch (page_size) {
    case X86_SmallPage:
        lu_ret = lookupPTSlot(find_ret.vspace_root, vptr);
        if (lu_ret.status != EXCEPTION_NONE) {
            return;
        }
        if (! (pte_ptr_get_present(lu_ret.ptSlot)
                && (pte_ptr_get_page_base_address(lu_ret.ptSlot)
                    == pptr_to_paddr(pptr)))) {
            return;
        }
        *lu_ret.ptSlot = makeUserPTEInvalid();
        break;

    case X86_LargePage:
        pd_ret = lookupPDSlot(find_ret.vspace_root, vptr);
        if (pd_ret.status != EXCEPTION_NONE) {
            return;
        }
        pde = pd_ret.pdSlot;
        if (! (pde_ptr_get_page_size(pde) == pde_pde_large
                && pde_pde_large_ptr_get_present(pde)
                && (pde_pde_large_ptr_get_page_base_address(pde)
                    == pptr_to_paddr(pptr)))) {
            return;
        }
        *pde = makeUserPDELargePageInvalid();
        break;

    default:
        modeUnmapPage(page_size, find_ret.vspace_root, vptr, pptr);
        break;
    }

    /* check if page belongs to current address space */
    threadRoot = TCB_PTR_CTE_PTR(ksCurThread, tcbVTable)->cap;
    if (config_set(CONFIG_SUPPORT_PCID) || (isValidNativeRoot(threadRoot) && (vspace_root_t*)pptr_of_cap(threadRoot) == find_ret.vspace_root)) {
        invalidateTranslationSingleASID(vptr, asid);
    }
}

void unmapPageTable(asid_t asid, vptr_t vaddr, pte_t* pt)
{
    findVSpaceForASID_ret_t find_ret;
    lookupPDSlot_ret_t    lu_ret;

    find_ret = findVSpaceForASID(asid);
    if (find_ret.status != EXCEPTION_NONE) {
        return;
    }

    lu_ret = lookupPDSlot(find_ret.vspace_root, vaddr);
    if (lu_ret.status != EXCEPTION_NONE) {
        return;
    }

    /* check if the PD actually refers to the PT */
    if (! (pde_ptr_get_page_size(lu_ret.pdSlot) == pde_pde_small &&
            pde_pde_small_ptr_get_present(lu_ret.pdSlot) &&
            (pde_pde_small_ptr_get_pt_base_address(lu_ret.pdSlot) == pptr_to_paddr(pt)))) {
        return;
    }

    flushTable(find_ret.vspace_root, vaddr, pt, asid);

    *lu_ret.pdSlot = makeUserPDEPageTableInvalid();

    invalidatePageStructureCacheASID(pptr_to_paddr(find_ret.vspace_root), asid);
}

static exception_t
performX86PageInvocationMapPTE(cap_t cap, cte_t *ctSlot, pte_t *ptSlot, pte_t pte, vspace_root_t *vspace)
{
    ctSlot->cap = cap;
    *ptSlot = pte;
    invalidatePageStructureCacheASID(pptr_to_paddr(vspace), cap_frame_cap_get_capFMappedASID(cap));
    return EXCEPTION_NONE;
}

static exception_t
performX86PageInvocationMapPDE(cap_t cap, cte_t *ctSlot, pde_t *pdSlot, pde_t pde, vspace_root_t *vspace)
{
    ctSlot->cap = cap;
    *pdSlot = pde;
    invalidatePageStructureCacheASID(pptr_to_paddr(vspace), cap_frame_cap_get_capFMappedASID(cap));
    return EXCEPTION_NONE;
}

static exception_t
performX86PageInvocationRemapPTE(pte_t *ptSlot, pte_t pte, asid_t asid, vspace_root_t *vspace)
{
    *ptSlot = pte;
    invalidatePageStructureCacheASID(pptr_to_paddr(vspace), asid);
    return EXCEPTION_NONE;
}

static exception_t
performX86PageInvocationRemapPDE(pde_t *pdSlot, pde_t pde, asid_t asid, vspace_root_t *vspace)
{
    *pdSlot = pde;
    invalidatePageStructureCacheASID(pptr_to_paddr(vspace), asid);
    return EXCEPTION_NONE;
}

static exception_t
performX86PageInvocationUnmap(cap_t cap, cte_t *ctSlot)
{
    unmapPage(
        cap_frame_cap_get_capFSize(cap),
        cap_frame_cap_get_capFMappedASID(cap),
        cap_frame_cap_get_capFMappedAddress(cap),
        (void *)cap_frame_cap_get_capFBasePtr(cap)
    );

    cap_frame_cap_ptr_set_capFMappedAddress(&ctSlot->cap, 0);
    cap_frame_cap_ptr_set_capFMappedASID(&ctSlot->cap, asidInvalid);
    cap_frame_cap_ptr_set_capFMapType(&ctSlot->cap, X86_MAPPING_NONE);

    return EXCEPTION_NONE;
}

exception_t decodeX86FrameInvocation(
    word_t invLabel,
    word_t length,
    cte_t* cte,
    cap_t cap,
    extra_caps_t excaps,
    word_t* buffer
)
{
    switch (invLabel) {
    case X86PageMap: { /* Map */
        word_t          vaddr;
        word_t          vtop;
        word_t          w_rightsMask;
        paddr_t         paddr;
        cap_t           vspaceCap;
        vspace_root_t*  vspace;
        vm_rights_t     capVMRights;
        vm_rights_t     vmRights;
        vm_attributes_t vmAttr;
        vm_page_size_t  frameSize;
        asid_t          asid;

        if (length < 3 || excaps.excaprefs[0] == NULL) {
            current_syscall_error.type = seL4_TruncatedMessage;

            return EXCEPTION_SYSCALL_ERROR;
        }

        frameSize = cap_frame_cap_get_capFSize(cap);
        vaddr = getSyscallArg(0, buffer);
        w_rightsMask = getSyscallArg(1, buffer);
        vmAttr = vmAttributesFromWord(getSyscallArg(2, buffer));
        vspaceCap = excaps.excaprefs[0]->cap;

        capVMRights = cap_frame_cap_get_capFVMRights(cap);

        if (cap_frame_cap_get_capFMappedASID(cap) != asidInvalid) {
            userError("X86Frame: Frame already mapped.");
            current_syscall_error.type = seL4_InvalidCapability;
            current_syscall_error.invalidCapNumber = 0;

            return EXCEPTION_SYSCALL_ERROR;
        }

        assert(cap_frame_cap_get_capFMapType(cap) == X86_MAPPING_NONE);

        if (!isValidNativeRoot(vspaceCap)) {
            userError("X86Frame: Attempting to map frame into invalid page directory cap.");
            current_syscall_error.type = seL4_InvalidCapability;
            current_syscall_error.invalidCapNumber = 1;

            return EXCEPTION_SYSCALL_ERROR;
        }
        vspace = (vspace_root_t*)pptr_of_cap(vspaceCap);
        asid = cap_get_capMappedASID(vspaceCap);

        {
            findVSpaceForASID_ret_t find_ret;

            find_ret = findVSpaceForASID(asid);
            if (find_ret.status != EXCEPTION_NONE) {
                current_syscall_error.type = seL4_FailedLookup;
                current_syscall_error.failedLookupWasSource = false;

                return EXCEPTION_SYSCALL_ERROR;
            }

            if (find_ret.vspace_root != vspace) {
                current_syscall_error.type = seL4_InvalidCapability;
                current_syscall_error.invalidCapNumber = 1;

                return EXCEPTION_SYSCALL_ERROR;
            }
        }

        vtop = vaddr + BIT(pageBitsForSize(frameSize));

        if (vtop > PPTR_USER_TOP) {
            userError("X86Frame: Mapping address too high.");
            current_syscall_error.type = seL4_InvalidArgument;
            current_syscall_error.invalidArgumentNumber = 0;

            return EXCEPTION_SYSCALL_ERROR;
        }

        vmRights = maskVMRights(capVMRights, rightsFromWord(w_rightsMask));

        if (!checkVPAlignment(frameSize, vaddr)) {
            current_syscall_error.type = seL4_AlignmentError;

            return EXCEPTION_SYSCALL_ERROR;
        }

        paddr = pptr_to_paddr((void*)cap_frame_cap_get_capFBasePtr(cap));

        cap = cap_frame_cap_set_capFMappedASID(cap, asid);
        cap = cap_frame_cap_set_capFMappedAddress(cap, vaddr);
        cap = cap_frame_cap_set_capFMapType(cap, X86_MAPPING_VSPACE);

        switch (frameSize) {
            /* PTE mappings */
        case X86_SmallPage: {
            pte_t              pte;
            lookupPTSlot_ret_t lu_ret;

            lu_ret = lookupPTSlot(vspace, vaddr);
            if (lu_ret.status != EXCEPTION_NONE) {
                current_syscall_error.type = seL4_FailedLookup;
                current_syscall_error.failedLookupWasSource = false;
                /* current_lookup_fault will have been set by lookupPTSlot */
                return EXCEPTION_SYSCALL_ERROR;
            }

            if (pte_ptr_get_present(lu_ret.ptSlot)) {
                current_syscall_error.type = seL4_DeleteFirst;
                return EXCEPTION_SYSCALL_ERROR;
            }

            pte = makeUserPTE(paddr, vmAttr, vmRights);
            setThreadState(ksCurThread, ThreadState_Restart);
            return performX86PageInvocationMapPTE(cap, cte, lu_ret.ptSlot, pte, vspace);
        }

        /* PDE mappings */
        case X86_LargePage: {
            pde_t* pdeSlot;
            pde_t  pde;
            lookupPDSlot_ret_t lu_ret;

            lu_ret = lookupPDSlot(vspace, vaddr);
            if (lu_ret.status != EXCEPTION_NONE) {
                current_syscall_error.type = seL4_FailedLookup;
                current_syscall_error.failedLookupWasSource = false;
                /* current_lookup_fault will have been set by lookupPDSlot */
                return EXCEPTION_SYSCALL_ERROR;
            }
            pdeSlot = lu_ret.pdSlot;

            /* check for existing page table */
            if ((pde_ptr_get_page_size(pdeSlot) == pde_pde_small) &&
                    (pde_pde_small_ptr_get_present(pdeSlot))) {
                current_syscall_error.type = seL4_DeleteFirst;

                return EXCEPTION_SYSCALL_ERROR;
            }

            /* check for existing large page */
            if ((pde_ptr_get_page_size(pdeSlot) == pde_pde_large) &&
                    (pde_pde_large_ptr_get_present(pdeSlot))) {
                current_syscall_error.type = seL4_DeleteFirst;

                return EXCEPTION_SYSCALL_ERROR;
            }

            pde = makeUserPDELargePage(paddr, vmAttr, vmRights);
            setThreadState(ksCurThread, ThreadState_Restart);
            return performX86PageInvocationMapPDE(cap, cte, lu_ret.pdSlot, pde, vspace);
        }

        default: {
            return decodeX86ModeMapRemapPage(invLabel, frameSize, cte, cap, vspace, vaddr, paddr, vmRights, vmAttr);
        }
        }

        return EXCEPTION_SYSCALL_ERROR;
    }

    case X86PageRemap: { /* Remap */
        word_t          vaddr;
        word_t          w_rightsMask;
        paddr_t         paddr;
        cap_t           vspaceCap;
        vspace_root_t*  vspace;
        vm_rights_t     capVMRights;
        vm_rights_t     vmRights;
        vm_attributes_t vmAttr;
        vm_page_size_t  frameSize;
        asid_t          asid;

        if (cap_frame_cap_get_capFMapType(cap) != X86_MAPPING_VSPACE) {
            userError("X86FrameRemap: Attempting to remap frame with different mapping type");
            current_syscall_error.type = seL4_IllegalOperation;

            return EXCEPTION_SYSCALL_ERROR;
        }

        if (length < 2 || excaps.excaprefs[0] == NULL) {
            userError("X86FrameRemap: Truncated message");
            current_syscall_error.type = seL4_TruncatedMessage;

            return EXCEPTION_SYSCALL_ERROR;
        }

        w_rightsMask = getSyscallArg(0, buffer);
        vmAttr = vmAttributesFromWord(getSyscallArg(1, buffer));
        vspaceCap = excaps.excaprefs[0]->cap;

        if (!isValidNativeRoot(vspaceCap)) {
            userError("X86FrameRemap: Attempting to map frame into invalid page directory.");
            current_syscall_error.type = seL4_InvalidCapability;
            current_syscall_error.invalidCapNumber = 1;

            return EXCEPTION_SYSCALL_ERROR;
        }
        vspace = (vspace_root_t*)pptr_of_cap(vspaceCap);
        asid = cap_get_capMappedASID(vspaceCap);

        if (cap_frame_cap_get_capFMappedASID(cap) == asidInvalid) {
            userError("X86PageRemap: Frame must already have been mapped.");
            current_syscall_error.type = seL4_InvalidCapability;
            current_syscall_error.invalidCapNumber = 0;

            return EXCEPTION_SYSCALL_ERROR;
        }

        {
            findVSpaceForASID_ret_t find_ret;

            find_ret = findVSpaceForASID(asid);
            if (find_ret.status != EXCEPTION_NONE) {
                current_syscall_error.type = seL4_FailedLookup;
                current_syscall_error.failedLookupWasSource = false;

                return EXCEPTION_SYSCALL_ERROR;
            }

            if (find_ret.vspace_root != vspace) {
                current_syscall_error.type = seL4_InvalidCapability;
                current_syscall_error.invalidCapNumber = 1;

                return EXCEPTION_SYSCALL_ERROR;
            }
        }

        vaddr       = cap_frame_cap_get_capFMappedAddress(cap);
        frameSize   = cap_frame_cap_get_capFSize(cap);
        capVMRights = cap_frame_cap_get_capFVMRights(cap);
        paddr       = pptr_to_paddr((void*)cap_frame_cap_get_capFBasePtr(cap));

        vmRights = maskVMRights(capVMRights, rightsFromWord(w_rightsMask));

        switch (frameSize) {
            /* PTE mappings */
        case X86_SmallPage: {
            pte_t              pte;
            lookupPTSlot_ret_t lu_ret;

            lu_ret = lookupPTSlot(vspace, vaddr);
            if (lu_ret.status != EXCEPTION_NONE) {
                current_syscall_error.type = seL4_FailedLookup;
                current_syscall_error.failedLookupWasSource = false;
                /* current_lookup_fault will have been set by lookupPTSlot */
                return EXCEPTION_SYSCALL_ERROR;
            }

            pte = makeUserPTE(paddr, vmAttr, vmRights);

            setThreadState(ksCurThread, ThreadState_Restart);
            return performX86PageInvocationRemapPTE(lu_ret.ptSlot, pte, asid, vspace);

        }

        /* PDE mappings */
        case X86_LargePage: {
            pde_t* pdeSlot;
            pde_t  pde;
            lookupPDSlot_ret_t lu_ret;

            lu_ret = lookupPDSlot(vspace, vaddr);
            if (lu_ret.status != EXCEPTION_NONE) {
                current_syscall_error.type = seL4_FailedLookup;
                current_syscall_error.failedLookupWasSource = false;
                /* current_lookup_fault will have been set by lookupPDSlot */
                return EXCEPTION_SYSCALL_ERROR;
            }
            pdeSlot = lu_ret.pdSlot;

            if ((pde_ptr_get_page_size(pdeSlot) == pde_pde_small) &&
                    (pde_pde_small_ptr_get_present(pdeSlot))) {
                current_syscall_error.type = seL4_DeleteFirst;

                return EXCEPTION_SYSCALL_ERROR;
            }

            pde = makeUserPDELargePage(paddr, vmAttr, vmRights);

            setThreadState(ksCurThread, ThreadState_Restart);
            return performX86PageInvocationRemapPDE(pdeSlot, pde, asid, vspace);
        }

        default: {
            return decodeX86ModeMapRemapPage(invLabel, frameSize, cte, cap, vspace, vaddr, paddr, vmRights, vmAttr);
        }
        }

        return EXCEPTION_SYSCALL_ERROR;
    }

    case X86PageUnmap: { /* Unmap */
        if (cap_frame_cap_get_capFMappedASID(cap) != asidInvalid) {
            switch (cap_frame_cap_get_capFMapType(cap)) {
            case X86_MAPPING_VSPACE:
                setThreadState(ksCurThread, ThreadState_Restart);
                return performX86PageInvocationUnmap(cap, cte);
            case X86_MAPPING_IOSPACE:
                return decodeX86IOUnMapInvocation(invLabel, length, cte, cap, excaps);
            /* todo EPT */
            case X86_MAPPING_NONE:
                fail("Mapped frame cap was not mapped");
                break;
            }
        }

        return EXCEPTION_NONE;
    }

    case X86PageMapIO: { /* MapIO */
        return decodeX86IOMapInvocation(invLabel, length, cte, cap, excaps, buffer);
    }

    case X86PageMapEPT:
        return decodeIA32EPTFrameMap(invLabel, length, cte, cap, excaps, buffer);

    case X86PageGetAddress: {
        /* Return it in the first message register. */
        assert(n_msgRegisters >= 1);

        setThreadState(ksCurThread, ThreadState_Restart);
        return performPageGetAddress((void*)cap_frame_cap_get_capFBasePtr(cap));
    }

    default:
        current_syscall_error.type = seL4_IllegalOperation;

        return EXCEPTION_SYSCALL_ERROR;
    }
}

static exception_t
performX86PageTableInvocationUnmap(cap_t cap, cte_t *ctSlot)
{

    if (cap_page_table_cap_get_capPTIsMapped(cap)) {
        pte_t *pt = PTE_PTR(cap_page_table_cap_get_capPTBasePtr(cap));
        unmapPageTable(
            cap_page_table_cap_get_capPTMappedASID(cap),
            cap_page_table_cap_get_capPTMappedAddress(cap),
            pt
        );
        clearMemory((void *)pt, cap_get_capSizeBits(cap));
    }
    cap_page_table_cap_ptr_set_capPTIsMapped(&(ctSlot->cap), 0);

    return EXCEPTION_NONE;
}

static exception_t
performX86PageTableInvocationMap(cap_t cap, cte_t *ctSlot, pde_t pde, pde_t *pdSlot, vspace_root_t *root)
{
    ctSlot->cap = cap;
    *pdSlot = pde;
    invalidatePageStructureCacheASID(pptr_to_paddr(root), cap_page_table_cap_get_capPTMappedASID(cap));
    return EXCEPTION_NONE;
}

static exception_t
decodeX86PageTableInvocation(
    word_t invLabel,
    word_t length,
    cte_t* cte, cap_t cap,
    extra_caps_t excaps,
    word_t* buffer
)
{
    word_t          vaddr;
    vm_attributes_t attr;
    lookupPDSlot_ret_t pdSlot;
    cap_t           vspaceCap;
    vspace_root_t*  vspace;
    pde_t           pde;
    paddr_t         paddr;
    asid_t          asid;

    if (invLabel == X86PageTableUnmap) {
        if (! isFinalCapability(cte)) {
            current_syscall_error.type = seL4_RevokeFirst;
            userError("X86PageTable: Cannot unmap if more than one cap exists.");
            return EXCEPTION_SYSCALL_ERROR;
        }
        setThreadState(ksCurThread, ThreadState_Restart);
        return performX86PageTableInvocationUnmap(cap, cte);
    }

    if (invLabel != X86PageTableMap ) {
        userError("X86PageTable: Illegal operation.");
        current_syscall_error.type = seL4_IllegalOperation;
        return EXCEPTION_SYSCALL_ERROR;
    }

    if (length < 2 || excaps.excaprefs[0] == NULL) {
        userError("X86PageTable: Truncated message.");
        current_syscall_error.type = seL4_TruncatedMessage;
        return EXCEPTION_SYSCALL_ERROR;
    }

    if (cap_page_table_cap_get_capPTIsMapped(cap)) {
        userError("X86PageTable: Page table is already mapped to a page directory.");
        current_syscall_error.type =
            seL4_InvalidCapability;
        current_syscall_error.invalidCapNumber = 0;

        return EXCEPTION_SYSCALL_ERROR;
    }

    vaddr = getSyscallArg(0, buffer) & (~MASK(PT_BITS + PAGE_BITS));
    attr = vmAttributesFromWord(getSyscallArg(1, buffer));
    vspaceCap = excaps.excaprefs[0]->cap;

    if (!isValidNativeRoot(vspaceCap)) {
        current_syscall_error.type = seL4_InvalidCapability;
        current_syscall_error.invalidCapNumber = 1;

        return EXCEPTION_SYSCALL_ERROR;
    }

    vspace = (vspace_root_t*)pptr_of_cap(vspaceCap);
    asid = cap_get_capMappedASID(vspaceCap);

    if (vaddr >= PPTR_USER_TOP) {
        userError("X86PageTable: Mapping address too high.");
        current_syscall_error.type = seL4_InvalidArgument;
        current_syscall_error.invalidArgumentNumber = 0;

        return EXCEPTION_SYSCALL_ERROR;
    }

    {
        findVSpaceForASID_ret_t find_ret;

        find_ret = findVSpaceForASID(asid);
        if (find_ret.status != EXCEPTION_NONE) {
            current_syscall_error.type = seL4_FailedLookup;
            current_syscall_error.failedLookupWasSource = false;

            return EXCEPTION_SYSCALL_ERROR;
        }

        if (find_ret.vspace_root != vspace) {
            current_syscall_error.type = seL4_InvalidCapability;
            current_syscall_error.invalidCapNumber = 1;

            return EXCEPTION_SYSCALL_ERROR;
        }
    }

    pdSlot = lookupPDSlot(vspace, vaddr);
    if (pdSlot.status != EXCEPTION_NONE) {
        current_syscall_error.type = seL4_FailedLookup;
        current_syscall_error.failedLookupWasSource = false;

        return EXCEPTION_SYSCALL_ERROR;
    }

    if (((pde_ptr_get_page_size(pdSlot.pdSlot) == pde_pde_small) && pde_pde_small_ptr_get_present(pdSlot.pdSlot)) ||
            ((pde_ptr_get_page_size(pdSlot.pdSlot) == pde_pde_large) && pde_pde_large_ptr_get_present(pdSlot.pdSlot))) {
        current_syscall_error.type = seL4_DeleteFirst;

        return EXCEPTION_SYSCALL_ERROR;
    }

    paddr = pptr_to_paddr(PTE_PTR(cap_page_table_cap_get_capPTBasePtr(cap)));
    pde = makeUserPDEPageTable(paddr, attr);

    cap = cap_page_table_cap_set_capPTIsMapped(cap, 1);
    cap = cap_page_table_cap_set_capPTMappedASID(cap, asid);
    cap = cap_page_table_cap_set_capPTMappedAddress(cap, vaddr);

    setThreadState(ksCurThread, ThreadState_Restart);
    return performX86PageTableInvocationMap(cap, cte, pde, pdSlot.pdSlot, vspace);
}

exception_t decodeX86MMUInvocation(
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

    case cap_frame_cap:
        return decodeX86FrameInvocation(invLabel, length, cte, cap, excaps, buffer);

    case cap_page_table_cap:
        return decodeX86PageTableInvocation(invLabel, length, cte, cap, excaps, buffer);

    case cap_asid_control_cap: {
        word_t     i;
        asid_t           asid_base;
        word_t           index;
        word_t           depth;
        cap_t            untyped;
        cap_t            root;
        cte_t*           parentSlot;
        cte_t*           destSlot;
        lookupSlot_ret_t lu_ret;
        void*            frame;
        exception_t      status;

        if (invLabel != X86ASIDControlMakePool) {
            current_syscall_error.type = seL4_IllegalOperation;

            return EXCEPTION_SYSCALL_ERROR;
        }

        if (length < 2 || excaps.excaprefs[0] == NULL
                || excaps.excaprefs[1] == NULL) {
            current_syscall_error.type = seL4_TruncatedMessage;
            return EXCEPTION_SYSCALL_ERROR;
        }

        index = getSyscallArg(0, buffer);
        depth = getSyscallArg(1, buffer);
        parentSlot = excaps.excaprefs[0];
        untyped = parentSlot->cap;
        root = excaps.excaprefs[1]->cap;

        /* Find first free pool */
        for (i = 0; i < nASIDPools && x86KSASIDTable[i]; i++);

        if (i == nASIDPools) {
            /* no unallocated pool is found */
            current_syscall_error.type = seL4_DeleteFirst;

            return EXCEPTION_SYSCALL_ERROR;
        }

        asid_base = i << asidLowBits;


        if (cap_get_capType(untyped) != cap_untyped_cap ||
                cap_untyped_cap_get_capBlockSize(untyped) != seL4_ASIDPoolBits ||
                cap_untyped_cap_get_capIsDevice(untyped)) {
            current_syscall_error.type = seL4_InvalidCapability;
            current_syscall_error.invalidCapNumber = 1;

            return EXCEPTION_SYSCALL_ERROR;
        }

        status = ensureNoChildren(parentSlot);
        if (status != EXCEPTION_NONE) {
            return status;
        }

        frame = WORD_PTR(cap_untyped_cap_get_capPtr(untyped));

        lu_ret = lookupTargetSlot(root, index, depth);
        if (lu_ret.status != EXCEPTION_NONE) {
            return lu_ret.status;
        }
        destSlot = lu_ret.slot;

        status = ensureEmptySlot(destSlot);
        if (status != EXCEPTION_NONE) {
            return status;
        }

        setThreadState(ksCurThread, ThreadState_Restart);
        return performASIDControlInvocation(frame, destSlot, parentSlot, asid_base);
    }

    case cap_asid_pool_cap: {
        cap_t        vspaceCap;
        cte_t*       vspaceCapSlot;
        asid_pool_t* pool;
        word_t i;
        asid_t       asid;

        if (invLabel != X86ASIDPoolAssign) {
            current_syscall_error.type = seL4_IllegalOperation;

            return EXCEPTION_SYSCALL_ERROR;
        }
        if (excaps.excaprefs[0] == NULL) {
            current_syscall_error.type = seL4_TruncatedMessage;

            return EXCEPTION_SYSCALL_ERROR;
        }

        vspaceCapSlot = excaps.excaprefs[0];
        vspaceCap = vspaceCapSlot->cap;

        if (!isVTableRoot(vspaceCap) ||
                cap_get_capMappedASID(vspaceCap) != asidInvalid) {
            userError("X86ASIDPool: Invalid vspace root.");
            current_syscall_error.type = seL4_InvalidCapability;
            current_syscall_error.invalidCapNumber = 1;

            return EXCEPTION_SYSCALL_ERROR;
        }

        pool = x86KSASIDTable[cap_asid_pool_cap_get_capASIDBase(cap) >> asidLowBits];
        if (!pool) {
            current_syscall_error.type = seL4_FailedLookup;
            current_syscall_error.failedLookupWasSource = false;
            current_lookup_fault = lookup_fault_invalid_root_new();
            return EXCEPTION_SYSCALL_ERROR;
        }

        if (pool != ASID_POOL_PTR(cap_asid_pool_cap_get_capASIDPool(cap))) {
            current_syscall_error.type = seL4_InvalidCapability;
            current_syscall_error.invalidCapNumber = 0;
            return EXCEPTION_SYSCALL_ERROR;
        }

        /* Find first free ASID */
        asid = cap_asid_pool_cap_get_capASIDBase(cap);
        for (i = 0; i < BIT(asidLowBits) && (asid + i == 0 || pool->array[i]); i++);

        if (i == BIT(asidLowBits)) {
            current_syscall_error.type = seL4_DeleteFirst;

            return EXCEPTION_SYSCALL_ERROR;
        }

        asid += i;

        setThreadState(ksCurThread, ThreadState_Restart);
        return performASIDPoolInvocation(asid, pool, vspaceCapSlot);
    }

    default:
        return decodeX86ModeMMUInvocation(invLabel, length, cptr, cte, cap, excaps, buffer);
    }
}

#ifdef CONFIG_VTX
struct lookupEPTPDSlot_ret {
    exception_t status;
    ept_pde_t*  pd;
    uint32_t    pdIndex;
};
typedef struct lookupEPTPDSlot_ret lookupEPTPDSlot_ret_t;

struct lookupEPTPTSlot_ret {
    exception_t status;
    ept_pte_t*  pt;
    uint32_t    ptIndex;
};
typedef struct lookupEPTPTSlot_ret lookupEPTPTSlot_ret_t;

static ept_pdpte_t* CONST lookupEPTPDPTSlot(ept_pdpte_t *pdpt, vptr_t vptr)
{
    unsigned int pdptIndex;

    pdptIndex = vptr >> (PAGE_BITS + EPT_PT_BITS + EPT_PD_BITS);
    return pdpt + pdptIndex;
}

static lookupEPTPDSlot_ret_t lookupEPTPDSlot(ept_pdpte_t* pdpt, vptr_t vptr)
{
    lookupEPTPDSlot_ret_t ret;
    ept_pdpte_t* pdptSlot;

    pdptSlot = lookupEPTPDPTSlot(pdpt, vptr);

    if (!ept_pdpte_ptr_get_read(pdptSlot)) {
        current_lookup_fault = lookup_fault_missing_capability_new(22);

        ret.pd = NULL;
        ret.pdIndex = 0;
        ret.status = EXCEPTION_LOOKUP_FAULT;
        return ret;
    } else {
        ret.pd = paddr_to_pptr(ept_pdpte_ptr_get_pd_base_address(pdptSlot));
        ret.pdIndex = (vptr >> (PAGE_BITS + EPT_PT_BITS)) & MASK(EPT_PD_BITS);

        ret.status = EXCEPTION_NONE;
        return ret;
    }
}

static lookupEPTPTSlot_ret_t lookupEPTPTSlot(ept_pdpte_t* pdpt, vptr_t vptr)
{
    lookupEPTPTSlot_ret_t ret;
    lookupEPTPDSlot_ret_t lu_ret;
    ept_pde_t *pdSlot;

    lu_ret = lookupEPTPDSlot(pdpt, vptr);
    if (lu_ret.status != EXCEPTION_NONE) {
        current_syscall_error.type = seL4_FailedLookup;
        current_syscall_error.failedLookupWasSource = false;
        /* current_lookup_fault will have been set by lookupEPTPDSlot */
        ret.pt = NULL;
        ret.ptIndex = 0;
        ret.status = EXCEPTION_LOOKUP_FAULT;
        return ret;
    }

    pdSlot = lu_ret.pd + lu_ret.pdIndex;

    if ((ept_pde_ptr_get_page_size(pdSlot) != ept_pde_ept_pde_4k) ||
            !ept_pde_ept_pde_4k_ptr_get_read(pdSlot)) {
        current_lookup_fault = lookup_fault_missing_capability_new(22);

        ret.pt = NULL;
        ret.ptIndex = 0;
        ret.status = EXCEPTION_LOOKUP_FAULT;
        return ret;
    } else {
        ret.pt = paddr_to_pptr(ept_pde_ept_pde_4k_ptr_get_pt_base_address(pdSlot));
        ret.ptIndex = (vptr >> (PAGE_BITS)) & MASK(EPT_PT_BITS);

        ret.status = EXCEPTION_NONE;
        return ret;
    }
}

ept_pdpte_t *lookupEPTPDPTFromPD(ept_pde_t *pd)
{
    cte_t *pd_cte;

    /* First query the cdt and find the cap */
#if 0
    pd_cte = cdtFindWithExtra(cap_ept_page_directory_cap_new(0, 0, EPT_PD_REF(pd)));
#endif
    assert(!"unimplemented");
    pd_cte = NULL;
    /* We will not be returned a slot if there was no 'extra' information (aka if it is not mapped) */
    if (!pd_cte) {
        return NULL;
    }

    /* Return the mapping information from the cap. */
    return EPT_PDPT_PTR(cap_ept_page_directory_cap_get_capPDMappedObject(pd_cte->cap));
}

static ept_pdpte_t *lookupEPTPDPTFromPT(ept_pte_t *pt)
{
    cte_t *pt_cte;
    cap_t pt_cap;
    ept_pde_t *pd;

    /* First query the cdt and find the cap */
#if 0
    pt_cte = cdtFindWithExtra(cap_ept_page_table_cap_new(0, 0, EPT_PT_REF(pt)));
#endif
    assert(!"not implemented");
    pt_cte = NULL;
    /* We will not be returned a slot if there was no 'extra' information (aka if it is not mapped) */
    if (!pt_cte) {
        return NULL;
    }

    /* Get any mapping information from the cap */
    pt_cap = pt_cte->cap;
    pd = EPT_PD_PTR(cap_ept_page_table_cap_get_capPTMappedObject(pt_cap));
    /* If we found it then it *should* have information */
    assert(pd);
    /* Now lookup the PDPT from the PD */
    return lookupEPTPDPTFromPD(pd);
}

void unmapEPTPD(ept_pdpte_t *pdpt, uint32_t index, ept_pde_t *pd)
{
    pdpt[index] = ept_pdpte_new(
                      0,  /* pd_base_address  */
                      0,  /* avl_cte_depth    */
                      0,  /* execute          */
                      0,  /* write            */
                      0   /* read             */
                  );
}

void unmapEPTPT(ept_pde_t *pd, uint32_t index, ept_pte_t *pt)
{
    pd[index] = ept_pde_ept_pde_4k_new(
                    0,  /* pt_base_address  */
                    0,  /* avl_cte_depth    */
                    0,  /* execute          */
                    0,  /* write            */
                    0   /* read             */
                );
}

void unmapAllEPTPD(ept_pdpte_t *pdpt)
{
    assert(!"not implemented");
#if 0
    uint32_t i;

    for (i = 0; i < BIT(EPT_PDPT_BITS); i++) {
        ept_pdpte_t *pdpte = pdpt + i;
        if (ept_pdpte_ptr_get_pd_base_address(pdpte)) {
            cap_t cap;
            cte_t *pdCte;

            ept_pde_t *pd = EPT_PD_PTR(paddr_to_pptr(ept_pdpte_ptr_get_pd_base_address(pdpte)));
            uint32_t depth = ept_pdpte_ptr_get_avl_cte_depth(pdpte);
            pdCte = cdtFindAtDepth(cap_ept_page_directory_cap_new(EPT_PDPT_REF(pdpt), i, EPT_PD_REF(pd)), depth);
            assert(pdCte);

            cap = pdCte->cap;
            cap = cap_ept_page_directory_cap_set_capPDMappedObject(cap, 0);
            cdtUpdate(pdCte, cap);
        }
    }
#endif
}

void unmapAllEPTPages(ept_pte_t *pt)
{
    assert(!"not implemented");
#if 0
    uint32_t i;

    for (i = 0; i < BIT(EPT_PT_BITS); i++) {
        ept_pte_t *pte = pt + i;
        if (ept_pte_ptr_get_page_base_address(pte)) {
            cap_t newCap;
            cte_t *frameCte;

            void *frame = paddr_to_pptr(ept_pte_ptr_get_page_base_address(pte));
            uint32_t depth = ept_pte_ptr_get_avl_cte_depth(pte);
            frameCte = cdtFindAtDepth(cap_frame_cap_new(IA32_SmallPage, EPT_PT_REF(pt), i, IA32_MAPPING_EPT, 0, (uint32_t)frame), depth);
            assert(frameCte);

            newCap = cap_frame_cap_set_capFMappedObject(frameCte->cap, 0);
            cdtUpdate(frameCte, newCap);
        }
    }
#endif
}

enum ept_cache_options {
    EPTUncacheable = 0,
    EPTWriteCombining = 1,
    EPTWriteThrough = 4,
    EPTWriteProtected = 5,
    EPTWriteBack = 6
};
typedef enum ept_cache_options ept_cache_options_t;

static ept_cache_options_t
eptCacheFromVmAttr(vm_attributes_t vmAttr)
{
    /* PAT cache options are 1-1 with ept_cache_options. But need to
       verify user has specified a sensible option */
    ept_cache_options_t option = vmAttr.words[0];
    if (option != EPTUncacheable ||
            option != EPTWriteCombining ||
            option != EPTWriteThrough ||
            option != EPTWriteBack) {
        /* No failure mode is supported here, vmAttr settings should be verified earlier */
        option = EPTWriteBack;
    }
    return option;
}

exception_t
decodeIA32EPTInvocation(
    word_t label,
    unsigned int length,
    cptr_t cptr,
    cte_t* cte,
    cap_t cap,
    extra_caps_t extraCaps,
    word_t* buffer
)
{
    switch (cap_get_capType(cap)) {
    case cap_ept_page_directory_pointer_table_cap:
        current_syscall_error.type = seL4_IllegalOperation;
        return EXCEPTION_SYSCALL_ERROR;
    case cap_ept_page_directory_cap:
        return decodeIA32EPTPageDirectoryInvocation(label, length, cte, cap, extraCaps, buffer);
    case cap_ept_page_table_cap:
        return decodeIA32EPTPageTableInvocation(label, length, cte, cap, extraCaps, buffer);
    default:
        fail("Invalid cap type");
    }
}

exception_t
decodeIA32EPTPageDirectoryInvocation(
    word_t label,
    unsigned int length,
    cte_t* cte,
    cap_t cap,
    extra_caps_t extraCaps,
    word_t* buffer
)
{
    word_t          vaddr;
    word_t          pdptIndex;
    cap_t           pdptCap;
    ept_pdpte_t*    pdpt;
    ept_pdpte_t*    pdptSlot;
    ept_pdpte_t     pdpte;
    paddr_t         paddr;

    (void)paddr;

    if (label == X86EPTPageDirectoryUnmap) {
        setThreadState(ksCurThread, ThreadState_Restart);

        pdpt = EPT_PDPT_PTR(cap_ept_page_directory_cap_get_capPDMappedObject(cap));
        if (pdpt) {
            ept_pde_t* pd = (ept_pde_t*)cap_ept_page_directory_cap_get_capPDBasePtr(cap);
            unmapEPTPD(pdpt, cap_ept_page_directory_cap_get_capPDMappedIndex(cap), pd);
            invept((void*)((uint32_t)pdpt - EPT_PDPT_OFFSET));
        }

        cap = cap_ept_page_directory_cap_set_capPDMappedObject(cap, 0);
#if 0
        cdtUpdate(cte, cap);
#endif
        assert(!"not implemented");

        return EXCEPTION_NONE;
    }

    if (label != X86EPTPageDirectoryMap) {
        userError("IA32EPTPageDirectory Illegal operation.");
        current_syscall_error.type = seL4_IllegalOperation;
        return EXCEPTION_SYSCALL_ERROR;
    }

    if (length < 2 || extraCaps.excaprefs[0] == NULL) {
        userError("IA32EPTPageDirectoryMap: Truncated message.");
        current_syscall_error.type = seL4_TruncatedMessage;
        return EXCEPTION_SYSCALL_ERROR;
    }

    if (cap_ept_page_directory_cap_get_capPDMappedObject(cap)) {
        userError("IA32EPTPageDirectoryMap: EPT Page directory is already mapped to an EPT page directory pointer table.");
        current_syscall_error.type =
            seL4_InvalidCapability;
        current_syscall_error.invalidCapNumber = 0;

        return EXCEPTION_SYSCALL_ERROR;
    }

    vaddr = getSyscallArg(0, buffer);
    pdptCap = extraCaps.excaprefs[0]->cap;

    if (cap_get_capType(pdptCap) != cap_ept_page_directory_pointer_table_cap) {
        userError("IA32EPTPageDirectoryMap: Not a valid EPT page directory pointer table.");
        current_syscall_error.type = seL4_InvalidCapability;
        current_syscall_error.invalidCapNumber = 1;

        return EXCEPTION_SYSCALL_ERROR;
    }

    pdpt = (ept_pdpte_t*)cap_ept_page_directory_pointer_table_cap_get_capPDPTBasePtr(pdptCap);

    if (vaddr >= PPTR_BASE) {
        userError("IA32EPTPageDirectoryMap: vaddr not in kernel window.");
        current_syscall_error.type = seL4_InvalidArgument;
        current_syscall_error.invalidArgumentNumber = 0;

        return EXCEPTION_SYSCALL_ERROR;
    }

    pdptIndex = vaddr >> (PAGE_BITS + EPT_PD_BITS + EPT_PT_BITS);
    pdptSlot = &pdpt[pdptIndex];

    if (ept_pdpte_ptr_get_read(pdptSlot)) {
        userError("IA32EPTPageDirectoryMap: Page directory already mapped here.");
        current_syscall_error.type = seL4_DeleteFirst;
        return EXCEPTION_SYSCALL_ERROR;
    }

    paddr = pptr_to_paddr((void*)(cap_ept_page_directory_cap_get_capPDBasePtr(cap)));
//#if 0
    pdpte = ept_pdpte_new(
                paddr,                                      /* pd_base_address  */
                0/*mdb_node_get_cdtDepth(cte->cteMDBNode)*/,     /* avl_cte_depth    */
                1,                                          /* execute          */
                1,                                          /* write            */
                1                                           /* read             */
            );
//#endif
//    assert(!"not implemented");

    cap = cap_ept_page_directory_cap_set_capPDMappedObject(cap, EPT_PDPT_REF(pdpt));
    cap = cap_ept_page_directory_cap_set_capPDMappedIndex(cap, pdptIndex);

//#if 0
//    cdtUpdate(cte, cap);
    cte->cap = cap;
//#endif
//    assert(!"not implemented");

    *pdptSlot = pdpte;
    invept((void*)((uint32_t)pdpt - EPT_PDPT_OFFSET));

    setThreadState(ksCurThread, ThreadState_Restart);
    return EXCEPTION_NONE;
}

exception_t
decodeIA32EPTPageTableInvocation(
    word_t label,
    unsigned int length,
    cte_t* cte,
    cap_t cap,
    extra_caps_t extraCaps,
    word_t* buffer
)
{
    word_t          vaddr;
    cap_t           pdptCap;
    ept_pdpte_t*    pdpt;
    ept_pde_t*      pd;
    unsigned int    pdIndex;
    ept_pde_t*      pdSlot;
    ept_pde_t       pde;
    paddr_t         paddr;
    lookupEPTPDSlot_ret_t lu_ret;
    (void)paddr;

    if (label == X86EPTPageTableUnmap) {
        setThreadState(ksCurThread, ThreadState_Restart);

        pd = EPT_PD_PTR(cap_ept_page_table_cap_get_capPTMappedObject(cap));
        if (pd) {
            ept_pte_t* pt = (ept_pte_t*)cap_ept_page_table_cap_get_capPTBasePtr(cap);
            unmapEPTPT(pd, cap_ept_page_table_cap_get_capPTMappedIndex(cap), pt);
            pdpt = lookupEPTPDPTFromPD(pd);
            if (pdpt) {
                invept((void*)((uint32_t)pdpt - EPT_PDPT_OFFSET));
            }
        }

        cap = cap_ept_page_table_cap_set_capPTMappedObject(cap, 0);
#if 0
        cdtUpdate(cte, cap);
#endif
        assert(!"not implemented");

        return EXCEPTION_NONE;
    }

    if (label != X86EPTPageTableMap) {
        userError("IA32EPTPageTable Illegal operation.");
        current_syscall_error.type = seL4_IllegalOperation;
        return EXCEPTION_SYSCALL_ERROR;
    }

    if (length < 2 || extraCaps.excaprefs[0] == NULL) {
        userError("IA32EPTPageTable: Truncated message.");
        current_syscall_error.type = seL4_TruncatedMessage;
        return EXCEPTION_SYSCALL_ERROR;
    }

    if (cap_ept_page_table_cap_get_capPTMappedObject(cap)) {
        userError("IA32EPTPageTable EPT Page table is already mapped to an EPT page directory.");
        current_syscall_error.type =
            seL4_InvalidCapability;
        current_syscall_error.invalidCapNumber = 0;

        return EXCEPTION_SYSCALL_ERROR;
    }

    vaddr = getSyscallArg(0, buffer);
    pdptCap = extraCaps.excaprefs[0]->cap;

    if (cap_get_capType(pdptCap) != cap_ept_page_directory_pointer_table_cap) {
        userError("IA32EPTPageTableMap: Not a valid EPT page directory pointer table.");
        userError("IA32ASIDPool: Invalid vspace root.");
        current_syscall_error.type = seL4_InvalidCapability;
        current_syscall_error.invalidCapNumber = 1;

        return EXCEPTION_SYSCALL_ERROR;
    }

    pdpt = (ept_pdpte_t*)(cap_ept_page_directory_pointer_table_cap_get_capPDPTBasePtr(pdptCap));

    if (vaddr >= PPTR_BASE) {
        userError("IA32EPTPageTableMap: vaddr not in kernel window.");
        current_syscall_error.type = seL4_InvalidArgument;
        current_syscall_error.invalidArgumentNumber = 0;

        return EXCEPTION_SYSCALL_ERROR;
    }

    lu_ret = lookupEPTPDSlot(pdpt, vaddr);
    if (lu_ret.status != EXCEPTION_NONE) {
        current_syscall_error.type = seL4_FailedLookup;
        current_syscall_error.failedLookupWasSource = false;
        /* current_lookup_fault will have been set by lookupPTSlot */
        return EXCEPTION_SYSCALL_ERROR;
    }

    pd = lu_ret.pd;
    pdIndex = lu_ret.pdIndex;
    pdSlot = pd + pdIndex;

    if (((ept_pde_ptr_get_page_size(pdSlot) == ept_pde_ept_pde_4k) &&
            ept_pde_ept_pde_4k_ptr_get_read(pdSlot)) ||
            ((ept_pde_ptr_get_page_size(pdSlot) == ept_pde_ept_pde_2m) &&
             ept_pde_ept_pde_2m_ptr_get_read(pdSlot))) {
        userError("IA32EPTPageTableMap: Page table already mapped here");
        current_syscall_error.type = seL4_DeleteFirst;
        return EXCEPTION_SYSCALL_ERROR;
    }

    paddr = pptr_to_paddr((void*)(cap_ept_page_table_cap_get_capPTBasePtr(cap)));
//#if 0
    pde = ept_pde_ept_pde_4k_new(
              paddr,                                      /* pt_base_address  */
              0/*mdb_node_get_cdtDepth(cte->cteMDBNode)*/,     /* avl_cte_depth    */
              1,                                          /* execute          */
              1,                                          /* write            */
              1                                           /* read             */
          );
//#endif
//    assert(!"not implemented");

    cap = cap_ept_page_table_cap_set_capPTMappedObject(cap, EPT_PD_REF(pd));
    cap = cap_ept_page_table_cap_set_capPTMappedIndex(cap, pdIndex);

//#if 0
//    cdtUpdate(cte, cap);
    cte->cap = cap;
//#endif
//    assert(!"not implemented");

    *pdSlot = pde;
    invept((void*)((uint32_t)pdpt - EPT_PDPT_OFFSET));

    setThreadState(ksCurThread, ThreadState_Restart);
    return EXCEPTION_NONE;
}

exception_t
decodeIA32EPTFrameMap(
    word_t invLabel,
    word_t length,
    cte_t* cte,
    cap_t cap,
    extra_caps_t excaps,
    word_t* buffer)
{
    word_t          vaddr;
    word_t          w_rightsMask;
    paddr_t         paddr;
    cap_t           pdptCap;
    ept_pdpte_t*    pdpt;
    vm_rights_t     capVMRights;
    vm_rights_t     vmRights;
    vm_attributes_t vmAttr;
    vm_page_size_t  frameSize;

    (void)paddr;
    (void)vmRights;
    (void)vmAttr;

    frameSize = cap_frame_cap_get_capFSize(cap);
    vaddr = getSyscallArg(0, buffer);
    w_rightsMask = getSyscallArg(1, buffer);
    vmAttr = vmAttributesFromWord(getSyscallArg(2, buffer));
    pdptCap = excaps.excaprefs[0]->cap;

    capVMRights = cap_frame_cap_get_capFVMRights(cap);

    if (cap_frame_cap_get_capFMappedASID(cap) != asidInvalid) {
        userError("X86EPTFrameMap: Frame already mapped.");
        current_syscall_error.type = seL4_InvalidCapability;
        current_syscall_error.invalidCapNumber = 0;

        return EXCEPTION_SYSCALL_ERROR;
    }

    assert(cap_frame_cap_get_capFMapType(cap) == X86_MAPPING_NONE);

    if (cap_get_capType(pdptCap) != cap_ept_page_directory_pointer_table_cap) {
        userError("X86EPTFrameMap: Attempting to map frame into invalid ept pdpt.");
        current_syscall_error.type = seL4_InvalidCapability;
        current_syscall_error.invalidCapNumber = 1;

        return EXCEPTION_SYSCALL_ERROR;
    }

    pdpt = (ept_pdpte_t*)(cap_ept_page_directory_pointer_table_cap_get_capPDPTBasePtr(pdptCap));

    vmRights = maskVMRights(capVMRights, rightsFromWord(w_rightsMask));

    if (!checkVPAlignment(frameSize, vaddr)) {
        current_syscall_error.type = seL4_AlignmentError;

        return EXCEPTION_SYSCALL_ERROR;
    }

    paddr = pptr_to_paddr((void*)cap_frame_cap_get_capFBasePtr(cap));

    switch (frameSize) {
        /* PTE mappings */
    case X86_SmallPage: {
        lookupEPTPTSlot_ret_t lu_ret;
        ept_pte_t *ptSlot;

        lu_ret = lookupEPTPTSlot(pdpt, vaddr);
        if (lu_ret.status != EXCEPTION_NONE) {
            current_syscall_error.type = seL4_FailedLookup;
            current_syscall_error.failedLookupWasSource = false;
            /* current_lookup_fault will have been set by lookupEPTPTSlot */
            return EXCEPTION_SYSCALL_ERROR;
        }

        ptSlot = lu_ret.pt + lu_ret.ptIndex;

        if (ept_pte_ptr_get_page_base_address(ptSlot) != 0) {
            userError("IA32EPTFrameMap: Mapping already present.");
            current_syscall_error.type = seL4_DeleteFirst;
            return EXCEPTION_SYSCALL_ERROR;
        }
        (void)eptCacheFromVmAttr;
//#if 0
        *ptSlot = ept_pte_new(
                      paddr,
                      0/*mdb_node_get_cdtDepth(cte->cteMDBNode)*/,
                      0,
                      eptCacheFromVmAttr(vmAttr),
                      1,
                      WritableFromVMRights(vmRights),
                      1);
//#endif
//        assert(!"not implemented");

        invept((void*)((uint32_t)pdpt - EPT_PDPT_OFFSET));

//#if 0
//        cap = cap_frame_cap_set_capFMappedObject(cap, EPT_PT_REF(lu_ret.pt));
//        cap = cap_frame_cap_set_capFMappedIndex(cap, lu_ret.ptIndex);
        cap = cap_frame_cap_set_capFMappedASID(cap, /*asid is bunk*/ 42);
        cap = cap_frame_cap_set_capFMappedAddress(cap, vaddr);
        cap = cap_frame_cap_set_capFMapType(cap, X86_MAPPING_EPT);

//        cdtUpdate(cte, cap);
        cte->cap = cap;
//#endif
//        assert(!"not implemented");

        setThreadState(ksCurThread, ThreadState_Restart);
        return EXCEPTION_NONE;
    }

    /* PDE mappings */
    case X86_LargePage: {
        lookupEPTPDSlot_ret_t lu_ret;
        ept_pde_t *pdSlot;

        lu_ret = lookupEPTPDSlot(pdpt, vaddr);
        if (lu_ret.status != EXCEPTION_NONE) {
            userError("IA32EPTFrameMap: Need a page directory first.");
            current_syscall_error.type = seL4_FailedLookup;
            current_syscall_error.failedLookupWasSource = false;
            /* current_lookup_fault will have been set by lookupEPTPDSlot */
            return EXCEPTION_SYSCALL_ERROR;
        }

        pdSlot = lu_ret.pd + lu_ret.pdIndex;

        if ((ept_pde_ptr_get_page_size(pdSlot) == ept_pde_ept_pde_4k) &&
                ept_pde_ept_pde_4k_ptr_get_read(pdSlot)) {
            userError("IA32EPTFrameMap: Page table already present.");
            current_syscall_error.type = seL4_DeleteFirst;
            return EXCEPTION_SYSCALL_ERROR;
        }
        if ((ept_pde_ptr_get_page_size(pdSlot + 1) == ept_pde_ept_pde_4k) &&
                ept_pde_ept_pde_4k_ptr_get_read(pdSlot + 1)) {
            userError("IA32EPTFrameMap: Page table already present.");
            current_syscall_error.type = seL4_DeleteFirst;
            return EXCEPTION_SYSCALL_ERROR;
        }
        if ((ept_pde_ptr_get_page_size(pdSlot) == ept_pde_ept_pde_2m) &&
                ept_pde_ept_pde_2m_ptr_get_read(pdSlot)) {
            userError("IA32EPTFrameMap: Mapping already present.");
            current_syscall_error.type = seL4_DeleteFirst;
            return EXCEPTION_SYSCALL_ERROR;
        }

        if (LARGE_PAGE_BITS == X86_4M_bits) {
#if 0
            pdSlot[1] = ept_pde_ept_pde_2m_new(
                            paddr + (1 << 21),
                            mdb_node_get_cdtDepth(cte->cteMDBNode),
                            0,
                            eptCacheFromVmAttr(vmAttr),
                            1,
                            WritableFromVMRights(vmRights),
                            1);
#endif
            assert(!"not implemented");
        }
#if 0
        pdSlot[0] = ept_pde_ept_pde_2m_new(
                        paddr,
                        mdb_node_get_cdtDepth(cte->cteMDBNode),
                        0,
                        eptCacheFromVmAttr(vmAttr),
                        1,
                        WritableFromVMRights(vmRights),
                        1);
#endif
        assert(!"not implemented");

        invept((void*)((uint32_t)pdpt - EPT_PDPT_OFFSET));

#if 0
        cap = cap_frame_cap_set_capFMappedObject(cap, EPT_PD_REF(lu_ret.pd));
        cap = cap_frame_cap_set_capFMappedIndex(cap, lu_ret.pdIndex);
        cap = cap_frame_cap_set_capFMappedType(cap, IA32_MAPPING_EPT);
        cdtUpdate(cte, cap);
#endif
        assert(!"not implemented");

        setThreadState(ksCurThread, ThreadState_Restart);
        return EXCEPTION_NONE;
    }

    default:
        fail("Invalid page type");
    }
}

void IA32EptPdpt_Init(ept_pml4e_t * pdpt)
{
    /* Map in a PDPT for the 512GB region. */
    pdpt[0] = ept_pml4e_new(
                  pptr_to_paddr((void*)(pdpt + BIT(EPT_PDPT_BITS))),
                  1,
                  1,
                  1
              );
    invept(pdpt);
}

void IA32PageUnmapEPT(cap_t cap)
{
    (void)lookupEPTPDPTFromPT;
#if 0
    void *object = (void*)cap_frame_cap_get_capFMappedObject(cap);
    uint32_t index = cap_frame_cap_get_capFMappedIndex(cap);
    ept_pdpte_t *pdpt;
    switch (cap_frame_cap_get_capFSize(cap)) {
    case IA32_SmallPage: {
        ept_pte_t *pt = EPT_PT_PTR(object);
        pt[index] = ept_pte_new(0, 0, 0, 0, 0, 0, 0);
        pdpt = lookupEPTPDPTFromPT(pt);
        break;
    }
    case IA32_LargePage: {
        ept_pde_t *pd = EPT_PD_PTR(object);
        pd[index] = ept_pde_ept_pde_2m_new(0, 0, 0, 0, 0, 0, 0);
        if (LARGE_PAGE_BITS == X86_4M_bits) {
            pd[index + 1] = ept_pde_ept_pde_2m_new(0, 0, 0, 0, 0, 0, 0);
        }
        pdpt = lookupEPTPDPTFromPD(pd);
        break;
    }
    default:
        fail("Invalid page type");
    }
    if (pdpt) {
        invept((void*)((uint32_t)pdpt - EPT_PDPT_OFFSET));
    }
#endif
    assert(!"not implemented");
}

#endif /* VTX */

