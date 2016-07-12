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
#include <types.h>
#include <api/failures.h>
#include <kernel/vspace.h>
#include <object/structures.h>
#include <arch/machine.h>
#include <arch/model/statedata.h>
#include <arch/machine/fpu.h>
#include <arch/object/objecttype.h>
#include <arch/object/ioport.h>

#include <arch/object/iospace.h>
#include <plat/machine/intel-vtd.h>


bool_t
Arch_isFrameType(word_t type)
{
    switch (type) {
    case seL4_X86_4K:
        return true;
    case seL4_X86_LargePageObject:
        return true;
    default:
        return false;
    }
}

deriveCap_ret_t
Mode_deriveCap(cte_t* slot, cap_t cap)
{
    deriveCap_ret_t ret;

    switch (cap_get_capType(cap)) {
    case cap_frame_cap:
        cap = cap_frame_cap_set_capFMapType(cap, X86_MAPPING_NONE);
        ret.cap = cap_frame_cap_set_capFMappedASID(cap, asidInvalid);
        ret.status = EXCEPTION_NONE;
        return ret;

    default:
        /* This assert has no equivalent in haskell,
         * as the options are restricted by type */
        fail("Invalid arch cap type");
    }
}

cap_t Mode_finaliseCap(cap_t cap, bool_t final)
{
    switch (cap_get_capType(cap)) {

    case cap_pdpt_cap:
        if (final && cap_pdpt_cap_get_capPDPTIsMapped(cap)) {
            deleteASID(
                cap_pdpt_cap_get_capPDPTMappedASID(cap),
                (vspace_root_t*)PDPTE_PTR(cap_pdpt_cap_get_capPDPTBasePtr(cap))
            );
        }
        break;

    case cap_frame_cap:
        if (final && cap_frame_cap_get_capFMappedASID(cap)) {
            switch (cap_frame_cap_get_capFMapType(cap)) {
            case X86_MAPPING_EPT:
                assert(!"not implemented");
                break;
            case X86_MAPPING_VSPACE:
                unmapPage(
                    cap_frame_cap_get_capFSize(cap),
                    cap_frame_cap_get_capFMappedASID(cap),
                    cap_frame_cap_get_capFMappedAddress(cap),
                    (void *)cap_frame_cap_get_capFBasePtr(cap)
                );
                break;
            case X86_MAPPING_IOSPACE:
                unmapIOPage(cap);
                break;
            default:
                fail("No mapping type for mapped cap");
                break;
            }
        }
        break;

    default:
        fail("Invalid arch cap type");
    }

    return cap_null_cap_new();
}

cap_t CONST
Mode_resetMemMapping(cap_t cap)
{
    return cap;
}

cap_t Mode_recycleCap(bool_t is_final, cap_t cap)
{
    switch (cap_get_capType(cap)) {

    case cap_pdpt_cap:
        clearMemory((void*)cap_get_capPtr(cap), cap_get_capSizeBits(cap));
        Mode_finaliseCap(cap, is_final);
        if (is_final) {
            return resetMemMapping(cap);
        }
        return cap;

    default:
        fail("Invalid arch cap type");
    }
}


bool_t CONST Mode_sameRegionAs(cap_t cap_a, cap_t cap_b)
{
    return false;
}

word_t
Mode_getObjectSize(word_t t)
{
    fail("Invalid object type");
    return 0;
}

cap_t
Mode_createObject(object_t t, void *regionBase, word_t userSize, bool_t deviceMemory)
{
    switch (t) {
    case seL4_X86_4K:
        if (!deviceMemory) {
            memzero(regionBase, 1 << pageBitsForSize(X86_SmallPage));
        }
        return cap_frame_cap_new(
                   X86_SmallPage,          /* capFSize             */
                   ASID_LOW(asidInvalid),  /* capFMappedASIDLow    */
                   false,                  /* capFMappedAddress    */
                   X86_MAPPING_NONE,       /* capFMapType          */
                   deviceMemory,           /* capFIsDevice         */
                   ASID_HIGH(asidInvalid), /* capFMappedASIDHigh   */
                   VMReadWrite,            /* capFVMRights         */
                   (word_t)regionBase      /* capFBasePtr          */
               );

    case seL4_X86_LargePageObject:
        if (!deviceMemory) {
            memzero(regionBase, 1 << pageBitsForSize(X86_LargePage));
        }
        return cap_frame_cap_new(
                   X86_LargePage,          /* capFSize             */
                   ASID_LOW(asidInvalid),  /* capFMappedASIDLow    */
                   false,                  /* capFMappedAddress    */
                   X86_MAPPING_NONE,       /* capFMapType          */
                   deviceMemory,           /* capFIsDevice         */
                   ASID_HIGH(asidInvalid), /* capFMappedASIDHigh   */
                   VMReadWrite,            /* capFVMRights         */
                   (word_t)regionBase      /* capFBasePtr          */
               );

    case seL4_X86_PageTableObject:
        memzero(regionBase, 1 << seL4_PageTableBits);
        return cap_page_table_cap_new(
                   0,                  /* capPTIsMapped        */
                   asidInvalid,        /* capPTMappedASID      */
                   0,                  /* capPTMappedAddress   */
                   (word_t)regionBase  /* capPTBasePtr         */
               );

    case seL4_X86_PageDirectoryObject:
        memzero(regionBase, 1 << seL4_PageDirBits);
#ifndef CONFIG_PAE_PAGING
        copyGlobalMappings(regionBase);
#endif
        return cap_page_directory_cap_new(
                   0,                  /* capPDIsMapped      */
                   asidInvalid,        /* capPDMappedASID    */
                   0,                  /* capPDMappedAddress */
                   (word_t)regionBase  /* capPDBasePtr       */
               );

#ifdef CONFIG_PAE_PAGING
    case seL4_IA32_PDPTObject:
        memzero(regionBase, 1 << seL4_PDPTBits);
        copyGlobalMappings(regionBase);

        return cap_pdpt_cap_new(
                   0,                  /* capPDPTIsMapped */
                   asidInvalid,        /* capPDPTMappedAsid*/
                   (word_t)regionBase  /* capPDPTBasePtr */
               );
#endif

    case seL4_X86_IOPageTableObject:
        memzero(regionBase, 1 << seL4_IOPageTableBits);
        return cap_io_page_table_cap_new(
                   0,  /* capIOPTIsMapped      */
                   0,  /* capIOPTLevel         */
                   0,  /* capIOPTMappedAddress */
                   0,  /* capIOPTIOASID        */
                   (word_t)regionBase  /* capIOPTBasePtr */
               );

    default:
        fail("Mode_createObject got an API type or invalid object type");
    }
}

exception_t
Mode_decodeInvocation(
    word_t invLabel,
    word_t length,
    cptr_t cptr,
    cte_t* slot,
    cap_t cap,
    extra_caps_t excaps,
    word_t* buffer
)
{
    switch (cap_get_capType(cap)) {
    case cap_pdpt_cap:
    case cap_page_directory_cap:
    case cap_page_table_cap:
    case cap_frame_cap:
        return decodeX86MMUInvocation(invLabel, length, cptr, slot, cap, excaps, buffer);
    default:
        current_syscall_error.type = seL4_InvalidCapability;
        current_syscall_error.invalidCapNumber = 0;
        return EXCEPTION_SYSCALL_ERROR;
    }
}
