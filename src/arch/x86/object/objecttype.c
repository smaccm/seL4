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
#include <plat/machine/pci.h>

#include <arch/object/iospace.h>
#include <plat/machine/intel-vtd.h>

deriveCap_ret_t Arch_deriveCap(cte_t* slot, cap_t cap)
{
    deriveCap_ret_t ret;

    switch (cap_get_capType(cap)) {

    case cap_page_table_cap:
        if (cap_page_table_cap_get_capPTIsMapped(cap)) {
            ret.cap = cap;
            ret.status = EXCEPTION_NONE;
        } else {
            userError("Deriving an unmapped PT cap");
            current_syscall_error.type = seL4_IllegalOperation;
            ret.cap = cap_null_cap_new();
            ret.status = EXCEPTION_SYSCALL_ERROR;
        }
        return ret;

    case cap_page_directory_cap:
        if (cap_page_directory_cap_get_capPDIsMapped(cap)) {
            ret.cap = cap;
            ret.status = EXCEPTION_NONE;
        } else {
            userError("Deriving a PD cap without an assigned ASID");
            current_syscall_error.type = seL4_IllegalOperation;
            ret.cap = cap_null_cap_new();
            ret.status = EXCEPTION_SYSCALL_ERROR;
        }
        return ret;

    case cap_pdpt_cap:
        if (cap_pdpt_cap_get_capPDPTIsMapped(cap)) {
            ret.cap = cap;
            ret.status = EXCEPTION_NONE;
        } else {
            userError("Deriving a PDPT cap without an assigned ASID");
            current_syscall_error.type = seL4_IllegalOperation;
            ret.cap = cap_null_cap_new();
            ret.status = EXCEPTION_SYSCALL_ERROR;
        }
        return ret;

    case cap_asid_control_cap:
    case cap_asid_pool_cap:
        ret.cap = cap;
        ret.status = EXCEPTION_NONE;
        return ret;

    case cap_io_port_cap:
        ret.cap = cap;
        ret.status = EXCEPTION_NONE;
        return ret;

    case cap_io_space_cap:
        ret.cap = cap;
        ret.status = EXCEPTION_NONE;
        return ret;

    case cap_io_page_table_cap:
        if (cap_io_page_table_cap_get_capIOPTIsMapped(cap)) {
            ret.cap = cap;
            ret.status = EXCEPTION_NONE;
        } else {
            current_syscall_error.type = seL4_IllegalOperation;
            ret.cap = cap_null_cap_new();
            ret.status = EXCEPTION_SYSCALL_ERROR;
        }
        return ret;

    default:
        return Mode_deriveCap(slot, cap);
        fail("Invalid arch cap type");
    }
}

cap_t CONST Arch_updateCapData(bool_t preserve, word_t data, cap_t cap)
{
    switch (cap_get_capType(cap)) {
    case cap_io_space_cap: {
        io_space_capdata_t w = { { data } };
        uint16_t PCIDevice = io_space_capdata_get_PCIDevice(w);
        uint16_t domainID = io_space_capdata_get_domainID(w);
        if (!preserve && cap_io_space_cap_get_capPCIDevice(cap) == 0 &&
                domainID >= ia32KSFirstValidIODomain &&
                domainID != 0                        &&
                domainID <= MASK(ia32KSnumIODomainIDBits)) {
            return cap_io_space_cap_new(domainID, PCIDevice);
        } else {
            return cap_null_cap_new();
        }
    }
    case cap_io_port_cap: {
        io_port_capdata_t w = { .words = { data } };
        uint16_t firstPort = io_port_capdata_get_firstPort(w);
        uint16_t lastPort = io_port_capdata_get_lastPort(w);
        uint16_t capFirstPort = cap_io_port_cap_get_capIOPortFirstPort(cap);
        uint16_t capLastPort = cap_io_port_cap_get_capIOPortLastPort(cap);
        assert(capFirstPort <= capLastPort);

        /* Ensure input data is ordered correctly. */
        if (firstPort > lastPort) {
            return cap_null_cap_new();
        }

        /* Allow the update if the new cap has range no larger than the old
         * cap. */
        if ((firstPort >= capFirstPort) && (lastPort <= capLastPort)) {
            return cap_io_port_cap_new(firstPort, lastPort);
        } else {
            return cap_null_cap_new();
        }
    }

    default:
        return cap;
    }
}

cap_t CONST Arch_maskCapRights(cap_rights_t cap_rights_mask, cap_t cap)
{
    if (cap_get_capType(cap) == cap_frame_cap) {
        vm_rights_t vm_rights;

        vm_rights = vmRightsFromWord(cap_frame_cap_get_capFVMRights(cap));
        vm_rights = maskVMRights(vm_rights, cap_rights_mask);
        return cap_frame_cap_set_capFVMRights(cap, wordFromVMRights(vm_rights));
    } else {
        return cap;
    }
}

cap_t Arch_finaliseCap(cap_t cap, bool_t final)
{

    switch (cap_get_capType(cap)) {

    case cap_page_directory_cap:
        if (final && cap_page_directory_cap_get_capPDIsMapped(cap)) {
            unmapPageDirectory(
                cap_page_directory_cap_get_capPDMappedASID(cap),
                cap_page_directory_cap_get_capPDMappedAddress(cap),
                PDE_PTR(cap_page_directory_cap_get_capPDBasePtr(cap))
            );
        }
        break;

    case cap_page_table_cap:
        if (final && cap_page_table_cap_get_capPTIsMapped(cap)) {
            unmapPageTable(
                cap_page_table_cap_get_capPTMappedASID(cap),
                cap_page_table_cap_get_capPTMappedAddress(cap),
                PT_PTR(cap_page_table_cap_get_capPTBasePtr(cap))
            );
        }
        break;


    case cap_asid_pool_cap:
        if (final) {
            deleteASIDPool(
                cap_asid_pool_cap_get_capASIDBase(cap),
                ASID_POOL_PTR(cap_asid_pool_cap_get_capASIDPool(cap))
            );
        }
        break;

    case cap_asid_control_cap:
    case cap_io_port_cap:
        break;

    case cap_io_page_table_cap:
        if (final && cap_io_page_table_cap_get_capIOPTIsMapped(cap)) {
            deleteIOPageTable(cap);
        }
        break;

    default:
        return Mode_finaliseCap(cap, final);
    }

    return cap_null_cap_new();
}

static cap_t CONST
resetMemMapping(cap_t cap)
{
    switch (cap_get_capType(cap)) {
    case cap_frame_cap:
        return cap_frame_cap_set_capFMappedASID(cap, asidInvalid);
    case cap_page_table_cap:
        /* We don't need to worry about clearing ASID and Address here, only whether it is mapped */
        return cap_page_table_cap_set_capPTIsMapped(cap, 0);
    case cap_page_directory_cap:
        /* We don't need to worry about clearing ASID and Address here, only whether it is mapped */
        return cap_page_directory_cap_set_capPDIsMapped(cap, 0);
    case cap_pdpt_cap:
        /* We don't need to worry about clearing ASID and Address here, only whether it is mapped */
        return cap_pdpt_cap_set_capPDPTIsMapped(cap, 0);
    }

    return Mode_resetMemMapping(cap);
}

cap_t Arch_recycleCap(bool_t is_final, cap_t cap)
{

    asid_pool_t* ptr;
    word_t base;

    switch (cap_get_capType(cap)) {

    case cap_frame_cap:
        clearMemory((void *)cap_get_capPtr(cap), cap_get_capSizeBits(cap));
        Arch_finaliseCap(cap, is_final);
        return resetMemMapping(cap);

    case cap_page_table_cap:
        clearMemory((void *)cap_get_capPtr(cap), cap_get_capSizeBits(cap));
        if (cap_page_table_cap_get_capPTIsMapped(cap)) {
            unmapPageTable(
                cap_page_table_cap_get_capPTMappedASID(cap),
                cap_page_table_cap_get_capPTMappedAddress(cap),
                PT_PTR(cap_page_table_cap_get_capPTBasePtr(cap))
            );
        }
        Arch_finaliseCap(cap, is_final);
        if (is_final) {
            return resetMemMapping(cap);
        }
        return cap;

    case cap_page_directory_cap:
        clearMemory((void*)cap_get_capPtr(cap), cap_get_capSizeBits(cap));
        if (cap_page_directory_cap_get_capPDIsMapped(cap)) {
            unmapPageDirectory(
                cap_page_directory_cap_get_capPDMappedASID(cap),
                cap_page_directory_cap_get_capPDMappedAddress(cap),
                PD_PTR(cap_page_directory_cap_get_capPDBasePtr(cap))
            );
        }
        Arch_finaliseCap(cap, is_final);
        if (is_final) {
            return resetMemMapping(cap);
        }
        return cap;

    case cap_asid_control_cap:
        return cap;

    case cap_asid_pool_cap:
        base = cap_asid_pool_cap_get_capASIDBase(cap);
        ptr = ASID_POOL_PTR(cap_asid_pool_cap_get_capASIDPool(cap));
        if (x86KSASIDTable[base >> asidLowBits] == ptr) {
            deleteASIDPool(base, ptr);
            memzero(ptr, BIT(ASID_POOL_SIZE_BITS));
            x86KSASIDTable[base >> asidLowBits] = ptr;
        }
        return cap;

    case cap_io_port_cap:
        return cap;

    case cap_io_space_cap:
        return cap;

    case cap_io_page_table_cap:
        clearMemory((void*)cap_get_capPtr(cap), cap_get_capSizeBits(cap));
        Arch_finaliseCap(cap, is_final);
        return cap;

    default:
        return Mode_recycleCap(is_final, cap);
    }
}


bool_t CONST
Arch_hasRecycleRights(cap_t cap)
{
    switch (cap_get_capType(cap)) {
    case cap_frame_cap:
        return cap_frame_cap_get_capFVMRights(cap) == VMReadWrite;

    default:
        return true;
    }
}


bool_t CONST Arch_sameRegionAs(cap_t cap_a, cap_t cap_b)
{
    switch (cap_get_capType(cap_a)) {
    case cap_frame_cap:
        if (cap_get_capType(cap_b) == cap_frame_cap) {
            word_t botA, botB, topA, topB;
            botA = cap_frame_cap_get_capFBasePtr(cap_a);
            botB = cap_frame_cap_get_capFBasePtr(cap_b);
            topA = botA + MASK (pageBitsForSize(cap_frame_cap_get_capFSize(cap_a)));
            topB = botB + MASK (pageBitsForSize(cap_frame_cap_get_capFSize(cap_b)));
            return ((botA <= botB) && (topA >= topB) && (botB <= topB));
        }
        break;

    case cap_page_table_cap:
        if (cap_get_capType(cap_b) == cap_page_table_cap) {
            return cap_page_table_cap_get_capPTBasePtr(cap_a) ==
                   cap_page_table_cap_get_capPTBasePtr(cap_b);
        }
        break;

    case cap_page_directory_cap:
        if (cap_get_capType(cap_b) == cap_page_directory_cap) {
            return cap_page_directory_cap_get_capPDBasePtr(cap_a) ==
                   cap_page_directory_cap_get_capPDBasePtr(cap_b);
        }
        break;

    case cap_pdpt_cap:
        if (cap_get_capType(cap_b) == cap_pdpt_cap) {
            return cap_pdpt_cap_get_capPDPTBasePtr(cap_a) ==
                   cap_pdpt_cap_get_capPDPTBasePtr(cap_b);
        }
        break;

    case cap_asid_control_cap:
        if (cap_get_capType(cap_b) == cap_asid_control_cap) {
            return true;
        }
        break;

    case cap_asid_pool_cap:
        if (cap_get_capType(cap_b) == cap_asid_pool_cap) {
            return cap_asid_pool_cap_get_capASIDPool(cap_a) ==
                   cap_asid_pool_cap_get_capASIDPool(cap_b);
        }
        break;

    case cap_io_port_cap:
        if (cap_get_capType(cap_b) == cap_io_port_cap) {
            return true;
        }
        break;

    case cap_io_space_cap:
        if (cap_get_capType(cap_b) == cap_io_space_cap) {
            return cap_io_space_cap_get_capPCIDevice(cap_a) ==
                   cap_io_space_cap_get_capPCIDevice(cap_b);
        }
        break;

    case cap_io_page_table_cap:
        if (cap_get_capType(cap_b) == cap_io_page_table_cap) {
            return cap_io_page_table_cap_get_capIOPTBasePtr(cap_a) ==
                   cap_io_page_table_cap_get_capIOPTBasePtr(cap_b);
        }
        break;

    }

    return Mode_sameRegionAs(cap_a, cap_b);
}

bool_t CONST Arch_sameObjectAs(cap_t cap_a, cap_t cap_b)
{

    if (cap_get_capType(cap_a) == cap_frame_cap) {
        if (cap_get_capType(cap_b) == cap_frame_cap) {
            return ((cap_frame_cap_get_capFBasePtr(cap_a) ==
                     cap_frame_cap_get_capFBasePtr(cap_b)) &&
                    (cap_frame_cap_get_capFSize(cap_a) ==
                     cap_frame_cap_get_capFSize(cap_b)));
        }
    }
    return Arch_sameRegionAs(cap_a, cap_b);
}

word_t
Arch_getObjectSize(word_t t)
{
    switch (t) {
    case seL4_IA32_4K:
        return pageBitsForSize(IA32_SmallPage);

    case seL4_IA32_LargePage:
        return pageBitsForSize(IA32_LargePage);

    case seL4_IA32_HugePage:
        return pageBitsForSize(IA32_HugePage);

    case seL4_IA32_PageTableObject:
        return PT_SIZE_BITS;
        
    case seL4_IA32_PageDirectoryObject:
        return PD_SIZE_BITS;

    case seL4_IA32_PDPTObject:
        return PDPT_SIZE_BITS;

    case seL4_IA32_IOPageTableObject:
        return VTD_PT_SIZE_BITS;

    default:
        return Mode_getObjectSize(t);
    }
}

cap_t
Arch_createObject(object_t t, void *regionBase, word_t userSize)
{
    return Mode_createObject(t, regionBase, userSize);
}

exception_t
Arch_decodeInvocation(
    word_t label,
    word_t length,
    cptr_t cptr,
    cte_t* slot,
    cap_t cap,
    extra_caps_t extraCaps,
    word_t* buffer
)
{
    switch (cap_get_capType(cap)) {
    case cap_asid_control_cap:
    case cap_asid_pool_cap:
        return decodeX86MMUInvocation(label, length, cptr, slot, cap, extraCaps, buffer);

    case cap_io_port_cap:
        return decodeX86PortInvocation(label, length, cptr, slot, cap, extraCaps, buffer);

    case cap_io_space_cap:
        return decodeIA32IOSpaceInvocation(label, cap);

    case cap_io_page_table_cap:
        return decodeIA32IOPTInvocation(label, length, slot, cap, extraCaps, buffer);

    default:
        return Mode_decodeInvocation(label, length, cptr, slot, cap, extraCaps, buffer);
    }
}

void
Arch_prepareThreadDelete(tcb_t *thread)
{
    Arch_fpuThreadDelete(thread);
}
