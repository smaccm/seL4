/*
 * Copyright 2014, NICTA
 *
 * This software may be distributed and modified according to the terms of
 * the BSD 2-Clause license. Note that NO WARRANTY is provided.
 * See "LICENSE_BSD2.txt" for details.
 *
 * @TAG(NICTA_BSD)
 */

#ifndef __LIBSEL4_ARCH_DEPRECATED_H
#define __LIBSEL4_ARCH_DEPRECATED_H

#include <autoconf.h>
#include <sel4/types.h>
#include <sel4/arch/syscalls.h>

#ifdef CONFIG_LIB_SEL4_HAVE_REGISTER_STUBS
static inline seL4_MessageInfo_t __attribute__((deprecated("Use seL4_ReplyRecvWithMRs")))
seL4_ReplyWaitWithMRs(seL4_CPtr src, seL4_MessageInfo_t msgInfo, seL4_Word *sender,
                      seL4_Word *mr0, seL4_Word *mr1)
{
    return seL4_ReplyRecvWithMRs(src, msgInfo, sender, mr0, mr1);
}
#endif /* CONFIG_LIB_SEL4_HAVE_REGISTER_STUBS */

#define seL4_IA32_PageDirectoryObject seL4_X86_PageDirectoryObject
#define seL4_IA32_PageTableObject seL4_X86_PageTableObject
#define seL4_IA32_4K seL4_X86_4K
#define seL4_IA32_LargePage seL4_X86_LargePageObject
#define seL4_IA32_HugePage seL4_X64_HugePageObject
#define seL4_IA32_PML4Object seL4_X64_PML4Object
#define seL4_IA32_PDPTObject seL4_X86_PDPTObject

#define seL4_IA32_Default_VMAttributes seL4_X86_Default_VMAttributes
#define seL4_IA32_VMAttributes seL4_X86_VMAttributes
#define seL4_IA32_CacheDisabled seL4_X86_CacheDisabled
#define seL4_IA32_Page_Map seL4_X86_Page_Map
#define seL4_IA32_PageDirectory_Map seL4_X86_PageDirectory_Map
#define seL4_IA32_PDPT_Map seL4_X86_PDPT_Map
#define seL4_IA32_PageTable_Map seL4_X86_PageTable_Map
#define seL4_IA32_Page_Remap seL4_X86_Page_Remap
#define seL4_IA32_Page_Unmap seL4_X86_Page_Unmap
#define seL4_IA32_ASIDControl_MakePool seL4_X86_ASIDControl_MakePool
#define seL4_IA32_ASIDPool_Assign seL4_X86_ASIDPool_Assign

#endif /* __ARCH_DEPRECATED_H__ */
