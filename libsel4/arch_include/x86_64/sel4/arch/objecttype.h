/*
 * Copyright 2014, NICTA
 *
 * This software may be distributed and modified according to the terms of
 * the BSD 2-Clause license. Note that NO WARRANTY is provided.
 * See "LICENSE_BSD2.txt" for details.
 *
 * @TAG(NICTA_BSD)
 */

#ifndef __ARCH_OBJECT_TYPE_H
#define __ARCH_OBJECT_TYPE_H

#ifdef HAVE_AUTOCONF
#include <autoconf.h>
#endif /* HAVE_AUTOCONF */

typedef enum _object {
    seL4_X86_4K = seL4_NonArchObjectTypeCount,
    seL4_X86_LargePageObject,
#ifdef CONFIG_HUGE_PAGE
    seL4_X64_HugePageObject,
#endif
    seL4_X86_PageTableObject,
    seL4_X86_PageDirectoryObject,
    seL4_X86_PDPTObject,
    seL4_X64_PML4Object,
    seL4_X86_IOPageTableObject,
    seL4_ObjectTypeCount
#ifndef CONFIG_HUGE_PAGE
    ,
    seL4_X64_HugePageObject
#endif
} seL4_ArchObjectType;

typedef seL4_Uint32 object_t;

/* Previously frame types were explcitly 4K and 2M. If not PAE
 * we assume legacy environment and emulate old definitions */
#ifndef CONFIG_PAE_PAGING
#define seL4_X64_2M seL4_X86_LargePageObject
#define seL4_X64_1G seL4_X64_HugePageObject
#endif

#endif
