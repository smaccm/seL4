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
    seL4_IA32_4K = seL4_NonArchObjectTypeCount,
    seL4_IA32_LargePage,
#ifdef CONFIG_HUGE_PAGE
    seL4_IA32_HugePage,
#endif
    seL4_IA32_PageTableObject,
    seL4_IA32_PageDirectoryObject,
    seL4_IA32_PDPTObject,
    seL4_IA32_PML4Object,
    seL4_IA32_IOPageTableObject,
    seL4_ObjectTypeCount
#ifndef CONFIG_HUGE_PAGE
    ,
    seL4_IA32_HugePage
#endif
} seL4_ArchObjectType;

typedef seL4_Uint32 object_t;

/* Previously frame types were explcitly 4K and 2M. If not PAE
 * we assume legacy environment and emulate old definitions */
#ifndef CONFIG_PAE_PAGING
#define seL4_IA32_2M seL4_IA32_LargePage
#define seL4_IA32_1G seL4_IA32_HugePage
#endif

#endif
