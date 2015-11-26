/*
 * Copyright 2014, General Dynamics C4 Systems
 *
 * This software may be distributed and modified according to the terms of
 * the GNU General Public License version 2. Note that NO WARRANTY is provided.
 * See "LICENSE_GPLv2.txt" for details.
 *
 * @TAG(GD_GPL)
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
#ifdef CONFIG_PAE_PAGING
    seL4_X86_PDPTObject,
#endif
#ifdef X86_64
    seL4_X86_PDPTObject,
    seL4_X64_PML4Object,
#endif
    seL4_X86_IOPageTableObject,
    seL4_ObjectTypeCount
#ifndef CONFIG_HUGE_PAGE
    ,
    seL4_X64_HugePageObject
#endif
#if !defined(CONFIG_PAE_PAGING) && !defined(X86_64)
    ,
    seL4_X86_PDPTObject
#endif
} seL4_ArchObjectType;
typedef word_t object_t;

/* Previously frame types were explcitly 4K and 4M. If not PAE
 * we assume legacy environment and emulate old definitions */
#ifndef CONFIG_PAE_PAGING
#define seL4_X86_4M seL4_X86_LargePage
#endif

#endif
