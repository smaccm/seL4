/*
 * Copyright 2014, NICTA
 *
 * This software may be distributed and modified according to the terms of
 * the BSD 2-Clause license. Note that NO WARRANTY is provided.
 * See "LICENSE_BSD2.txt" for details.
 *
 * @TAG(NICTA_BSD)
 */

#ifndef __SEL4_ARCH_OBJECT_TYPE_H
#define __SEL4_ARCH_OBJECT_TYPE_H

#ifdef HAVE_AUTOCONF
#include <autoconf.h>
#endif /* HAVE_AUTOCONF */

typedef enum _mode_object {
    seL4_X86_PDPTObject = seL4_NonArchObjectTypeCount,
    seL4_X64_PML4Object,
#ifdef CONFIG_HUGE_PAGE
    seL4_X64_HugePageObject,
#endif
    seL4_ModeObjectTypeCount
} seL4_seL4ArchObjectType;

/* allow seL4_X86_PDPTObject and seL4_IA32_PDPTObject to be used interchangeable */
#define seL4_IA32_PDPTObject seL4_X86_PDPTObject

#ifndef CONFIG_HUGE_PAGE
#define seL4_X64_HugePageObject 0xfffffffe
#endif

#endif
