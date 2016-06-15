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
    seL4_X86_4K = seL4_ModeObjectTypeCount,
    seL4_X86_LargePageObject,
    seL4_X86_PageTableObject,
    seL4_X86_PageDirectoryObject,
#ifdef CONFIG_IOMMU
    seL4_X86_IOPageTableObject,
#endif
#ifdef CONFIG_VTX
    seL4_X86_VCPUObject,
    seL4_X86_EPTPageDirectoryPointerTableObject,
    seL4_X86_EPTPageDirectoryObject,
    seL4_X86_EPTPageTableObject,
#endif
    seL4_ObjectTypeCount
} seL4_ArchObjectType;
typedef seL4_Word object_t;

#ifndef CONFIG_IOMMU
#define seL4_X86_IOPageTableObject 0xffffff
#endif

#ifndef CONFIG_VTX
#define seL4_X86_VCPUObject 0xfffffe
#define seL4_X86_EPTPageDirectoryPointerTableObject 0xfffffd
#define seL4_X86_EPTPageDirectoryObject 0xfffffc
#define seL4_X86_EPTPageTableObject 0xfffffb
#endif

#endif
