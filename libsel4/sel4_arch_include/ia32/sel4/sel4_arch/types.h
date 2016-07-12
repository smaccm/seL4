/*
 * Copyright 2014, NICTA
 *
 * This software may be distributed and modified according to the terms of
 * the BSD 2-Clause license. Note that NO WARRANTY is provided.
 * See "LICENSE_BSD2.txt" for details.
 *
 * @TAG(NICTA_BSD)
 */

#ifndef __LIBSEL4_SEL4_ARCH_TYPES_H
#define __LIBSEL4_SEL4_ARCH_TYPES_H

#include <autoconf.h>

typedef seL4_Uint32 seL4_Word;
typedef seL4_Word seL4_NodeId;
typedef seL4_Word seL4_PAddr;
typedef seL4_Word seL4_Domain;

typedef seL4_Word seL4_CPtr;

/* User context as used by seL4_TCB_ReadRegisters / seL4_TCB_WriteRegisters */
typedef struct seL4_UserContext_ {
    /* frameRegisters */
    seL4_Word eip, esp, eflags, eax, ebx, ecx, edx, esi, edi, ebp;
    /* gpRegisters */
    seL4_Word tls_base, fs, gs;
} seL4_UserContext;

typedef struct seL4_VCPUContext_ {
    seL4_Word eax, ebx, ecx, edx, esi, edi, ebp;
} seL4_VCPUContext;

#endif
