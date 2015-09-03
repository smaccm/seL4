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
#include <sel4/simple_types.h>

typedef seL4_Uint64 seL4_Word;
typedef seL4_Word seL4_NodeId;
typedef seL4_Word seL4_PAddr;
typedef seL4_Word seL4_Domain;

typedef seL4_Word seL4_CPtr;

typedef seL4_CPtr seL4_X64_PML4;

/* User context as used by seL4_TCB_ReadRegisters / seL4_TCB_WriteRegisters */

typedef struct seL4_UserContext_ {
    seL4_Word rip, rsp, rflags, rax, rbx, rcx, rdx, rsi, rdi, rbp,
              r8, r9, r10, r11, r12, r13, r14, r15;
    seL4_Word tls_base;
} seL4_UserContext;

#endif
