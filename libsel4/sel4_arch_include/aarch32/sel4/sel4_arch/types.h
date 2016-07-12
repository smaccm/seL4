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

#include <sel4/simple_types.h>

typedef seL4_Uint32 seL4_Word;
typedef seL4_Word seL4_CPtr;
typedef seL4_Uint64 seL4_Time;
typedef seL4_Word seL4_NodeId;
typedef seL4_Word seL4_PAddr;
typedef seL4_Word seL4_Domain;

typedef struct seL4_UserContext_ {
    /* frame registers */
    seL4_Word pc, sp, cpsr, r0, r1, r8, r9, r10, r11, r12;
    /* other integer registers */
    seL4_Word r2, r3, r4, r5, r6, r7, r14;
} seL4_UserContext;

typedef enum {
    seL4_ARM_PageCacheable = 0x01,
    seL4_ARM_ParityEnabled = 0x02,
    seL4_ARM_Default_VMAttributes = 0x03,
    seL4_ARM_ExecuteNever  = 0x04,
    /* seL4_ARM_PageCacheable | seL4_ARM_ParityEnabled */
    SEL4_FORCE_LONG_ENUM(seL4_ARM_VMAttributes),
} seL4_ARM_VMAttributes;

#endif
