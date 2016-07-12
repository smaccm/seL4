/*
 * Copyright 2014, NICTA
 *
 * This software may be distributed and modified according to the terms of
 * the BSD 2-Clause license. Note that NO WARRANTY is provided.
 * See "LICENSE_BSD2.txt" for details.
 *
 * @TAG(NICTA_BSD)
 */

#ifndef __LIBSEL4_TYPES_H
#define __LIBSEL4_TYPES_H

#include <sel4/simple_types.h>
#include <sel4/macros.h>
#include <sel4/arch/types.h>
#include <sel4/sel4_arch/types.h>
#include <sel4/shared_types_gen.h>
#include <sel4/types_gen.h>
#include <sel4/syscall.h>
#include <sel4/objecttype.h>
#include <sel4/sel4_arch/objecttype.h>
#include <sel4/arch/objecttype.h>
#include <sel4/errors.h>
#include <sel4/constants.h>
#include <sel4/shared_types.h>

typedef enum {
    seL4_NoFault = 0,
    seL4_CapFault,
    seL4_VMFault,
    seL4_UnknownSyscall,
    seL4_UserException,
    seL4_TemporalFault,
    seL4_NoFaultHandler,
    SEL4_FORCE_LONG_ENUM(seL4_FaultType),
} seL4_FaultType;

typedef enum {
    seL4_NoFailure = 0,
    seL4_InvalidRoot,
    seL4_MissingCapability,
    seL4_DepthMismatch,
    seL4_GuardMismatch,
    SEL4_FORCE_LONG_ENUM(seL4_LookupFailureType),
} seL4_LookupFailureType;

typedef enum {
    seL4_CanWrite = 0x01,
    seL4_CanRead = 0x02,
    seL4_CanGrant = 0x04,
    seL4_AllRights = 0x07, /* seL4_CanWrite | seL4_CanRead | seL4_CanGrant */
    SEL4_FORCE_LONG_ENUM(seL4_CapRights),
} seL4_CapRights;

#define seL4_UntypedRetypeMaxObjects 256
#define seL4_GuardSizeBits 5
#define seL4_GuardBits 18
#define seL4_BadgeBits 28

typedef seL4_CPtr seL4_CNode;
typedef seL4_CPtr seL4_IRQHandler;
typedef seL4_CPtr seL4_IRQControl;
typedef seL4_CPtr seL4_TCB;
typedef seL4_CPtr seL4_Untyped;
typedef seL4_CPtr seL4_SchedContext;
typedef seL4_CPtr seL4_SchedControl;

#define seL4_NilData seL4_CapData_Badge_new(0)

#if CONFIG_MAX_NUM_TRACE_POINTS > 0
typedef struct {
    seL4_Word key;
    seL4_Word data;
} seL4_LogEntry;
#endif /* CONFIG_MAX_NUM_TRACE_POINTS > 0 */

#include <sel4/arch/constants.h>

#endif
