/*
 * Copyright 2014, NICTA
 *
 * This software may be distributed and modified according to the terms of
 * the BSD 2-Clause license. Note that NO WARRANTY is provided.
 * See "LICENSE_BSD2.txt" for details.
 *
 * @TAG(NICTA_BSD)
 */

#ifndef __API_CONSTANTS_H
#define __API_CONSTANTS_H

#define LIBSEL4_BIT(n) (1ul<<(n))

typedef enum {
    seL4_IOBreakpoint = 0,
    seL4_DataBreakpoint,
    seL4_InstructionBreakpoint,
    seL4_SingleStep
} seL4_BreakpointType;

typedef enum {
    seL4_BreakOnRead = 0,
    seL4_BreakOnWrite,
    seL4_BreakOnReadWrite,
    seL4_MaxBreakpointAccess
} seL4_BreakpointAccess;

typedef enum {
    seL4_DebugException_Breakpoint = 0,
    seL4_DebugException_Watchpoint,
    seL4_DebugException_SingleStep
} seL4_DebugExceptionReason;

#define seL4_SingleStep_Disable     0

enum priorityConstants {
    seL4_InvalidPrio = -1,
    seL4_MinPrio = 0,
    seL4_MaxPrio = CONFIG_NUM_PRIORITIES - 1
};

/* seL4_MessageInfo_t defined in api/shared_types.bf */

enum seL4_MsgLimits {
    seL4_MsgLengthBits = 7,
    seL4_MsgExtraCapBits = 2
};

enum {
    seL4_MsgMaxLength = 120,
};
#define seL4_MsgMaxExtraCaps (LIBSEL4_BIT(seL4_MsgExtraCapBits)-1)

#endif /* __API_CONSTANTS_H */
