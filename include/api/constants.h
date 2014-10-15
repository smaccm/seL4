/*
 * Copyright 2014, General Dynamics C4 Systems
 *
 * This software may be distributed and modified according to the terms of
 * the GNU General Public License version 2. Note that NO WARRANTY is provided.
 * See "LICENSE_GPLv2.txt" for details.
 *
 * @TAG(GD_GPL)
 */

#ifndef __API_CONSTANTS_H
#define __API_CONSTANTS_H

#define BIT(n) (1ul<<(n))

enum priorityConstants {
    seL4_InvalidPrio = -1,
    seL4_MinPrio = 0,
#ifdef CONFIG_EDF
    seL4_EDFPrio = 125,
#endif /* CONFIG_EDF */
    seL4_MaxPrio = 255,
};

enum {
    seL4_SoftCBS = 0,
    seL4_HardCBS = 1,
    seL4_Isolated = 2
};
typedef uint32_t seL4_CBS;

typedef uint32_t seL4_TaskType;
enum {
    seL4_EventTriggered = 0,
    seL4_TimeTriggered = 1
};

typedef struct {
    uint64_t period;
    uint64_t deadline;
    uint64_t budget;
    seL4_SchedFlags_t flags;
} seL4_SchedParams_t;

/* message_info_t defined in api/types.bf */

enum seL4_MsgLimits {
    seL4_MsgLengthBits = 7,
    seL4_MsgExtraCapBits = 2
};


#define seL4_MsgMaxLength 120
#define seL4_MsgMaxExtraCaps (BIT(seL4_MsgExtraCapBits)-1)

#endif /* __API_CONSTANTS_H */
