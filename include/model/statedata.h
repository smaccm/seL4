/*
 * Copyright 2014, General Dynamics C4 Systems
 *
 * This software may be distributed and modified according to the terms of
 * the GNU General Public License version 2. Note that NO WARRANTY is provided.
 * See "LICENSE_GPLv2.txt" for details.
 *
 * @TAG(GD_GPL)
 */

#ifndef __MODEL_STATEDATA_H
#define __MODEL_STATEDATA_H

#include <config.h>
#include <types.h>
#include <object/structures.h>
#include <object/tcb.h>
#include <object/schedcontext.h>
#include <arch/model/statedata.h>

extern tcb_queue_t ksReadyQueues[] VISIBLE;
extern word_t ksReadyQueuesL1Bitmap[CONFIG_NUM_DOMAINS] VISIBLE;
extern word_t ksReadyQueuesL2Bitmap[CONFIG_NUM_DOMAINS][(CONFIG_NUM_PRIORITIES / wordBits) + 1] VISIBLE;
extern tcb_t *ksCurThread VISIBLE;
extern tcb_t *ksIdleThread VISIBLE;
extern sc_prio_queue_t ksDeadlinePQ VISIBLE;
extern sc_prio_queue_t ksReleasePQ VISIBLE;
extern tcb_queue_t ksCriticalityQueues[] VISIBLE;
extern uint64_t ksCurrentTime VISIBLE;
extern bool_t ksReprogram VISIBLE;
extern uint32_t ksTicksPerUs;
extern sched_context_t *ksSchedContext;
extern uint32_t ksCriticality;
extern tcb_t *ksSchedulerAction VISIBLE;
extern word_t ksWorkUnitsCompleted;
extern irq_state_t intStateIRQTable[] VISIBLE;
extern cte_t *intStateIRQNode VISIBLE;

extern const dschedule_t ksDomSchedule[];
extern const unsigned int ksDomScheduleLength;
extern uint32_t ksDomScheduleIdx;
extern dom_t ksCurDomain;
extern word_t ksDomainTime;

#define SchedulerAction_ResumeCurrentThread ((tcb_t*)0)
#define SchedulerAction_ChooseNewThread ((tcb_t*)~0)

#define NUM_READY_QUEUES (CONFIG_NUM_DOMAINS * CONFIG_NUM_PRIORITIES)

#endif
