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
extern tcb_t *ksCurThread VISIBLE;
extern tcb_t *ksIdleThread VISIBLE;
extern sc_prio_queue_t ksDeadlinePQ VISIBLE;
extern sc_prio_queue_t ksReleasePQ VISIBLE;
extern uint64_t ksCurrentTime VISIBLE;
extern bool_t ksReprogram VISIBLE;
extern uint32_t ksTicksPerUs;
extern sched_context_t *ksSchedContext;
#ifdef CONFIG_BENCHMARK
extern tcb_t *suspended;
#endif
extern tcb_t *ksSchedulerAction VISIBLE;
extern bool_t ksRestoreSC VISIBLE;
extern word_t ksWorkUnitsCompleted;
extern irq_state_t intStateIRQTable[] VISIBLE;
extern cte_t *intStateIRQNode VISIBLE;

#ifdef CONFIG_BENCHMARK
extern tcb_t *ksWaitingThread;
#endif /* CONFIG_BENCHMARK */
extern const dschedule_t ksDomSchedule[];
extern const unsigned int ksDomScheduleLength;
extern uint32_t ksDomScheduleIdx;
extern dom_t ksCurDomain;
extern word_t ksDomainTime;

#define SchedulerAction_ResumeCurrentThread ((tcb_t*)0)
#define SchedulerAction_ChooseNewThread ((tcb_t*)~0)

#define NUM_READY_QUEUES (CONFIG_NUM_DOMAINS * CONFIG_NUM_PRIORITIES)

#endif
