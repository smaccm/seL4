/*
 * Copyright 2014, General Dynamics C4 Systems
 *
 * This software may be distributed and modified according to the terms of
 * the GNU General Public License version 2. Note that NO WARRANTY is provided.
 * See "LICENSE_GPLv2.txt" for details.
 *
 * @TAG(GD_GPL)
 */

#ifndef __KERNEL_THREAD_H
#define __KERNEL_THREAD_H

#include <types.h>
#include <util.h>
#include <object/structures.h>

static inline PURE uint32_t
ready_queues_index(uint32_t dom, uint32_t prio)
{
    if (CONFIG_NUM_DOMAINS > 1) {
        return dom * CONFIG_NUM_PRIORITIES + prio;
    } else {
        assert(dom == 0);
        return prio;
    }
}

/* the l1 index is the top 32 bits of prio (2^5 == 32) */
static inline PURE uint32_t
prio_to_l1index(uint32_t prio)
{
    return (prio >> 5);
}

static inline PURE uint32_t
l1index_to_prio(uint32_t l1index)
{
    return (l1index << 5);
}

static inline void
addToBitmap(word_t dom, word_t prio)
{
    uint32_t l1index;

    l1index = prio_to_l1index(prio);
    ksReadyQueuesL1Bitmap[dom] |= BIT(l1index);
    ksReadyQueuesL2Bitmap[dom][l1index] |= BIT(prio & MASK(5));
}

static inline void
removeFromBitmap(word_t dom, word_t prio)
{
    uint32_t l1index;

    l1index = prio_to_l1index(prio);
    ksReadyQueuesL2Bitmap[dom][l1index] &= ~BIT(prio & MASK(5));
    if (unlikely(!ksReadyQueuesL2Bitmap[dom][l1index])) {
        ksReadyQueuesL1Bitmap[dom] &= ~BIT(l1index);
    }
}

void configureIdleThread(tcb_t *tcb);
void activateThread(void) VISIBLE;
void suspend(tcb_t *target);
void restart(tcb_t *target);
void doIPCTransfer(tcb_t *sender, endpoint_t *endpoint,
                   word_t badge, bool_t grant, tcb_t *receiver,
                   bool_t diminish);
void doReplyTransfer(tcb_t *sender, tcb_t *receiver, cte_t *slot, bool_t donate, cap_t cap);
void doNormalTransfer(tcb_t *sender, word_t *sendBuffer, endpoint_t *endpoint,
                      word_t badge, bool_t canGrant, tcb_t *receiver,
                      word_t *receiveBuffer, bool_t diminish);
void doFaultTransfer(word_t badge, tcb_t *sender, tcb_t *receiver,
                     word_t *receiverIPCBuffer);
void doPollFailedTransfer(tcb_t *thread);
void schedule(void);
tcb_t *getHighestPrioThread(void);
tcb_t *pickThread(void);
void chooseThread(void);
void releaseJob(sched_context_t *toRelease);
void completeCurrentJob(void);
uint64_t getNextInterrupt(void);
bool_t raiseTemporalException(tcb_t *tcb);

/* deadline heap ops */
void deadlineAdd(sched_context_t *sc);
void deadlineBehead(void);
void deadlineRemove(sched_context_t *sc);
void deadlinePostpone(void);
void deadlineHeadChanged(void);

/* release heap ops */
void releaseBehead(void);
void releaseAdd(sched_context_t *sc);
void releaseRemove(sched_context_t *sc);
void releaseHeadChanged(void);
void postpone(sched_context_t *sc);

void enqueueJob(sched_context_t *sc, tcb_t *tcb);
void releaseJobs(void);
void enforceBudget(void);
void updateBudget(void);
void resumeSchedContext(sched_context_t *sc);

uint32_t getHighestPrio(void);

void switchToThread(tcb_t *thread) VISIBLE;
void switchToIdleThread(void);
void setDomain(tcb_t *tptr, dom_t dom);
void setPriority(tcb_t *tptr, tcb_prio_t prio);
void scheduleTCB(tcb_t *tptr);
void attemptSwitchTo(tcb_t *tptr, bool_t donate);
void switchIfRequiredTo(tcb_t *tptr, bool_t donate);
void setThreadState(tcb_t *tptr, _thread_state_t ts) VISIBLE;
void timerTick(void);
void rescheduleRequired(void);

#endif
