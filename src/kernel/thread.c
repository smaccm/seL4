/*
 * Copyright 2014, General Dynamics C4 Systems
 *
 * This software may be distributed and modified according to the terms of
 * the GNU General Public License version 2. Note that NO WARRANTY is provided.
 * See "LICENSE_GPLv2.txt" for details.
 *
 * @TAG(GD_GPL)
 */

#include <config.h>
#include <object.h>
#include <util.h>
#include <api/faults.h>
#include <api/types.h>
#include <kernel/cspace.h>
#include <kernel/thread.h>
#include <kernel/vspace.h>
#include <model/statedata.h>
#include <arch/machine.h>
#include <arch/kernel/thread.h>
#include <machine/registerset.h>
#include <arch/linker.h>
#include <object/structures.h>
#include <object/schedcontext.h>

#define TRACE(...)
//#define TRACE(...) printf(__VA_ARGS__)

static message_info_t
transferCaps(message_info_t info, extra_caps_t caps,
             endpoint_t *endpoint, tcb_t *receiver,
             word_t *receiveBuffer, bool_t diminish);

static inline bool_t PURE
isTimeTriggered(const sched_context_t *sc)
{
    return sched_context_status_get_trigger(sc->status) == seL4_TimeTriggered;
}


static inline bool_t PURE
isBlocked(const tcb_t *thread)
{
    switch (thread_state_get_tsType(thread->tcbState)) {
    case ThreadState_Inactive:
    case ThreadState_BlockedOnReceive:
    case ThreadState_BlockedOnSend:
    case ThreadState_BlockedOnAsyncEvent:
    case ThreadState_BlockedOnReply:
        return true;

    default:
        return false;
    }
}

BOOT_CODE void
configureIdleThread(tcb_t *tcb)
{
    Arch_configureIdleThread(tcb);
    setThreadState(tcb, ThreadState_IdleThreadState);
}

static inline void
recharge(sched_context_t *sc)
{
    sc->budgetRemaining = sc->budget;
    sc->nextRelease = ksCurrentTime + sc->period;
}

void
activateThread(void)
{
    switch (thread_state_get_tsType(ksCurThread->tcbState)) {
    case ThreadState_Running:
        break;

    case ThreadState_Restart: {
        word_t pc;

        pc = getRestartPC(ksCurThread);
        setNextPC(ksCurThread, pc);
        setThreadState(ksCurThread, ThreadState_Running);
        break;
    }

    case ThreadState_IdleThreadState:
        Arch_activateIdleThread(ksCurThread);
        break;

    default:
        fail("Current thread is blocked");
    }

    assert(ksCurThread->tcbSchedContext == NULL);
}

void
suspend(tcb_t *target)
{
    ipcCancel(target);
    setThreadState(target, ThreadState_Inactive);
    tcbSchedDequeue(target);
    tcbCritDequeue(target);

    if (target->tcbSchedContext) {
        sched_context_purge(target->tcbSchedContext);
    }

}

static inline PURE bool_t
readyToRelease(sched_context_t *sc)
{
    return sc->nextRelease <= ksCurrentTime + PLAT_LEEWAY;
}

void
resumeSchedContext(sched_context_t *sc)
{

    if (unlikely(sc == NULL || (sc->budget == 0 && sc->period == 0))) {
        return;
    }

    if (tcb_prio_get_criticality(sc->tcb->tcbPriority) < ksCriticality) {
        releaseAdd(sc);
    } else  {

        if (readyToRelease(sc)) {
            /* recharge time has passed, recharge */
            recharge(sc);
        }

        if (sc->budgetRemaining > PLAT_LEEWAY) {
            /* still has budget, release time has not passed, just resume */
            switchIfRequiredTo(sc->tcb, false);
        } else {
            /* release time hasn't passed, no budget left */
            releaseAdd(sc);
        }
    }
}


void
restart(tcb_t *target)
{
    if (isBlocked(target)) {
        ipcCancel(target);
        setThreadState(target, ThreadState_Restart);
        resumeSchedContext(target->tcbSchedContext);
        tcbCritEnqueue(target);
    }

}

void
doIPCTransfer(tcb_t *sender, endpoint_t *endpoint, word_t badge,
              bool_t grant, tcb_t *receiver, bool_t diminish)
{
    void *receiveBuffer, *sendBuffer;

    receiveBuffer = lookupIPCBuffer(true, receiver);

    if (likely(!fault_get_faultType(sender->tcbFault) != fault_null_fault)) {
        sendBuffer = lookupIPCBuffer(false, sender);
        doNormalTransfer(sender, sendBuffer, endpoint, badge, grant,
                         receiver, receiveBuffer, diminish);
    } else {
        doFaultTransfer(badge, sender, receiver, receiveBuffer);
    }
}

void
doReplyTransfer(tcb_t *sender, tcb_t *receiver, cte_t *slot, cap_t cap)
{

    tcb_t * callee;
    sched_context_t *reply_sc;
    bool_t donate;

    reply_sc = SC_PTR(cap_reply_cap_get_schedcontext(cap));
    donate = (reply_sc == sender->tcbSchedContext);

    if (!donate && reply_sc != NULL) {
        /* this case occurs when someone else replies on behalf of the original called thread,
         * at which point the sched context in the sc should be returned */
        callee = reply_sc->tcb;
        receiver->tcbSchedContext = reply_sc;
        reply_sc->tcb = receiver;

        if (callee) {
            rescheduleRequired();
            callee->tcbSchedContext = NULL;
        }

        /* TODO@alyons should this only happen in response to a temporal fault? */
        /* since the scheduling context was effectively paused,
         * we need to check if there is enough budget before scheduling it
         */
        if (reply_sc->budgetRemaining < PLAT_LEEWAY) {
            if (readyToRelease(reply_sc)) {
                recharge(reply_sc);
            } else if (raiseTemporalException(receiver)) {
                /* The reply isn't going to happen because the budget was already out,
                 * we are just bouncing temporal exceptions at this point */
                cteDeleteOne(slot);
                return;
            } else {
                resumeSchedContext(reply_sc);
            }
        }
    }

    assert(thread_state_get_tsType(receiver->tcbState) ==
           ThreadState_BlockedOnReply);

    if (likely(fault_get_faultType(receiver->tcbFault) == fault_null_fault)) {
        doIPCTransfer(sender, NULL, 0, true, receiver, false);
        cteDeleteOne(slot);
        setThreadState(receiver, ThreadState_Running);
        if (donate || isSchedulable(receiver)) {
            attemptSwitchTo(receiver, donate);
        }
    } else {
        bool_t restart;

        cteDeleteOne(slot);
        restart = handleFaultReply(receiver, sender);
        fault_null_fault_ptr_new(&receiver->tcbFault);
        if (restart) {
            setThreadState(receiver, ThreadState_Restart);
            if (donate || isSchedulable(receiver)) {
                attemptSwitchTo(receiver, donate);
            }
        } else {
            setThreadState(receiver, ThreadState_Inactive);
        }
    }

}

void
doNormalTransfer(tcb_t *sender, word_t *sendBuffer, endpoint_t *endpoint,
                 word_t badge, bool_t canGrant, tcb_t *receiver,
                 word_t *receiveBuffer, bool_t diminish)
{
    unsigned int msgTransferred;
    message_info_t tag;
    exception_t status;
    extra_caps_t caps;

    tag = messageInfoFromWord(getRegister(sender, msgInfoRegister));

    if (canGrant) {
        status = lookupExtraCaps(sender, sendBuffer, tag);
        caps = current_extra_caps;
        if (unlikely(status != EXCEPTION_NONE)) {
            caps.excaprefs[0] = NULL;
        }
    } else {
        caps = current_extra_caps;
        caps.excaprefs[0] = NULL;
    }

    msgTransferred = copyMRs(sender, sendBuffer, receiver, receiveBuffer,
                             message_info_get_msgLength(tag));

    tag = transferCaps(tag, caps, endpoint, receiver, receiveBuffer, diminish);

    tag = message_info_set_msgLength(tag, msgTransferred);
    setRegister(receiver, msgInfoRegister, wordFromMessageInfo(tag));
    setRegister(receiver, badgeRegister, badge);
}

void
doFaultTransfer(word_t badge, tcb_t *sender, tcb_t *receiver,
                word_t *receiverIPCBuffer)
{
    unsigned int sent;
    message_info_t msgInfo;

    sent = setMRs_fault(sender, receiver, receiverIPCBuffer);
    msgInfo = message_info_new(
                  fault_get_faultType(sender->tcbFault), 0, 0, sent);
    setRegister(receiver, msgInfoRegister, wordFromMessageInfo(msgInfo));
    setRegister(receiver, badgeRegister, badge);
}

/* Like getReceiveSlots, this is specialised for single-cap transfer. */
static message_info_t
transferCaps(message_info_t info, extra_caps_t caps,
             endpoint_t *endpoint, tcb_t *receiver,
             word_t *receiveBuffer, bool_t diminish)
{
    unsigned int i;
    cte_t* destSlot;

    info = message_info_set_msgExtraCaps(info, 0);
    info = message_info_set_msgCapsUnwrapped(info, 0);

    if (likely(!caps.excaprefs[0] || !receiveBuffer)) {
        return info;
    }

    destSlot = getReceiveSlots(receiver, receiveBuffer);

    for (i = 0; i < seL4_MsgMaxExtraCaps && caps.excaprefs[i] != NULL; i++) {
        cte_t *slot = caps.excaprefs[i];
        cap_t cap = slot->cap;

        if (cap_get_capType(cap) == cap_endpoint_cap &&
                EP_PTR(cap_endpoint_cap_get_capEPPtr(cap)) == endpoint) {
            /* If this is a cap to the endpoint on which the message was sent,
             * only transfer the badge, not the cap. */
            setExtraBadge(receiveBuffer,
                          cap_endpoint_cap_get_capEPBadge(cap), i);

            info = message_info_set_msgCapsUnwrapped(info,
                                                     message_info_get_msgCapsUnwrapped(info) | (1 << i));

        } else {
            deriveCap_ret_t dc_ret;

            if (!destSlot) {
                break;
            }

            if (diminish) {
                dc_ret = deriveCap(slot, maskCapRights(noWrite, cap));
            } else {
                dc_ret = deriveCap(slot, cap);
            }

            if (dc_ret.status != EXCEPTION_NONE) {
                break;
            }
            if (cap_get_capType(dc_ret.cap) == cap_null_cap) {
                break;
            }

            cteInsert(dc_ret.cap, slot, destSlot);

            destSlot = NULL;
        }
    }

    return message_info_set_msgExtraCaps(info, i);
}

void doPollFailedTransfer(tcb_t *thread)
{
    /* Set the badge register to 0 to indicate there was no message */
    setRegister(thread, badgeRegister, 0);
}

static void
nextDomain(void)
{
    ksDomScheduleIdx++;
    if (ksDomScheduleIdx >= ksDomScheduleLength) {
        ksDomScheduleIdx = 0;
    }
    ksWorkUnitsCompleted = 0;
    ksCurDomain = ksDomSchedule[ksDomScheduleIdx].domain;
    ksDomainTime = ksDomSchedule[ksDomScheduleIdx].length;
}

uint64_t
getNextInterrupt(void)
{

    /* TODO@alyons reevaluate IA32_EXTRA -- might just have to up PLAT_LEEWAY */
    /* TODO@alyons s/PLAT_LEEWAY/KERNEL_WCET */
    uint64_t nextInterrupt = UINT64_MAX - IA32_EXTRA;

    if (ksCurThread != ksIdleThread) {
        assert(ksSchedContext->budgetRemaining >= PLAT_LEEWAY);
        TRACE("deadline irq at %llx, budget %llx\n", ksSchedContext->budgetRemaining + ksCurrentTime,
              ksSchedContext->budgetRemaining);
        /* TODO@alyons avoid overflow here ???*/
        nextInterrupt = ksSchedContext->budgetRemaining + ksCurrentTime;
    }

    assert(ksSchedContext->budgetRemaining > PLAT_LEEWAY);
    assert(nextInterrupt > ksCurrentTime);
    if (ksReleasePQ.head != NULL && ksReleasePQ.head->nextRelease < nextInterrupt) {
        TRACE("Release irq\n");
        nextInterrupt = ksReleasePQ.head->nextRelease;
    }

#ifdef CONFIG_DEBUG_BUILD
    if (nextInterrupt <= ksCurrentTime) {
        printf("Next interrupt %llx, ksCurrentTime %llx\n", nextInterrupt, ksCurrentTime);
    }
    assert(nextInterrupt > ksCurrentTime);
#endif /* CONFIG_DEBUG_BUILD */

    return nextInterrupt - PLAT_LEEWAY + IA32_EXTRA;
}

void
schedule(void)
{
    word_t action;

    if (ksReprogram) {
        /* wake up any jobs that need it. */
        releaseJobs();
    }

    action = (word_t)ksSchedulerAction;
    if (action == (word_t)SchedulerAction_ChooseNewThread) {
        if (isSchedulable(ksCurThread)) {
            tcbSchedEnqueue(ksCurThread);
        }

        if (CONFIG_NUM_DOMAINS > 1 && ksDomainTime == 0) {
            nextDomain();
        }
        chooseThread();
        ksSchedulerAction = SchedulerAction_ResumeCurrentThread;
    } else if (action != (word_t)SchedulerAction_ResumeCurrentThread) {
        if (isSchedulable(ksCurThread)) {
            tcbSchedEnqueue(ksCurThread);
        }
        /* SwitchToThread */
        switchToThread(ksSchedulerAction);
        ksSchedulerAction = SchedulerAction_ResumeCurrentThread;
    } else if (ksCurThread->tcbSchedContext != ksSchedContext) {
        /* we have changed scheduling context (but not thread) */
        switchToThread(ksCurThread);
    }

    /* reset for next kernel entry */
    assert(ksSchedContext->lastScheduled > 0llu);

    /* reprogram the timer for the next deadline or release time */
    if (ksReprogram) {
        uint64_t nextInterrupt;
        nextInterrupt = getNextInterrupt();

        /* in 2s */
#ifndef CONFIG_DEBUG_BUILD
        setDeadline(nextInterrupt);
#else /* CONFIG_DEBUG_BUILD */
        {
            int reschedule = setDeadline(nextInterrupt);
            if (reschedule) {
                uint64_t time = getCurrentTime();
                printf("Failed to set next IRQ, race: deadline: %llx currentTime: %llx, %d\n",
                       nextInterrupt, time, PLAT_LEEWAY);
                assert(!reschedule);
            }
        }
#endif /* CONFIG_DEBUG_BUILD */
        TRACE("Set next IRQ to %llx\n", nextInterrupt);
        ksReprogram = false;
    }

    /* the ksCurThread is implicitly bound to ksSchedContext --
     * this optimises scheduling context donation */
    ksCurThread->tcbSchedContext = NULL;
    ksSchedContext->tcb = NULL;
}

/*
 * ksCurThread is out of budget, handle appropriately:
 *
 * + if the release time has passed, recharge the thread and rotate on scheduler queue.
 * + otherwise, if it has a temporal fault endpoint, raise an exception
 * + otherwise, postpone until next recharge time.
 *
 */
void
enforceBudget(void)
{

    if (!readyToRelease(ksSchedContext) && raiseTemporalException(ksCurThread)) {
        return;
    }

    if (readyToRelease(ksSchedContext)) {
        /* recharge time has already passed, just apply round robin */
        recharge(ksSchedContext);
        tcbSchedAppend(ksCurThread);
        TRACE("RR'd FP thread %p, prio %d\n", ksCurThread, tcb_prio_get_prio(ksCurThread->tcbPriority));
    } else {
        TRACE("Rate limited FP thread %llx < %llx\n",
              ksSchedContext->nextRelease, ksCurrentTime + PLAT_LEEWAY);
        ksSchedContext->budgetRemaining = 0;
        releaseAdd(ksSchedContext);
    }

}

void
postpone(sched_context_t *sc)
{
    assert(sc != NULL);
    assert(!sched_context_status_get_inReleaseHeap(sc->status));
    sc->nextRelease = ksCurrentTime + sc->period;
    releaseAdd(sc);
}

/* Release heap operations */
static inline void
releaseHeadChanged(void)
{
    ksReprogram = true;
}

void
releaseBehead(void)
{
    sched_context_t *head;
    head = edfDequeue(&ksReleasePQ);

    assert(sched_context_status_get_inReleaseHeap(head->status) == 1);

    sched_context_status_ptr_set_inReleaseHeap(&head->status, 0);
    releaseHeadChanged();
}


void
releaseAdd(sched_context_t *sc)
{
    sched_context_t *head = ksReleasePQ.head;

    sc->priority = sc->nextRelease;
    assert(sched_context_status_get_inReleaseHeap(sc->status) == 0);
    assert(sc != ksReleasePQ.head);
    edfEnqueue(&ksReleasePQ, sc);
    sched_context_status_ptr_set_inReleaseHeap(&sc->status, 1);

    /* new tcb was placed at head of release queue */
    if (head != ksReleasePQ.head) {
        releaseHeadChanged();
    }
    assert(sched_context_status_get_inReleaseHeap(sc->status) == 1);

}

void
releaseRemove(sched_context_t *sc)
{
    assert(sched_context_status_get_inReleaseHeap(sc->status) == 1);

    if (sc == ksReleasePQ.head) {
        releaseBehead();
    } else {
        /* not a new head as we are removing something in the middle of the heap */
        edfRemove(&ksReleasePQ, sc);
        sched_context_status_ptr_set_inReleaseHeap(&sc->status, 0);
    }
}

void
completeCurrentJob(void)
{

    rescheduleRequired();

    /* update parameters */
    ksSchedContext->lastScheduled = ksCurrentTime;
    /* remaining budget is forfeit (job is finished) */
    ksSchedContext->budgetRemaining = 0;
    TRACE("%p Job complete at %llx,next: %llx, dl: %llx\n", ksCurThread, ksCurrentTime, ksSchedContext->nextRelease, ksSchedContext->nextDeadline);

    if (isTimeTriggered(ksSchedContext)) {
        sendAsyncIPC(ksCurThread->boundAsyncEndpoint, 0);
    }

}

void
enqueueJob(sched_context_t *sc, tcb_t *tcb)
{

    if (sched_context_status_get_inReleaseHeap(sc->status)) {
        return;
    }

    resumeSchedContext(sc);
}

/* release pending jobs */
void releaseJobs(void)
{

    sched_context_t *head = ksReleasePQ.head;
    while (head != NULL && readyToRelease(head)) {
        tcb_t *thread = head->tcb;

        assert(thread != NULL);
        assert(isRunnable(thread));

        if (tcb_prio_get_criticality(thread->tcbPriority) < ksCriticality) {
            releaseBehead();
            postpone(head);
        } else {
            /* recharge the budget */
            recharge(head);
            tcbCritEnqueue(head->tcb);

            rescheduleRequired();

            if (thread_state_get_tsType(thread->tcbState) == ThreadState_BlockedOnAsyncEvent) {
                TRACE("Sending async IPC to %x\n", ksReleasePQ.head);
                sendAsyncIPC(thread->boundAsyncEndpoint, 0);
            } else {
                TRACE("Releasing rate limited job %x, budget %llx deadline %llx now %llx\n", thread, head->budgetRemaining,
                      head->nextDeadline, ksCurrentTime);
                releaseBehead();
                tcbSchedAppend(thread);
            }
        }
        head = ksReleasePQ.head;
    }

#ifdef CONFIG_DEBUG
    if (head != NULL) {
        TRACE("Did not release head job, ksCurTime %llx, head->nextRelease %llx\n", ksCurrentTime, head->nextRelease);
    }
#endif /* CONFIG_DEBUG */
}

static inline prio_t
getHighestPrio(void)
{
    word_t dom;
    if (CONFIG_NUM_DOMAINS > 1) {
        dom = ksCurDomain;
    } else {
        dom = 0;
    }

    if (unlikely(ksReadyQueuesL1Bitmap[dom] == 0)) {
        return seL4_InvalidPrio;
    } else {
        uint32_t l1index = 31 - CLZ(ksReadyQueuesL1Bitmap[dom]);
        uint32_t l2index = 31 - CLZ(ksReadyQueuesL2Bitmap[dom][l1index]);
        return l1index_to_prio(l1index) | l2index;
    }

}

bool_t
raiseTemporalException(tcb_t *tcb)
{
    /* if the thread has a temporal exception handler, send a
     * temporal exception */

    lookupCap_ret_t lu_ret;
    bool_t sent = false;

    lu_ret = lookupCap(tcb, tcb->tcbTemporalFaultHandler);
    if (cap_get_capType(lu_ret.cap) != cap_null_cap) {
        current_fault = fault_temporal_new(tcb->tcbSchedContext->data, 0);
        sendFaultIPC(tcb, false);
        sent = true;
    }

    return sent;
}

tcb_t *
getHighestPrioThread(void)
{

    word_t prio;
    word_t dom;
    tcb_t *thread;

    if (CONFIG_NUM_DOMAINS > 1) {
        dom = ksCurDomain;
    } else {
        dom = 0;
    }

    prio = getHighestPrio();

    if (unlikely(prio == seL4_InvalidPrio)) {
        TRACE("chose idle\n");
        return ksIdleThread;
    }

    thread = ksReadyQueues[ready_queues_index(dom, prio)].head;
    TRACE("Chose prio thread %p at prio %d\n", thread, prio);

    assert(thread != NULL);
    assert(isRunnable(thread) || thread == ksIdleThread);
    return thread;
}

void
chooseThread(void)
{
    tcb_t *thread = getHighestPrioThread();

    if (unlikely(thread == ksIdleThread)) {
        switchToIdleThread();
    } else {
        switchToThread(thread);
    }

    return;
}

void
updateBudget(void)
{

    uint64_t consumed = 0llu;

    /* bill the previous thread */
    assert(ksCurrentTime > ksSchedContext->lastScheduled);
    consumed = ksCurrentTime - ksSchedContext->lastScheduled;
    if (consumed > ksSchedContext->budgetRemaining) {
        ksSchedContext->budgetRemaining = 0;
    } else {
        ksSchedContext->budgetRemaining -= consumed;
    }
    ksSchedContext->lastScheduled = ksCurrentTime;

    /* need to reprogram the timer
     * as we have updated the budget */
    ksReprogram = true;
}

void
switchToThread(tcb_t *thread)
{
    assert(thread->tcbSchedContext != NULL);
    assert(tcb_prio_get_criticality(thread->tcbPriority) >= ksCriticality);

    /* we are switching from ksSchedContext to thread->tcbSchedContext */
    if (thread->tcbSchedContext != ksSchedContext) {

        /* we are going to need a new timer interrupt */
        ksReprogram = true;

        if (ksSchedContext->lastScheduled != ksCurrentTime) {
            /* this case means the thread has not already been charged */
            updateBudget();
            assert(ksSchedContext->budgetRemaining > PLAT_LEEWAY);
        }

        /* switch the scheduling context */
        ksSchedContext = thread->tcbSchedContext;

        /* update the billing time for the new scheduling context */
        ksSchedContext->lastScheduled = ksCurrentTime + 1;
        TRACE("Update: KsCurThread %p, KsSchedContext %p\n", thread, ksSchedContext);
    }

    if (thread != ksCurThread) {
        Arch_switchToThread(thread);
    }
    tcbSchedDequeue(thread);
    ksCurThread = thread;
}

void
switchToIdleThread(void)
{
    Arch_switchToIdleThread();
    /* restore sched context binding on previous thread */
    if (ksSchedContext != ksIdleThread->tcbSchedContext) {
        ksCurThread->tcbSchedContext = ksSchedContext;
        ksSchedContext->tcb = ksCurThread;
        ksSchedContext = ksIdleThread->tcbSchedContext;
    }

    ksCurThread = ksIdleThread;
}

void
setDomain(tcb_t *tptr, dom_t dom)
{
    tcbSchedDequeue(tptr);
    tptr->tcbDomain = dom;
    if (isRunnable(tptr)) {
        tcbSchedEnqueue(tptr);
    }
    if (tptr == ksCurThread) {
        rescheduleRequired();
    }
}

void
setPriority(tcb_t *tptr, tcb_prio_t newPrio)
{
    thread_state_t *state = &tptr->tcbState;
    tcb_queue_t queue;
    prio_t oldPrio;
    bool_t critChange;
    bool_t prioChange;

    oldPrio = tcb_prio_get_prio(tptr->tcbPriority);
    critChange = tcb_prio_get_criticality(newPrio) != tcb_prio_get_criticality(tptr->tcbPriority);
    prioChange = oldPrio != tcb_prio_get_prio(newPrio);

    tcbCritDequeue(tptr);
    tcbSchedDequeue(tptr);

    tptr->tcbPriority = newPrio;

    if (isSchedulable(tptr)) {

        tcbCritEnqueue(tptr);
        if (tptr != ksCurThread) {
            tcbSchedEnqueue(tptr);
            if (tcb_prio_get_prio(newPrio) > tcb_prio_get_prio(ksCurThread->tcbPriority)) {
                attemptSwitchTo(tptr, false);
            }
        } else {
            rescheduleRequired();
        }

        if (critChange && tcb_prio_get_criticality(newPrio) < ksCriticality) {
            enforceCriticality(tptr);
        }
    } else if (prioChange) {

        /* if the priority has changed we might need to reorder the ipc endpoint queue
        * that thread may be in */
        switch (thread_state_ptr_get_tsType(state)) {
        case ThreadState_BlockedOnSend:
        case ThreadState_BlockedOnReceive: {
            /* thread is in a sync endpoint queue */
            endpoint_t *epptr = EP_PTR(thread_state_ptr_get_blockingIPCEndpoint(state));

            assert(endpoint_ptr_get_state(epptr) != EPState_Idle);

            queue = ep_ptr_get_queue(epptr);
            queue = tcbEPReorder(tptr, queue, oldPrio);
            ep_ptr_set_queue(epptr, queue);
        }
        break;
        case ThreadState_BlockedOnAsyncEvent: {
            /* thread is in an async endpoint queue */
            async_endpoint_t *aepptr = AEP_PTR(thread_state_ptr_get_blockingIPCEndpoint(state));

            assert(async_endpoint_ptr_get_state(aepptr) == AEPState_Waiting);

            queue = aep_ptr_get_queue(aepptr);
            queue = tcbEPReorder(tptr, queue, oldPrio);
            aep_ptr_set_queue(aepptr, queue);
        }
        break;
        default:
            /* nothing to do */
            break;
        }
    }

}

static void
possibleSwitchTo(tcb_t* target, bool_t onSamePriority, bool_t donate)
{
    prio_t curPrio, targetPrio;
    tcb_t *action;

    assert(isRunnable(target));
    assert(donate || target->tcbSchedContext != NULL);
    assert(donate || target->tcbSchedContext->budgetRemaining >= PLAT_LEEWAY);

    curPrio = tcb_prio_get_prio(ksCurThread->tcbPriority);
    targetPrio = tcb_prio_get_prio(target->tcbPriority);

    action = ksSchedulerAction;

    if (CONFIG_NUM_DOMAINS > 1 && ksCurDomain != target->tcbDomain) {
        assert(target->tcbSchedContext != NULL);
        /* TODO@alyons this is totally broken with the RT kernel */
        tcbSchedEnqueue(target);
    } else {
        /* scheduling context donation */
        if (donate) {
            assert(target->tcbSchedContext == NULL);
            target->tcbSchedContext = ksSchedContext;
            ksSchedContext->tcb = target;
            ksCurThread->tcbSchedContext = NULL;
        }

        if (unlikely(tcb_prio_get_criticality(target->tcbPriority) < ksCriticality)) {
            postpone(target->tcbSchedContext);
        } else if ((targetPrio > curPrio || (targetPrio == curPrio && onSamePriority))
                   && ksSchedulerAction == SchedulerAction_ResumeCurrentThread) {
            ksSchedulerAction = target;
        } else {
            tcbSchedEnqueue(target);
            if (donate) {
                rescheduleRequired();
            }
        }

        if (action != SchedulerAction_ResumeCurrentThread
                && action != SchedulerAction_ChooseNewThread) {
            rescheduleRequired();
        }
    }
}

void
attemptSwitchTo(tcb_t* target, bool_t donate)
{
    possibleSwitchTo(target, true, donate);
}

void
switchIfRequiredTo(tcb_t* target, bool_t donate)
{
    possibleSwitchTo(target, false, donate);
}

void
setThreadState(tcb_t *tptr, _thread_state_t ts)
{
    thread_state_ptr_set_tsType(&tptr->tcbState, ts);
    scheduleTCB(tptr);
}

void
scheduleTCB(tcb_t *tptr)
{
    if (tptr == ksCurThread &&
            ksSchedulerAction == SchedulerAction_ResumeCurrentThread &&
            !isRunnable(tptr)) {
        rescheduleRequired();
    }
}

void
timerTick(void)
{
    TRACE("Tick at %llx\n", ksCurrentTime);

    ackDeadlineIRQ();

    /* release pending jobs */
    ksReprogram = true;

    if (CONFIG_NUM_DOMAINS > 1) {
        ksDomainTime--;
        if (ksDomainTime == 0) {
            rescheduleRequired();
        }
    }
}

void
rescheduleRequired(void)
{
    if (ksSchedulerAction != SchedulerAction_ResumeCurrentThread
            && ksSchedulerAction != SchedulerAction_ChooseNewThread) {
        tcbSchedEnqueue(ksSchedulerAction);
    }
    ksSchedulerAction = SchedulerAction_ChooseNewThread;
}

void
enforceCriticality(tcb_t *tcb)
{
    assert(tcb_prio_get_criticality(tcb->tcbPriority) < ksCriticality);

    if (tcb->tcbSchedContext != NULL &&
            sched_context_status_get_inReleaseHeap(tcb->tcbSchedContext->status)) {
        /* nothing to do */

        return;
    }

    switch (thread_state_get_tsType(tcb->tcbState)) {
    case ThreadState_Inactive:
    case ThreadState_IdleThreadState:
        /* thread is not schedulable, nothing to do */
        break;
    case ThreadState_Running:
    case ThreadState_Restart:
        /* thread is runnable, might be in the scheduler queue */
        if (tcb->tcbSchedContext != NULL) {
            tcbSchedDequeue(tcb);
            postpone(tcb->tcbSchedContext);
            tcbCritDequeue(tcb);
        }
        break;
    case ThreadState_BlockedOnReply: {
        /* the thread may have donated its scheduling context to a server, or chain
         * of servers, that need to be reset as this scheduling context is no longer
         * active.
         */
        sched_context_t *home = tcb->tcbHomeSchedContext;
        if (home && home->tcb != tcb &&
                tcb_prio_get_criticality(home->tcb->tcbPriority) >= ksCriticality) {
            /* try to raise an exception */
            raiseTemporalException(home->tcb);
            /* if it fails, just let the server keep going, the client
             * will be not be switched to unless the criticality level is raised
             */
        }

        break;
    }
    case ThreadState_BlockedOnReceive:
    case ThreadState_BlockedOnSend: {
        /* thread is blocked in an endpoint queue, and should not be serviced -- pull it out
         * and restart the op once the thread is scheduled again */
        endpoint_t *ep;
        tcb_queue_t queue;

        ep = EP_PTR(thread_state_get_blockingIPCEndpoint(tcb->tcbState));
        assert(ep != NULL);

        /* set thread to restart ipc op when criticality raises */
        setThreadState(tcb, ThreadState_Running);

        /* remove from endpoint queue */
        queue = ep_ptr_get_queue(ep);
        queue = tcbEPDequeue(tcb, queue);
        if (queue.head == NULL) {
            endpoint_ptr_set_state(ep, EPState_Idle);
        }
        ep_ptr_set_queue(ep, queue);

        if (tcb->tcbSchedContext != NULL) {
            postpone(tcb->tcbSchedContext);
        }
        tcbCritDequeue(tcb);
        break;
    }
    case ThreadState_BlockedOnAsyncEvent: {
        /* blocked in a queue on an async enpoint: same as sync, pull it out and restart
         * theop once the thread is scheduled again */
        async_endpoint_t *aep;
        tcb_queue_t queue;

        aep = AEP_PTR(thread_state_get_blockingIPCEndpoint(tcb->tcbState));
        assert(aep != NULL);

        /* set thread to restart ipc op when criticality raises */
        setThreadState(tcb, ThreadState_Running);

        /* remove from endpoint queue */
        queue = aep_ptr_get_queue(aep);
        queue = tcbEPDequeue(tcb, queue);
        aep_ptr_set_queue(aep, queue);
        if (queue.head == NULL) {
            async_endpoint_ptr_set_state(aep, AEPState_Idle);
        }

        postpone(tcb->tcbSchedContext);
        tcbCritDequeue(tcb);
    }
    }
}


