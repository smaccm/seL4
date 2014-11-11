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

#ifdef CONFIG_EDF

static inline bool_t PURE
isEDFThread(const tcb_t *thread)
{
    return tcb_prio_get_prio(thread->tcbPriority) == seL4_EDFPrio;
}

#endif /* CONFIG_EDF */
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

static inline bool_t PURE
isRunnable(const tcb_t *thread)
{
    switch (thread_state_get_tsType(thread->tcbState)) {
    case ThreadState_Running:
    case ThreadState_Restart:
        return true;

    default:
        return false;
    }
}

static inline bool_t
isSchedulable(const tcb_t *thread)
{
    /* a thread can only be scheduled if its runnable
     * and has a valid scheduling context
     * that is not currently in the release queue */
    if (unlikely(!isRunnable(thread))) {
        return false;
    } else if (unlikely(thread->tcbSchedContext == NULL)) {
        return false;
    } else if (unlikely(sched_context_status_get_inReleaseHeap(thread->tcbSchedContext->status))) {
        return false;
    }

    return true;
}

BOOT_CODE void
configureIdleThread(tcb_t *tcb)
{
    Arch_configureIdleThread(tcb);
    setThreadState(tcb, ThreadState_IdleThreadState);
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

    if (target->tcbSchedContext) {
        sched_context_purge(target->tcbSchedContext);
    }

}

void
releaseJob(sched_context_t *target)
{
    assert(target->tcb != NULL);

#ifdef CONFIG_EDF
    if (isEDFThread(target->tcb)) {
        deadlineAdd(target);
        return;
    }
#endif
    tcbSchedEnqueue(target->tcb);
    rescheduleRequired();
}

void
resumeSchedContext(sched_context_t *sc)
{

    if (unlikely(sc == NULL || (sc->budget == 0 && sc->period == 0))) {
        return;
    }

    if (tcb_prio_get_criticality(sc->tcb->tcbPriority) < ksCriticality) {
        releaseAdd(sc);
    } else if (sc->nextRelease < ksCurrentTime) {
        /* recharge time has passed, recharge */
        sc->budgetRemaining = sc->budget;
        sc->nextRelease = ksCurrentTime + sc->period;
#ifdef CONFIG_EDF
        sc->nextDeadline = ksCurrentTime + sc->deadline;
#endif /* CONFIG_EDF */
        releaseJob(sc);
    } else if (sc->budgetRemaining > PLAT_LEEWAY) {
        /* still has budget, release time has not passed, just resume */
        releaseJob(sc);
    } else {
        /* release time hasn't passed, no budget left */
        releaseAdd(sc);
    }

}


void
restart(tcb_t *target)
{
    if (isBlocked(target)) {
        ipcCancel(target);
        setThreadState(target, ThreadState_Restart);
        resumeSchedContext(target->tcbSchedContext);
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
doReplyTransfer(tcb_t *sender, tcb_t *receiver, cte_t *slot, bool_t donate, cap_t cap)
{

    assert(thread_state_get_tsType(receiver->tcbState) ==
           ThreadState_BlockedOnReply);

    /* this case occurs when someone else replies on behalf of the original called thread */
    if (!donate && cap_reply_cap_get_schedcontext(cap)) {
        sched_context_t *sc = SC_PTR(cap_reply_cap_get_schedcontext(cap));
        tcb_t *callee = sc->tcb;

        /* restore the scheduling context */
        receiver->tcbSchedContext = sc;
        sc->tcb = receiver;
        if (callee) {
            rescheduleRequired();
            callee->tcbSchedContext = NULL;
        }

        if (receiver->tcbSchedContext->budgetRemaining < PLAT_LEEWAY) {
            if (raiseTemporalException(receiver)) {
                cteDeleteOne(slot);
                return;
            } else {
                resumeSchedContext(receiver->tcbSchedContext);
            }
        }
    }

    if (likely(fault_get_faultType(receiver->tcbFault) == fault_null_fault)) {
        doIPCTransfer(sender, NULL, 0, true, receiver, false);
        cteDeleteOne(slot);
        if (donate ||
                (receiver->tcbSchedContext != NULL
                 && !sched_context_status_get_inReleaseHeap(receiver->tcbSchedContext->status))) {
            attemptSwitchTo(receiver, donate);
        }
        setThreadState(receiver, ThreadState_Running);
    } else {
        bool_t restart;

        cteDeleteOne(slot);
        restart = handleFaultReply(receiver, sender);
        fault_null_fault_ptr_new(&receiver->tcbFault);
        if (restart) {
            setThreadState(receiver, ThreadState_Restart);
            if (donate ||
                    (receiver->tcbSchedContext != NULL
                     && !sched_context_status_get_inReleaseHeap(receiver->tcbSchedContext->status))) {
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
    message_info_t msgInfo = message_info_new(0, 0, 0, 0);
    setRegister(thread, msgInfoRegister,
                wordFromMessageInfo(msgInfo));
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

#ifdef CONFIG_EDF_CBS
    uint64_t nextInterrupt = UINT64_MAX - IA32_EXTRA;

    if (ksCurThread != ksIdleThread) {
        TRACE("deadline irq at %llx, budget %llx\n", ksSchedContext->budgetRemaining + ksCurrentTime,
              ksSchedContext->budgetRemaining);
        /* TODO@alyons avoid overflow here ???*/
        nextInterrupt = ksSchedContext->budgetRemaining + ksCurrentTime;
        assert((nextInterrupt - PLAT_LEEWAY + IA32_EXTRA) > ksCurrentTime);
    }

    assert(ksSchedContext->budgetRemaining > PLAT_LEEWAY);
    assert(nextInterrupt > ksCurrentTime);
    if (ksReleasePQ.head != NULL && ksReleasePQ.head->nextRelease < nextInterrupt) {
        TRACE("Release irq\n");
        nextInterrupt = ksReleasePQ.head->nextRelease;
#ifdef CONFIG_DEBUG_BUILD
        if (!(nextInterrupt - PLAT_LEEWAY + IA32_EXTRA) > ksCurrentTime) {
            printf("Next interrupt %llx - PLATEEWAY + IA32_EXTRA %llx <= ksCurrentTime %llx\n",
                   nextInterrupt, nextInterrupt - PLAT_LEEWAY + IA32_EXTRA, ksCurrentTime);
        }
#endif /* CONFIG_DEBUG_BUILD */
        assert((nextInterrupt - PLAT_LEEWAY + IA32_EXTRA) > ksCurrentTime);
    }

#ifdef CONFIG_DEBUG_BUILD
    if (nextInterrupt <= ksCurrentTime) {
        printf("Next interrupt %llx, ksCurrentTime %llx\n", nextInterrupt, ksCurrentTime);
    }
#endif
    assert(nextInterrupt > ksCurrentTime);
    return nextInterrupt - PLAT_LEEWAY + IA32_EXTRA;
#else
    uint64_t nextInterrupt = UINT64_MAX;
    if (ksReleasePQ.head != NULL) {
        nextInterrupt = ksReleasePQ.head->nextRelease;
    }

    /* set the irq to come in early */
    return nextInterrupt - PLAT_LEEWAY;
#endif /* CONFIG_EDF_CBS */
}

void
schedule(void)
{
    word_t action;


    TRACE_POINT_START;
    if (ksReprogram) {
        /* wake up any jobs that need it. */
        releaseJobs();
    }

    action = (word_t)ksSchedulerAction;
    if (action == (word_t)SchedulerAction_ChooseNewThread) {
        TRACE("Picked new thread\n");
        if (isSchedulable(ksCurThread)) {
            tcbSchedEnqueue(ksCurThread);
        }

        if (CONFIG_NUM_DOMAINS > 1 && ksDomainTime == 0) {
            nextDomain();
        }
        chooseThread();
        ksSchedulerAction = SchedulerAction_ResumeCurrentThread;
    } else if (action != (word_t)SchedulerAction_ResumeCurrentThread) {
        TRACE("Switched to new thread\n");
        if (isSchedulable(ksCurThread)) {
            tcbSchedEnqueue(ksCurThread);
        }
        /* SwitchToThread */
        switchToThread(ksSchedulerAction);
        ksSchedulerAction = SchedulerAction_ResumeCurrentThread;
    } else if (ksCurThread->tcbSchedContext != ksSchedContext) {
        TRACE("Switched to new sc\n");
        /* we have changed scheduling context (but not thread) */
        switchToThread(ksCurThread);
    } else {
        TRACE("Nothing\n");
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

    TRACE_POINT_STOP;
}


/* current thread is out of budget, handle appropriately */
void
enforceBudget(void)
{

    assert(ksSchedContext->budgetRemaining <= PLAT_LEEWAY);

#ifdef CONFIG_FAIL_CBS
    fail("budget enforcement triggered");
#endif /* CONFIG_FAIL_CBS */

    if (raiseTemporalException(ksCurThread)) {
        return;
    }

    /* otherwise rate limit */
#ifdef CONFIG_EDF
    if (isEDFThread(ksCurThread)) {
        ksSchedContext->budgetRemaining = ksSchedContext->budget;
        if (sched_context_status_get_cbs(ksSchedContext->status) == seL4_HardCBS) {
            deadlineRemove(ksSchedContext);
            TRACE("Rate limited %x(%x)\n", ksSchedContext, ksCurThread);
            ksSchedContext->nextRelease = ksCurrentTime + ksSchedContext->period;
            ksSchedContext->nextDeadline = ksSchedContext->nextRelease + ksSchedContext->deadline;
            releaseAdd(ksSchedContext);
        } else {
            TRACE("Post-poned %x\n", ksCurThread);
            ksSchedContext->nextDeadline = ksCurrentTime + ksSchedContext->deadline;
            deadlinePostpone();
        }
        return;
    }
#endif /* CONFIG_EDF */

    if (ksSchedContext->nextRelease < ksCurrentTime + PLAT_LEEWAY) {
        /* recharge time has already passed, just apply round robin */
        ksSchedContext->budgetRemaining = ksSchedContext->budget;
        ksSchedContext->nextRelease = ksCurrentTime + ksSchedContext->period;
        tcbSchedAppend(ksCurThread);
        TRACE("RR'd FP thread %p, prio %d\n", ksCurThread, ksCurThread->tcbPriority);
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
void
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

#ifdef CONFIG_EDF
/* deadline heap operations */

void
deadlineHeadChanged(void)
{
    ksReprogram = true;
    rescheduleRequired();
}

void
deadlineAdd(sched_context_t *sc)
{
    assert(sched_context_status_get_inDeadlineHeap(sc->status) == 0);
    assert(sc->tcb != NULL);

    sc->priority = sc->nextDeadline;
    assert(sc != ksDeadlinePQ.head);
    edfEnqueue(&ksDeadlinePQ, sc);
    sched_context_status_ptr_set_inDeadlineHeap(&sc->status, 1);

    if (ksDeadlinePQ.head == sc) {
        deadlineHeadChanged();
    }
}

void
deadlineBehead(void)
{
    sched_context_status_ptr_set_inDeadlineHeap(&ksDeadlinePQ.head->status, 0);
    edfDequeue(&ksDeadlinePQ);
    deadlineHeadChanged();
}

void
deadlineRemove(sched_context_t *sc)
{
    assert(sched_context_status_get_inDeadlineHeap(sc->status) == 1);

    if (ksDeadlinePQ.head == sc) {
        deadlineBehead();
    } else {
        /* not a new head as we are removing something in the middle of the heap */
        edfRemove(&ksDeadlinePQ, sc);
        sched_context_status_ptr_set_inDeadlineHeap(&sc->status, 0);
    }
}

void
deadlinePostpone(void)
{
    sched_context_t *head = edfDequeue(&ksDeadlinePQ);
    assert(ksDeadlinePQ.head != head);
    assert(sched_context_status_get_inDeadlineHeap(head->status) == 1);

    head->priority = head->nextDeadline;
    assert(head != ksDeadlinePQ.head);
    edfEnqueue(&ksDeadlinePQ, head);
    /* this is called in the context of CBS enforcement,
     * so we are already choosing a new thread and don't
     * need to call deadlineHeadChanged()
     */
}
#endif /* CONFIG_EDF */

/* complete the head of the EDF queue and set up for rerelease */
void
completeCurrentJob(void)
{


#ifdef CONFIG_EDF
    if (isEDFThread(ksCurThread)) {
        deadlineBehead();
    } else {
#endif /* CONFIG_EDF */
        rescheduleRequired();
#ifdef CONFIG_EDF
    }
#endif /* CONFIG_EDF */


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
    while (head != NULL && head->nextRelease <= ksCurrentTime + PLAT_LEEWAY) {
        /* released too late */
        //if (ksCurrentTime > (head->nextRelease)) {
        //    printf("WARN: task released %llx too late\n", ksCurrentTime - head->nextRelease);
        //}

        //assert(ksCurrentTime < head->nextRelease + PLAT_LEEWAY);

        tcb_t *thread = head->tcb;
        assert(thread != NULL);

        rescheduleRequired();

        if (tcb_prio_get_criticality(thread->tcbPriority) < ksCriticality) {
            releaseBehead();
            postpone(head);
        } else {
            /* recharge the budget */
            head->budgetRemaining = head->budget;
            head->nextRelease = ksCurrentTime + head->period;

            if (thread_state_get_tsType(thread->tcbState) == ThreadState_BlockedOnAsyncEvent) {
                TRACE("Sending async IPC to %x\n", ksReleasePQ.head);
                sendAsyncIPC(thread->boundAsyncEndpoint, 0);
            } else {
                TRACE("Releasing CBS job %x, budget %llx deadline %llx now %llx\n", thread, head->budgetRemaining,
                      head->nextDeadline, ksCurrentTime);
                releaseBehead();

#ifdef CONFIG_EDF
                if (isEDFThread(thread)) {
#ifdef CONFIG_EDF_CBS
                    //TODO this is wrong, as we can't incorporate the deadline/period ratio without 64bit division.
                    if (head->budgetRemaining > ((head->nextDeadline - ksCurrentTime))) {
                        head->nextDeadline = ksCurrentTime + head->deadline;
                    }
#endif /* CONFIG_EDF_CBS */
                    assert(head != ksDeadlinePQ.head);
                    deadlineAdd(head);
                    setThreadState(thread, ThreadState_Running);
                } else {
#endif /* CONFIG_EDF */
                    setThreadState(thread, ThreadState_Running);
                    tcbSchedAppend(thread);
#ifdef CONFIG_EDF
                }
#endif /* CONFIG_EDF */
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

inline prio_t
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

#ifdef CONFIG_EDF
    if (ksDeadlinePQ.head == NULL) {
        /* don't pick edf if there are no runnable edf threads */
        removeFromBitmap(dom, seL4_EDFPrio);
    }
#endif /* CONFIG_EDF */
    prio = getHighestPrio();

    addToBitmap(dom, seL4_EDFPrio);

    if (unlikely(prio == seL4_InvalidPrio)) {
        TRACE("chose idle\n");
        return ksIdleThread;
    }
#ifdef CONFIG_EDF
    if (prio == seL4_EDFPrio) {
        if (ksDeadlinePQ.head == ksSchedContext) {
            thread = ksCurThread;
        } else {
            thread = ksDeadlinePQ.head->tcb;
        }
        TRACE("Chose EDF thread %p, budget: %llx\n", thread, ksDeadlinePQ.head->budgetRemaining);
    } else {
#endif /* CONFIG_EDF */
        thread = ksReadyQueues[ready_queues_index(dom, prio)].head;
        TRACE("Chose prio thread %p at prio %d\n", thread, prio);
#ifdef CONFIG_EDF
    }
#endif /* CONFIG_EDF */

    assert(thread != NULL);
    assert(isRunnable(thread) || thread == ksIdleThread);
    return thread;
}

tcb_t *
pickThread(void)
{
    tcb_t *thread = NULL;
    tcb_t *home;

    while (thread == NULL) {

        thread = getHighestPrioThread();
        home = thread->tcbSchedContext->home;

        if (unlikely(home != thread && tcb_prio_get_criticality(home->tcbPriority) < ksCriticality)) {
            /* this thread isn't eligible to run but its blocking a resource,
             * attempt to send a temporal fault. */
            if (raiseTemporalException(thread)) {
                thread = NULL;
            }
        } else if (unlikely(tcb_prio_get_criticality(thread->tcbPriority) < ksCriticality)) {
            /* postpone */
            tcbSchedDequeue(thread);
            postpone(thread->tcbSchedContext);
            thread = NULL;
        }
    }

    assert(tcb_prio_get_criticality(thread->tcbPriority) >= ksCriticality);
    assert(thread != NULL);
    return thread;
}


void
chooseThread(void)
{
    tcb_t *thread = pickThread();

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
    consumed = ksCurrentTime - ksSchedContext->lastScheduled;
    if (consumed > ksSchedContext->budgetRemaining) {
        ksSchedContext->budgetRemaining = 0;
    } else {
        ksSchedContext->budgetRemaining -= consumed;
    }
    ksSchedContext->lastScheduled = ksCurrentTime;
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
            /* this case means the thread has already been charged */
            updateBudget();

            if (ksSchedContext->budgetRemaining <= PLAT_LEEWAY) {
                tcb_t *woken;

                enforceBudget();
                /* pick a new thread, the in case enforceBudget() woke a higher prio
                 * exception handler */
                woken = pickThread();
                if (tcb_prio_get_prio(woken->tcbPriority) > tcb_prio_get_prio(thread->tcbPriority)) {
                    thread = woken;
                }

                if (thread == ksIdleThread) {
                    switchToIdleThread();
                    return;
                }
            }
        }

        /* switch the scheduling context */
        ksSchedContext = thread->tcbSchedContext;

        /* update the billing time for the new scheduling context */
        ksSchedContext->lastScheduled = ksCurrentTime + 1;
        TRACE("Update: KsCurThread %p, KsSchedContext %p\n", thread, ksSchedContext);
    } else {
        TRACE("No Switching Scheduling context, %p %p\n", thread->tcbSchedContext, ksSchedContext);
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
setPriority(tcb_t *tptr, tcb_prio_t prio)
{
    thread_state_t *state = &tptr->tcbState;
    tcb_queue_t queue;
    prio_t oldPrio;

    oldPrio = tcb_prio_get_prio(tptr->tcbPriority);
    if (tcb_prio_get_prio(prio) == oldPrio) {
        /* just changing max prio, nothing to see here */
        tptr->tcbPriority = prio;
        return;
    }

    /* otherwise we are changing priority, lots to do */
    tcbSchedDequeue(tptr);

    tptr->tcbPriority = prio;

    if (isSchedulable(tptr)) {
        tcbSchedEnqueue(tptr);
    }

    if (tptr == ksCurThread) {
        rescheduleRequired();
    }

    /* need to reorder ipc endpoint queue that thread may be in */
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

    return;
}

static void
possibleSwitchTo(tcb_t* target, bool_t onSamePriority, bool_t donate)
{
    prio_t curPrio, targetPrio;
    tcb_t *action;

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

        if ((targetPrio > curPrio || (targetPrio == curPrio && onSamePriority))
                && ksSchedulerAction == SchedulerAction_ResumeCurrentThread
                && tcb_prio_get_criticality(target->tcbPriority) >= ksCriticality) {
            ksSchedulerAction = target;
        } else {
            tcbSchedEnqueue(target);
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

    /* we will need to reprogram the timer if the interrupt came in early */
    ksReprogram = true;

    updateBudget();
    if (ksSchedContext->budgetRemaining <= PLAT_LEEWAY) {
        enforceBudget();
        rescheduleRequired();
    }

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




