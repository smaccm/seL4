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
#include <types.h>
#include <api/failures.h>
#include <api/invocation.h>
#include <api/syscall.h>
#include <machine/io.h>
#include <object/structures.h>
#include <object/objecttype.h>
#include <object/cnode.h>
#include <object/tcb.h>
#include <kernel/cspace.h>
#include <kernel/thread.h>
#include <kernel/vspace.h>
#include <model/statedata.h>
#include <util.h>

#define TCB_PRIO_NULL ((tcb_prio_t) { .words[0] = 0 })

/* Add TCB to the head of a scheduler queue */
void
tcbSchedEnqueue(tcb_t *tcb)
{
    assert(tcb->tcbSchedContext != NULL);
    assert(tcb->tcbSchedContext->budgetRemaining > PLAT_LEEWAY);
    assert(!sched_context_status_get_inReleaseHeap(tcb->tcbSchedContext->status));

    if (!thread_state_get_tcbQueued(tcb->tcbState)) {
        tcb_queue_t queue;
        UNUSED dom_t dom;
        prio_t prio;
        unsigned int idx;

        dom = tcb->tcbDomain;
        prio = tcb_prio_get_prio(tcb->tcbPriority);
        idx = ready_queues_index(dom, prio);
        queue = ksReadyQueues[idx];

        if (!queue.end) { /* Empty list */
            queue.end = tcb;
            addToBitmap(dom, prio);
        } else {
            queue.head->tcbSchedPrev = tcb;
        }
        tcb->tcbSchedPrev = NULL;
        tcb->tcbSchedNext = queue.head;
        queue.head = tcb;

        ksReadyQueues[idx] = queue;

        thread_state_ptr_set_tcbQueued(&tcb->tcbState, true);
    }
}

/* Add TCB to the end of a scheduler queue */
void
tcbSchedAppend(tcb_t *tcb)
{
    if (!thread_state_get_tcbQueued(tcb->tcbState)) {
        tcb_queue_t queue;
        UNUSED dom_t dom;
        prio_t prio;
        unsigned int idx;

        dom = tcb->tcbDomain;
        prio = tcb_prio_get_prio(tcb->tcbPriority);
        idx = ready_queues_index(dom, prio);
        queue = ksReadyQueues[idx];

        if (!queue.head) { /* Empty list */
            queue.head = tcb;
            addToBitmap(dom, prio);
        } else {
            queue.end->tcbSchedNext = tcb;
        }
        tcb->tcbSchedPrev = queue.end;
        tcb->tcbSchedNext = NULL;
        queue.end = tcb;

        ksReadyQueues[idx] = queue;

        thread_state_ptr_set_tcbQueued(&tcb->tcbState, true);
    }
}

/* Remove TCB from a scheduler queue */
void
tcbSchedDequeue(tcb_t *tcb)
{
    if (thread_state_get_tcbQueued(tcb->tcbState)) {
        tcb_queue_t queue;
        UNUSED dom_t dom;
        prio_t prio;
        unsigned int idx;

        dom = tcb->tcbDomain;
        prio = tcb_prio_get_prio(tcb->tcbPriority);
        idx = ready_queues_index(dom, prio);
        queue = ksReadyQueues[idx];

        if (tcb->tcbSchedPrev) {
            tcb->tcbSchedPrev->tcbSchedNext = tcb->tcbSchedNext;
        } else {
            queue.head = tcb->tcbSchedNext;
            if (likely(!tcb->tcbSchedNext)) {
                removeFromBitmap(dom, prio);
            }
        }

        if (tcb->tcbSchedNext) {
            tcb->tcbSchedNext->tcbSchedPrev = tcb->tcbSchedPrev;
        } else {
            queue.end = tcb->tcbSchedPrev;
        }

        ksReadyQueues[idx] = queue;

        thread_state_ptr_set_tcbQueued(&tcb->tcbState, false);
    }
}

/* reorder a tcb in an ipc endpoint queue */
tcb_queue_t
tcbEPReorder(tcb_t *tcb, tcb_queue_t queue, prio_t oldPrio)
{

    prio_t newPrio = tcb_prio_get_prio(tcb->tcbPriority);

    /* nothing to do, prio didn't change */
    if (newPrio == oldPrio) {
        return queue;
    }

    if (newPrio > oldPrio) {
        /* move tcb up in the queue */
        tcb_t *prev = tcb->tcbEPPrev;

        if (prev == NULL ||
                tcb_prio_get_prio(tcb->tcbPriority) < tcb_prio_get_prio(prev->tcbPriority)) {
            /* nothing to do, tcb is at head of list or in the correct place */
            return queue;
        }

        /* take the tcb out (this will fix the end of the queue if required) */
        queue = tcbEPDequeue(tcb, queue);

        /* now find the next place to put it based on the old place */
        while (prev != NULL &&
                tcb_prio_get_prio(prev->tcbPriority) < newPrio) {
            prev = prev->tcbEPPrev;
        }

        /* not possible */
        assert(prev != queue.end);

        if (prev == NULL) {
            /* tcb goes to the head */
            tcb->tcbEPNext = queue.head;
            tcb->tcbEPPrev = NULL;
            queue.head->tcbEPPrev = tcb;
            queue.head = tcb;
        } else {
            /* tcb goes after prev */
            tcb->tcbEPNext = prev->tcbEPNext;
            tcb->tcbEPPrev = prev;
            tcb->tcbEPNext->tcbEPPrev = tcb;
            prev->tcbEPNext = tcb;
        }

    } else { /* new_prio < old_prio */
        /* move tcb down in queue */
        tcb_t *next = tcb->tcbEPNext;

        if (next == NULL ||
                tcb_prio_get_prio(tcb->tcbPriority) >= tcb_prio_get_prio(next->tcbPriority)) {
            /* nothing to do, tcb is at tail of list or in the correct place */
            return queue;
        }

        /* take the tcb out (this will fix up the head of the queue if required) */
        queue = tcbEPDequeue(tcb, queue);

        while (next != NULL &&
                tcb_prio_get_prio(next->tcbPriority) >= newPrio) {
            next = next->tcbEPNext;
        }

        /* not possible */
        assert(next != queue.head);

        if (next == NULL) {
            /* tcb goes to the tail */
            tcb->tcbEPPrev = queue.end;
            tcb->tcbEPNext = NULL;
            queue.end->tcbEPNext = tcb;
            queue.end = tcb;
        } else {
            /* tcb goes before next */
            tcb->tcbEPNext = next;
            tcb->tcbEPPrev = next->tcbEPPrev;
            next->tcbEPPrev->tcbEPNext = tcb;
            next->tcbEPPrev = tcb;
        }
    }

    return queue;
}

/* Add TCB to the ordered endpoint queue */
tcb_queue_t
tcbEPAppend(tcb_t *tcb, tcb_queue_t queue)
{
    if (!queue.head) { /* Empty list */
        queue.head = tcb;
        tcb->tcbEPPrev = NULL;
        tcb->tcbEPNext = NULL;
        queue.end = tcb;
    } else {
        /* insert ordered */
        tcb_t *prev = NULL;
        tcb_t *current = queue.head;

        /* find a place to put the tcb */
        while (tcb_prio_get_prio(tcb->tcbPriority) <=
                tcb_prio_get_prio(current->tcbPriority)) {
            prev = current;
            current = current->tcbEPNext;

            /* we hit the end */
            if (current == NULL) {
                break;
            }
        }

        /* there is at least one other tcb in the queue
         * (since queue.head was not null) so we are either
         * inserting at head, tail or middle */
        if (prev == NULL) {
            /* insert at head */
            queue.head = tcb;
            tcb->tcbEPNext = current;
            tcb->tcbEPPrev = NULL;
            current->tcbEPPrev = tcb;
        } else if (current == NULL) {
            /* insert at end */
            prev->tcbEPNext = tcb;
            queue.end = tcb;
            tcb->tcbEPPrev = prev;
            tcb->tcbEPNext = NULL;
        } else {
            /* insert between prev and current */
            prev->tcbEPNext = tcb;
            current->tcbEPPrev = tcb;
            tcb->tcbEPPrev = prev;
            tcb->tcbEPNext = current;
        }
    }

    return queue;
}

/* Remove TCB from an endpoint queue */
tcb_queue_t
tcbEPDequeue(tcb_t *tcb, tcb_queue_t queue)
{
    if (tcb->tcbEPPrev) {
        tcb->tcbEPPrev->tcbEPNext = tcb->tcbEPNext;
    } else {
        queue.head = tcb->tcbEPNext;
    }

    if (tcb->tcbEPNext) {
        tcb->tcbEPNext->tcbEPPrev = tcb->tcbEPPrev;
    } else {
        queue.end = tcb->tcbEPPrev;
    }

    return queue;
}

cptr_t PURE
getExtraCPtr(word_t *bufferPtr, unsigned int i)
{
    return (cptr_t)bufferPtr[seL4_MsgMaxLength + 2 + i];
}

void
setExtraBadge(word_t *bufferPtr, word_t badge,
              unsigned int i)
{
    bufferPtr[seL4_MsgMaxLength + 2 + i] = badge;
}

void
setupCallerCap(tcb_t *sender, tcb_t *receiver, sched_context_t *donated)
{
    cte_t *replySlot, *callerSlot;
    cap_t masterCap UNUSED, callerCap UNUSED;

    setThreadState(sender, ThreadState_BlockedOnReply);
    replySlot = TCB_PTR_CTE_PTR(sender, tcbReply);
    masterCap = replySlot->cap;
    /* Haskell error: "Sender must have a valid master reply cap" */
    assert(cap_get_capType(masterCap) == cap_reply_cap);
    assert(cap_reply_cap_get_capReplyMaster(masterCap));
    assert(TCB_PTR(cap_reply_cap_get_capTCBPtr(masterCap)) == sender);
    callerSlot = TCB_PTR_CTE_PTR(receiver, tcbCaller);
    callerCap = callerSlot->cap;
    /* Haskell error: "Caller cap must not already exist" */
    assert(cap_get_capType(callerCap) == cap_null_cap);
    cteInsert(cap_reply_cap_new(false, TCB_REF(sender), SC_REF(donated)),
              replySlot, callerSlot);
}

void
deleteCallerCap(tcb_t *receiver)
{
    cte_t *callerSlot;

    callerSlot = TCB_PTR_CTE_PTR(receiver, tcbCaller);
    cteDeleteOne(callerSlot);
}

extra_caps_t current_extra_caps;

exception_t
lookupExtraCaps(tcb_t* thread, word_t *bufferPtr, message_info_t info)
{
    lookupSlot_raw_ret_t lu_ret;
    cptr_t cptr;
    unsigned int i, length;

    if (!bufferPtr) {
        current_extra_caps.excaprefs[0] = NULL;
        return EXCEPTION_NONE;
    }

    length = message_info_get_msgExtraCaps(info);

    for (i = 0; i < length; i++) {
        cptr = getExtraCPtr(bufferPtr, i);

        lu_ret = lookupSlot(thread, cptr);
        if (lu_ret.status != EXCEPTION_NONE) {
            current_fault = fault_cap_fault_new(cptr, false);
            return lu_ret.status;
        }

        current_extra_caps.excaprefs[i] = lu_ret.slot;
    }
    if (i < seL4_MsgMaxExtraCaps) {
        current_extra_caps.excaprefs[i] = NULL;
    }

    return EXCEPTION_NONE;
}

/* Copy IPC MRs from one thread to another */
unsigned int
copyMRs(tcb_t *sender, word_t *sendBuf, tcb_t *receiver,
        word_t *recvBuf, unsigned int n)
{
    unsigned int i;

    /* Copy inline words */
    for (i = 0; i < n && i < n_msgRegisters; i++) {
        setRegister(receiver, msgRegisters[i],
                    getRegister(sender, msgRegisters[i]));
    }

    if (!recvBuf || !sendBuf) {
        return i;
    }

    /* Copy out-of-line words */
    for (; i < n; i++) {
        recvBuf[i + 1] = sendBuf[i + 1];
    }

    return i;
}

/* The following functions sit in the syscall error monad, but include the
 * exception cases for the preemptible bottom end, as they call the invoke
 * functions directly.  This is a significant deviation from the Haskell
 * spec. */
exception_t
decodeTCBInvocation(word_t label, unsigned int length, cap_t cap,
                    cte_t* slot, extra_caps_t extraCaps, bool_t call,
                    word_t *buffer)
{
    switch (label) {
    case TCBReadRegisters:
        /* Second level of decoding */
        return decodeReadRegisters(cap, length, call, buffer);

    case TCBWriteRegisters:
        return decodeWriteRegisters(cap, length, buffer);

    case TCBCopyRegisters:
        return decodeCopyRegisters(cap, length, extraCaps, buffer);

    case TCBSuspend:
        /* Jump straight to the invoke */
        setThreadState(ksCurThread, ThreadState_Restart);
        return invokeTCB_Suspend(
                   TCB_PTR(cap_thread_cap_get_capTCBPtr(cap)));

    case TCBResume:
        setThreadState(ksCurThread, ThreadState_Restart);
        return invokeTCB_Resume(
                   TCB_PTR(cap_thread_cap_get_capTCBPtr(cap)));

    case TCBConfigure:
        return decodeTCBConfigure(cap, length, slot, extraCaps, buffer);

    case TCBSetPriority:
        return decodeSetPriority(cap, length, buffer);

    case TCBSetMaxPriority:
        return decodeSetMaxPriority(cap, length, buffer);

    case TCBSetIPCBuffer:
        return decodeSetIPCBuffer(cap, length, slot, extraCaps, buffer);

    case TCBSetSpace:
        return decodeSetSpace(cap, length, slot, extraCaps, buffer);

    case TCBBindAEP:
        return decodeBindAEP(cap, extraCaps);

    case TCBUnbindAEP:
        return decodeUnbindAEP(cap);

    case TCBSetSchedContext:
        return decodeSetSchedContext(cap, extraCaps);

    case TCBClearSchedContext:
        return decodeClearSchedContext(cap);

    default:
        /* Haskell: "throw IllegalOperation" */
        userError("TCB: Illegal operation.");
        current_syscall_error.type = seL4_IllegalOperation;
        return EXCEPTION_SYSCALL_ERROR;
    }
}

enum CopyRegistersFlags {
    CopyRegisters_suspendSource = 0,
    CopyRegisters_resumeTarget = 1,
    CopyRegisters_transferFrame = 2,
    CopyRegisters_transferInteger = 3
};

exception_t
decodeCopyRegisters(cap_t cap, unsigned int length,
                    extra_caps_t extraCaps, word_t *buffer)
{
    word_t transferArch;
    tcb_t *srcTCB;
    cap_t source_cap;
    word_t flags;

    if (length < 1 || extraCaps.excaprefs[0] == NULL) {
        userError("TCB CopyRegisters: Truncated message.");
        current_syscall_error.type = seL4_TruncatedMessage;
        return EXCEPTION_SYSCALL_ERROR;
    }

    flags = getSyscallArg(0, buffer);

    transferArch = Arch_decodeTransfer(flags >> 8);

    source_cap = extraCaps.excaprefs[0]->cap;

    if (cap_get_capType(source_cap) == cap_thread_cap) {
        srcTCB = TCB_PTR(cap_thread_cap_get_capTCBPtr(source_cap));
    } else {
        userError("TCB CopyRegisters: Invalid source TCB.");
        current_syscall_error.type = seL4_InvalidCapability;
        current_syscall_error.invalidCapNumber = 1;
        return EXCEPTION_SYSCALL_ERROR;
    }

    setThreadState(ksCurThread, ThreadState_Restart);
    return invokeTCB_CopyRegisters(
               TCB_PTR(cap_thread_cap_get_capTCBPtr(cap)), srcTCB,
               flags & BIT(CopyRegisters_suspendSource),
               flags & BIT(CopyRegisters_resumeTarget),
               flags & BIT(CopyRegisters_transferFrame),
               flags & BIT(CopyRegisters_transferInteger),
               transferArch);

}

enum ReadRegistersFlags {
    ReadRegisters_suspend = 0
};

exception_t
decodeReadRegisters(cap_t cap, unsigned int length, bool_t call,
                    word_t *buffer)
{
    word_t transferArch, flags, n;
    tcb_t* thread;

    if (length < 2) {
        userError("TCB ReadRegisters: Truncated message.");
        current_syscall_error.type = seL4_TruncatedMessage;
        return EXCEPTION_SYSCALL_ERROR;
    }

    flags = getSyscallArg(0, buffer);
    n     = getSyscallArg(1, buffer);

    if (n < 1 || n > n_frameRegisters + n_gpRegisters) {
        userError("TCB ReadRegisters: Attempted to read an invalid number of registers (%d).",
                  (int)n);
        current_syscall_error.type = seL4_RangeError;
        current_syscall_error.rangeErrorMin = 1;
        current_syscall_error.rangeErrorMax = n_frameRegisters +
                                              n_gpRegisters;
        return EXCEPTION_SYSCALL_ERROR;
    }

    transferArch = Arch_decodeTransfer(flags >> 8);

    thread = TCB_PTR(cap_thread_cap_get_capTCBPtr(cap));
    if (thread == ksCurThread) {
        userError("TCB ReadRegisters: Attempted to read our own registers.");
        current_syscall_error.type = seL4_IllegalOperation;
        return EXCEPTION_SYSCALL_ERROR;
    }

    setThreadState(ksCurThread, ThreadState_Restart);
    return invokeTCB_ReadRegisters(
               TCB_PTR(cap_thread_cap_get_capTCBPtr(cap)),
               flags & BIT(ReadRegisters_suspend),
               n, transferArch, call);
}

enum WriteRegistersFlags {
    WriteRegisters_resume = 0
};

exception_t
decodeWriteRegisters(cap_t cap, unsigned int length, word_t *buffer)
{
    word_t flags, w;
    word_t transferArch;
    tcb_t* thread;

    if (length < 2) {
        userError("TCB WriteRegisters: Truncated message.");
        current_syscall_error.type = seL4_TruncatedMessage;
        return EXCEPTION_SYSCALL_ERROR;
    }

    flags = getSyscallArg(0, buffer);
    w     = getSyscallArg(1, buffer);

    if (length - 2 < w) {
        userError("TCB WriteRegisters: Message too short for requested write size (%d/%d).",
                  (int)(length - 2), (int)w);
        current_syscall_error.type = seL4_TruncatedMessage;
        return EXCEPTION_SYSCALL_ERROR;
    }

    transferArch = Arch_decodeTransfer(flags >> 8);

    thread = TCB_PTR(cap_thread_cap_get_capTCBPtr(cap));
    if (thread == ksCurThread) {
        userError("TCB WriteRegisters: Attempted to write our own registers.");
        current_syscall_error.type = seL4_IllegalOperation;
        return EXCEPTION_SYSCALL_ERROR;
    }

    setThreadState(ksCurThread, ThreadState_Restart);
    return invokeTCB_WriteRegisters(thread,
                                    flags & BIT(WriteRegisters_resume),
                                    w, transferArch, buffer);
}

/* SetPriority, SetIPCParams and SetSpace are all
 * specialisations of TCBConfigure. */

exception_t
decodeTCBConfigure(cap_t cap, unsigned int length, cte_t* slot,
                   extra_caps_t rootCaps, word_t *buffer)
{
    cte_t *bufferSlot, *cRootSlot, *vRootSlot;
    cap_t bufferCap, cRootCap, vRootCap;
    cap_t scCap;
    sched_context_t *sched_context;
    deriveCap_ret_t dc_ret;
    cptr_t faultEP, temporalFEP;
    word_t cRootData, vRootData, bufferAddr;
    tcb_prio_t prio;

    if (length < 7 || rootCaps.excaprefs[0] == NULL
            || rootCaps.excaprefs[1] == NULL
            || rootCaps.excaprefs[2] == NULL
       ) {
        userError("TCB Configure: Truncated message.");
        current_syscall_error.type = seL4_TruncatedMessage;
        return EXCEPTION_SYSCALL_ERROR;
    }

    faultEP    = getSyscallArg(0, buffer);
    prio.words[0] = getSyscallArg(1, buffer);
    scCap = lookupSlot(ksCurThread, getSyscallArg(2, buffer)).slot->cap;
    temporalFEP = getSyscallArg(6, buffer);

    /* sched context cap should either be null or a valid sc cap */
    if (likely(cap_get_capType(scCap) == cap_sched_context_cap)) {
        sched_context = SCHED_CONTEXT_PTR(cap_sched_context_cap_get_capPtr(scCap));
    } else if (cap_get_capType(scCap) == cap_null_cap) {
        sched_context = NULL;
    } else {
        userError("TCB Configure: sched_context_cap is invalid.");
        current_syscall_error.type = seL4_IllegalOperation;
        return EXCEPTION_SYSCALL_ERROR;
    }

    cRootData  = getSyscallArg(3, buffer);
    vRootData  = getSyscallArg(4, buffer);
    bufferAddr = getSyscallArg(5, buffer);

    cRootSlot  = rootCaps.excaprefs[0];
    cRootCap   = rootCaps.excaprefs[0]->cap;
    vRootSlot  = rootCaps.excaprefs[1];
    vRootCap   = rootCaps.excaprefs[1]->cap;
    bufferSlot = rootCaps.excaprefs[2];
    bufferCap  = rootCaps.excaprefs[2]->cap;

    if (tcb_prio_get_prio(prio) > tcb_prio_get_maxPrio(ksCurThread->tcbPriority)) {
        userError("TCB Configure: Requested priority %d too high (max %d).",
                  (int) tcb_prio_get_prio(prio),
                  (int) tcb_prio_get_maxPrio(ksCurThread->tcbPriority));
        current_syscall_error.type = seL4_IllegalOperation;
        return EXCEPTION_SYSCALL_ERROR;
    }

    if (tcb_prio_get_maxPrio(prio) > tcb_prio_get_maxPrio(ksCurThread->tcbPriority)) {
        userError("TCB Configure: Requested max prio %d too high (max %d).",
                  (int) tcb_prio_get_maxPrio(prio),
                  (int) tcb_prio_get_maxPrio(ksCurThread->tcbPriority));
        current_syscall_error.type = seL4_IllegalOperation;
        return EXCEPTION_SYSCALL_ERROR;
    }

    if (tcb_prio_get_criticality(prio) > tcb_prio_get_maxCriticality(ksCurThread->tcbPriority)) {
        userError("TCB Configure: Requested criticality %d too high (max %d).",
                  (int) tcb_prio_get_criticality(prio),
                  (int) tcb_prio_get_maxCriticality(ksCurThread->tcbPriority));
        current_syscall_error.type = seL4_IllegalOperation;
        return EXCEPTION_SYSCALL_ERROR;
    }

    if (tcb_prio_get_maxCriticality(prio) > tcb_prio_get_maxCriticality(ksCurThread->tcbPriority)) {
        userError("TCB Configure: Requested max criticality %d too high (max %d).",
                  (int) tcb_prio_get_maxCriticality(prio),
                  (int) tcb_prio_get_maxCriticality(ksCurThread->tcbPriority));
        current_syscall_error.type = seL4_IllegalOperation;
        return EXCEPTION_SYSCALL_ERROR;
    }

    if (bufferAddr == 0) {
        bufferSlot = NULL;
    } else {
        exception_t e;

        dc_ret = deriveCap(bufferSlot, bufferCap);
        if (dc_ret.status != EXCEPTION_NONE) {
            return dc_ret.status;
        }
        bufferCap = dc_ret.cap;
        e = checkValidIPCBuffer(bufferAddr, bufferCap);
        if (e != EXCEPTION_NONE) {
            return e;
        }
    }

    if (slotCapLongRunningDelete(
                TCB_PTR_CTE_PTR(cap_thread_cap_get_capTCBPtr(cap), tcbCTable)) ||
            slotCapLongRunningDelete(
                TCB_PTR_CTE_PTR(cap_thread_cap_get_capTCBPtr(cap), tcbVTable))) {
        userError("TCB Configure: CSpace or VSpace currently being deleted.");
        current_syscall_error.type = seL4_IllegalOperation;
        return EXCEPTION_SYSCALL_ERROR;
    }

    if (cRootData != 0) {
        cRootCap = updateCapData(false, cRootData, cRootCap);
    }

    dc_ret = deriveCap(cRootSlot, cRootCap);
    if (dc_ret.status != EXCEPTION_NONE) {
        return dc_ret.status;
    }
    cRootCap = dc_ret.cap;

    if (cap_get_capType(cRootCap) != cap_cnode_cap &&
            (!config_set(CONFIG_ALLOW_NULL_CSPACE) ||
             cap_get_capType(cRootCap) != cap_null_cap)) {
        userError("TCB Configure: CSpace cap is invalid.");
        current_syscall_error.type = seL4_IllegalOperation;
        return EXCEPTION_SYSCALL_ERROR;
    }

    if (vRootData != 0) {
        vRootCap = updateCapData(false, vRootData, vRootCap);
    }

    dc_ret = deriveCap(vRootSlot, vRootCap);
    if (dc_ret.status != EXCEPTION_NONE) {
        return dc_ret.status;
    }
    vRootCap = dc_ret.cap;

    if (!isValidVTableRoot(vRootCap)) {
        userError("TCB Configure: VSpace cap is invalid.");
        current_syscall_error.type = seL4_IllegalOperation;
        return EXCEPTION_SYSCALL_ERROR;
    }

    setThreadState(ksCurThread, ThreadState_Restart);
    return invokeTCB_ThreadControl(
               TCB_PTR(cap_thread_cap_get_capTCBPtr(cap)), slot,
               faultEP, temporalFEP, prio,
               cRootCap, cRootSlot,
               vRootCap, vRootSlot,
               bufferAddr, bufferCap,
               bufferSlot,
               sched_context,
               thread_control_update_all);
}

exception_t
invokeTCB_SetSchedContext(tcb_t *tcb, sched_context_t *sched_context)
{

    exception_t status;

    setThreadState(ksCurThread, ThreadState_Restart);
    status = invokeTCB_ThreadControl(
                 tcb, NULL,
                 0, 0, TCB_PRIO_NULL,
                 cap_null_cap_new(), 0,
                 cap_null_cap_new(), 0,
                 0, cap_null_cap_new(),
                 0, sched_context,
                 thread_control_update_sc);

    if (status == EXCEPTION_NONE) {
        if (isSchedulable(tcb)) {
            possibleSwitchTo(tcb, false, false);
        }
    }

    /* don't even ask */
#ifdef CONFIG_BENCHMARK
    //TODO@alyons remove
    /* Fine. The kernel needs to be able to start
     * all of the edf threads at once for the benchmarks.
     * But it doesn't have a list of all of them.
     * Unless we sneakily put them all (unresumed) into
     * the scheduler queue anyway... */
    tcbSchedEnqueue(tcb);
#endif

    return status;
}

exception_t
invokeTCB_ClearSchedContext(tcb_t *tcb)
{

    tcb->tcbSchedContext->tcb = NULL;
    tcb->tcbSchedContext = NULL;

    if (tcb == ksCurThread) {
        rescheduleRequired();
    }

    return EXCEPTION_NONE;
}

exception_t
decodeClearSchedContext(cap_t cap)
{

    tcb_t *tcb = TCB_PTR(cap_thread_cap_get_capTCBPtr(cap));
    if (tcb->tcbSchedContext == NULL) {
        userError("TCB has no scheduling context\n");
        current_syscall_error.type = seL4_IllegalOperation;
        return EXCEPTION_SYSCALL_ERROR;
    }

    setThreadState(ksCurThread, ThreadState_Restart);
    return invokeTCB_ClearSchedContext(tcb);
}


exception_t
decodeSetSchedContext(cap_t cap, extra_caps_t rootCaps)
{
    cap_t scCap;
    sched_context_t *sched_context;
    tcb_t *tcb;

    if (rootCaps.excaprefs[0] == NULL) {
        userError("TCB SetSchedContext: Truncated message.");
        current_syscall_error.type = seL4_TruncatedMessage;
        return EXCEPTION_SYSCALL_ERROR;
    }

    /* sched_context cap must be valid */
    scCap = rootCaps.excaprefs[0]->cap;

    if (cap_get_capType(scCap) != cap_sched_context_cap) {
        userError("TCB SetSchedContext: sched_context cap is invalid.");
        current_syscall_error.type = seL4_IllegalOperation;
        return EXCEPTION_SYSCALL_ERROR;
    }

    sched_context = SCHED_CONTEXT_PTR(cap_sched_context_cap_get_capPtr(scCap));
    tcb = TCB_PTR(cap_thread_cap_get_capTCBPtr(cap));

    if (sched_context->tcb != NULL) {
        userError("TCB SetSchedContext: sched_context is already bound to a tcb.");
        current_syscall_error.type = seL4_IllegalOperation;
        return EXCEPTION_SYSCALL_ERROR;
    }

    return invokeTCB_SetSchedContext(tcb, sched_context);
}


exception_t
decodeSetMaxPriority(cap_t cap, unsigned int length, word_t *buffer)
{
    prio_t newMaxPrio;
    tcb_prio_t new;
    tcb_t *tcb;

    if (length < 1) {
        userError("TCB SetPriority: Truncated message.");
        current_syscall_error.type = seL4_TruncatedMessage;
        return EXCEPTION_SYSCALL_ERROR;
    }

    newMaxPrio = getSyscallArg(0, buffer);

    /* assuming here seL4_MaxPrio is of form 2^n - 1 */
    newMaxPrio = newMaxPrio & MASK(8);

    if (newMaxPrio > tcb_prio_get_maxPrio(ksCurThread->tcbPriority)) {
        userError("TCB SetPriority: Requested max priority %d too high (max %d).",
                  (int) newMaxPrio,
                  (int) tcb_prio_get_maxPrio(ksCurThread->tcbPriority));
        current_syscall_error.type = seL4_IllegalOperation;
        return EXCEPTION_SYSCALL_ERROR;
    }

    setThreadState(ksCurThread, ThreadState_Restart);
    tcb = TCB_PTR(cap_thread_cap_get_capTCBPtr(cap));


    new = tcb_prio_new(newMaxPrio, tcb_prio_get_prio(tcb->tcbPriority),
                       tcb_prio_get_maxCriticality(tcb->tcbPriority), tcb_prio_get_criticality(tcb->tcbPriority));

    return invokeTCB_ThreadControl(
               tcb, NULL,
               0, 0, new,
               cap_null_cap_new(), NULL,
               cap_null_cap_new(), NULL,
               0, cap_null_cap_new(),
               NULL, NULL,
               thread_control_update_priority);

}

exception_t
decodeSetPriority(cap_t cap, unsigned int length, word_t *buffer)
{
    prio_t newPrio;
    tcb_t *tcb;
    tcb_prio_t new;

    if (length < 1) {
        userError("TCB SetPriority: Truncated message.");
        current_syscall_error.type = seL4_TruncatedMessage;
        return EXCEPTION_SYSCALL_ERROR;
    }

    newPrio = getSyscallArg(0, buffer);

    /* assuming here seL4_MaxPrio is of form 2^n - 1 */
    newPrio = newPrio & MASK(8);

    if (newPrio > tcb_prio_get_maxPrio(ksCurThread->tcbPriority)) {
        userError("TCB SetPriority: Requested priority %d too high (max %d).",
                  (int) newPrio,
                  (int) tcb_prio_get_maxPrio(ksCurThread->tcbPriority));
        current_syscall_error.type = seL4_IllegalOperation;
        return EXCEPTION_SYSCALL_ERROR;
    }

    setThreadState(ksCurThread, ThreadState_Restart);
    tcb = TCB_PTR(cap_thread_cap_get_capTCBPtr(cap));

    new = tcb_prio_new(tcb_prio_get_maxPrio(tcb->tcbPriority), newPrio,
                       tcb_prio_get_maxCriticality(tcb->tcbPriority), tcb_prio_get_criticality(tcb->tcbPriority));

    return invokeTCB_ThreadControl(
               tcb, NULL,
               0, 0, new,
               cap_null_cap_new(), NULL,
               cap_null_cap_new(), NULL,
               0, cap_null_cap_new(),
               NULL, NULL,
               thread_control_update_priority);
}

exception_t
decodeSetIPCBuffer(cap_t cap, unsigned int length, cte_t* slot,
                   extra_caps_t extraCaps, word_t *buffer)
{
    cptr_t cptr_bufferPtr;
    cap_t bufferCap;
    cte_t *bufferSlot;

    if (length < 1 || extraCaps.excaprefs[0] == NULL) {
        userError("TCB SetIPCBuffer: Truncated message.");
        current_syscall_error.type = seL4_TruncatedMessage;
        return EXCEPTION_SYSCALL_ERROR;
    }

    cptr_bufferPtr  = getSyscallArg(0, buffer);
    bufferSlot = extraCaps.excaprefs[0];
    bufferCap  = extraCaps.excaprefs[0]->cap;

    if (cptr_bufferPtr == 0) {
        bufferSlot = NULL;
    } else {
        exception_t e;
        deriveCap_ret_t dc_ret;

        dc_ret = deriveCap(bufferSlot, bufferCap);
        if (dc_ret.status != EXCEPTION_NONE) {
            return dc_ret.status;
        }
        bufferCap = dc_ret.cap;
        e = checkValidIPCBuffer(cptr_bufferPtr, bufferCap);
        if (e != EXCEPTION_NONE) {
            return e;
        }
    }

    setThreadState(ksCurThread, ThreadState_Restart);
    return invokeTCB_ThreadControl(
               TCB_PTR(cap_thread_cap_get_capTCBPtr(cap)), slot,
               0, 0, TCB_PRIO_NULL,
               cap_null_cap_new(), NULL,
               cap_null_cap_new(), NULL,
               cptr_bufferPtr, bufferCap,
               bufferSlot, NULL,
               thread_control_update_ipc_buffer);
}

exception_t
decodeSetSpace(cap_t cap, unsigned int length, cte_t* slot,
               extra_caps_t extraCaps, word_t *buffer)
{
    cptr_t faultEP, temporalFEP;
    word_t cRootData, vRootData;
    cte_t *cRootSlot, *vRootSlot;
    cap_t cRootCap, vRootCap;
    deriveCap_ret_t dc_ret;

    if (length < 4 || extraCaps.excaprefs[0] == NULL
            || extraCaps.excaprefs[1] == NULL) {
        userError("TCB SetSpace: Truncated message.");
        current_syscall_error.type = seL4_TruncatedMessage;
        return EXCEPTION_SYSCALL_ERROR;
    }

    faultEP   = getSyscallArg(0, buffer);
    cRootData = getSyscallArg(1, buffer);
    vRootData = getSyscallArg(2, buffer);
    temporalFEP = getSyscallArg(3, buffer);

    cRootSlot  = extraCaps.excaprefs[0];
    cRootCap   = extraCaps.excaprefs[0]->cap;
    vRootSlot  = extraCaps.excaprefs[1];
    vRootCap   = extraCaps.excaprefs[1]->cap;

    if (slotCapLongRunningDelete(
                TCB_PTR_CTE_PTR(cap_thread_cap_get_capTCBPtr(cap), tcbCTable)) ||
            slotCapLongRunningDelete(
                TCB_PTR_CTE_PTR(cap_thread_cap_get_capTCBPtr(cap), tcbVTable))) {
        userError("TCB SetSpace: CSpace or VSpace currently being deleted.");
        current_syscall_error.type = seL4_IllegalOperation;
        return EXCEPTION_SYSCALL_ERROR;
    }

    if (cRootData != 0) {
        cRootCap = updateCapData(false, cRootData, cRootCap);
    }

    dc_ret = deriveCap(cRootSlot, cRootCap);
    if (dc_ret.status != EXCEPTION_NONE) {
        return dc_ret.status;
    }
    cRootCap = dc_ret.cap;

    if (cap_get_capType(cRootCap) != cap_cnode_cap &&
            (!config_set(CONFIG_ALLOW_NULL_CSPACE) ||
             cap_get_capType(cRootCap) != cap_null_cap)) {
        userError("TCB SetSpace: Invalid CNode cap.");
        current_syscall_error.type = seL4_IllegalOperation;
        return EXCEPTION_SYSCALL_ERROR;
    }

    if (vRootData != 0) {
        vRootCap = updateCapData(false, vRootData, vRootCap);
    }

    dc_ret = deriveCap(vRootSlot, vRootCap);
    if (dc_ret.status != EXCEPTION_NONE) {
        return dc_ret.status;
    }
    vRootCap = dc_ret.cap;

    if (!isValidVTableRoot(vRootCap)) {
        userError("TCB SetSpace: Invalid VSpace cap.");
        current_syscall_error.type = seL4_IllegalOperation;
        return EXCEPTION_SYSCALL_ERROR;
    }

    setThreadState(ksCurThread, ThreadState_Restart);
    return invokeTCB_ThreadControl(
               TCB_PTR(cap_thread_cap_get_capTCBPtr(cap)), slot,
               faultEP, temporalFEP,
               TCB_PRIO_NULL,
               cRootCap, cRootSlot,
               vRootCap, vRootSlot,
               0, cap_null_cap_new(), NULL, NULL,
               thread_control_update_space);
}

exception_t
decodeDomainInvocation(word_t label, unsigned int length, extra_caps_t extraCaps, word_t *buffer)
{
    word_t domain;
    cap_t tcap;

    if (unlikely(label != DomainSetSet)) {
        current_syscall_error.type = seL4_IllegalOperation;
        return EXCEPTION_SYSCALL_ERROR;
    }

    if (unlikely(length == 0)) {
        userError("Domain Configure: Truncated message.");
        current_syscall_error.type = seL4_TruncatedMessage;
        return EXCEPTION_SYSCALL_ERROR;
    } else {
        domain = getSyscallArg(0, buffer);
        if (domain >= CONFIG_NUM_DOMAINS) {
            userError("Domain Configure: invalid domain (%d >= %d).",
                      (int)domain, CONFIG_NUM_DOMAINS);
            current_syscall_error.type = seL4_InvalidArgument;
            current_syscall_error.invalidArgumentNumber = 0;
            return EXCEPTION_SYSCALL_ERROR;
        }
    }

    if (unlikely(extraCaps.excaprefs[0] == NULL)) {
        userError("Domain Configure: Truncated message.");
        current_syscall_error.type = seL4_TruncatedMessage;
        return EXCEPTION_SYSCALL_ERROR;
    }

    tcap = extraCaps.excaprefs[0]->cap;
    if (unlikely(cap_get_capType(tcap) != cap_thread_cap)) {
        userError("Domain Configure: thread cap required.");
        current_syscall_error.type = seL4_InvalidArgument;
        current_syscall_error.invalidArgumentNumber = 1;
        return EXCEPTION_SYSCALL_ERROR;
    }

    setThreadState(ksCurThread, ThreadState_Restart);
    setDomain(TCB_PTR(cap_thread_cap_get_capTCBPtr(tcap)), domain);
    return EXCEPTION_NONE;
}

exception_t decodeBindAEP(cap_t cap, extra_caps_t extraCaps)
{
    async_endpoint_t *aepptr;
    tcb_t *tcb;

    if (extraCaps.excaprefs[0] == NULL) {
        userError("TCB BindAEP: Truncated message.");
        current_syscall_error.type = seL4_TruncatedMessage;
        return EXCEPTION_SYSCALL_ERROR;
    }

    if (cap_get_capType(extraCaps.excaprefs[0]->cap) != cap_async_endpoint_cap) {
        userError("TCB BindAEP: Async endpoint is invalid.");
        current_syscall_error.type = seL4_IllegalOperation;
        return EXCEPTION_SYSCALL_ERROR;
    }

    tcb = TCB_PTR(cap_thread_cap_get_capTCBPtr(cap));

    if (tcb->boundAsyncEndpoint) {
        userError("TCB BindAEP: TCB already has AEP.");
        current_syscall_error.type = seL4_IllegalOperation;
        return EXCEPTION_SYSCALL_ERROR;
    }

    aepptr = AEP_PTR(cap_async_endpoint_cap_get_capAEPPtr(extraCaps.excaprefs[0]->cap));
    if ((tcb_t*)async_endpoint_ptr_get_aepQueue_head(aepptr)) {
        userError("TCB BindAEP: AEP cannot be bound.");
        current_syscall_error.type = seL4_IllegalOperation;
        return EXCEPTION_SYSCALL_ERROR;
    }

    setThreadState(ksCurThread, ThreadState_Restart);
    return invokeTCB_AEPControl(tcb, aepptr);
}

exception_t decodeUnbindAEP(cap_t cap)
{
    tcb_t *tcb;

    tcb = TCB_PTR(cap_thread_cap_get_capTCBPtr(cap));

    if (!tcb->boundAsyncEndpoint) {
        userError("TCB UnbindAEP: TCB already has no bound AEP.");
        current_syscall_error.type = seL4_IllegalOperation;
        return EXCEPTION_SYSCALL_ERROR;
    }

    setThreadState(ksCurThread, ThreadState_Restart);
    return invokeTCB_AEPControl(tcb, NULL);
}

/* The following functions sit in the preemption monad and implement the
 * preemptible, non-faulting bottom end of a TCB invocation. */
exception_t
invokeTCB_Suspend(tcb_t *thread)
{
    suspend(thread);
    return EXCEPTION_NONE;
}

exception_t
invokeTCB_Resume(tcb_t *thread)
{
    restart(thread);
    return EXCEPTION_NONE;
}

exception_t
invokeTCB_ThreadControl(tcb_t *target, cte_t* slot,
                        cptr_t faultep, cptr_t temporalFEP, tcb_prio_t priority,
                        cap_t cRoot_newCap, cte_t *cRoot_srcSlot,
                        cap_t vRoot_newCap, cte_t *vRoot_srcSlot,
                        word_t bufferAddr, cap_t bufferCap,
                        cte_t *bufferSrcSlot,
                        sched_context_t *sched_context,
                        thread_control_flag_t updateFlags)
{
    exception_t e;
    cap_t tCap = cap_thread_cap_new((word_t)target);

    if (updateFlags & thread_control_update_space) {
        target->tcbFaultHandler = faultep;
        target->tcbTemporalFaultHandler = temporalFEP;
    }

    if (updateFlags & thread_control_update_sc) {
        target->tcbSchedContext = sched_context;
        target->tcbHomeSchedContext = sched_context;
        if (sched_context != NULL) {
            sched_context->tcb = target;
            sched_context->home = target;
        }
    }

    setupReplyMaster(target);

    if (updateFlags & thread_control_update_priority) {
        setPriority(target, priority);
    }

    if (updateFlags & thread_control_update_space) {
        cte_t *rootSlot;

        rootSlot = TCB_PTR_CTE_PTR(target, tcbCTable);
        e = cteDelete(rootSlot, true);
        if (e != EXCEPTION_NONE) {
            return e;
        }
        if (sameObjectAs(cRoot_newCap, cRoot_srcSlot->cap) &&
                sameObjectAs(tCap, slot->cap)) {
            cteInsert(cRoot_newCap, cRoot_srcSlot, rootSlot);
        }
    }

    if (updateFlags & thread_control_update_space) {
        cte_t *rootSlot;

        rootSlot = TCB_PTR_CTE_PTR(target, tcbVTable);
        e = cteDelete(rootSlot, true);
        if (e != EXCEPTION_NONE) {
            return e;
        }
        if (sameObjectAs(vRoot_newCap, vRoot_srcSlot->cap) &&
                sameObjectAs(tCap, slot->cap)) {
            cteInsert(vRoot_newCap, vRoot_srcSlot, rootSlot);
        }
    }

    if (updateFlags & thread_control_update_ipc_buffer) {
        cte_t *bufferSlot;

        bufferSlot = TCB_PTR_CTE_PTR(target, tcbBuffer);
        e = cteDelete(bufferSlot, true);
        if (e != EXCEPTION_NONE) {
            return e;
        }
        target->tcbIPCBuffer = bufferAddr;
        if (bufferSrcSlot && sameObjectAs(bufferCap, bufferSrcSlot->cap) &&
                sameObjectAs(tCap, slot->cap)) {
            cteInsert(bufferCap, bufferSrcSlot, bufferSlot);
        }
    }

    return EXCEPTION_NONE;
}

exception_t
invokeTCB_CopyRegisters(tcb_t *dest, tcb_t *tcb_src,
                        bool_t suspendSource, bool_t resumeTarget,
                        bool_t transferFrame, bool_t transferInteger,
                        word_t transferArch)
{
    if (suspendSource) {
        suspend(tcb_src);
    }

    if (resumeTarget) {
        restart(dest);
    }

    if (transferFrame) {
        unsigned int i;
        word_t v;
        word_t pc;

        for (i = 0; i < n_frameRegisters; i++) {
            v = getRegister(tcb_src, frameRegisters[i]);
            setRegister(dest, frameRegisters[i], v);
        }

        pc = getRestartPC(dest);
        setNextPC(dest, pc);
    }

    if (transferInteger) {
        unsigned int i;
        word_t v;

        for (i = 0; i < n_gpRegisters; i++) {
            v = getRegister(tcb_src, gpRegisters[i]);
            setRegister(dest, gpRegisters[i], v);
        }
    }

    return Arch_performTransfer(transferArch, tcb_src, dest);
}

/* ReadRegisters is a special case: replyFromKernel & setMRs are
 * unfolded here, in order to avoid passing the large reply message up
 * to the top level in a global (and double-copying). We prevent the
 * top-level replyFromKernel_success_empty() from running by setting the
 * thread state. Retype does this too.
 */
exception_t
invokeTCB_ReadRegisters(tcb_t *tcb_src, bool_t suspendSource,
                        unsigned int n, word_t arch, bool_t call)
{
    unsigned int i, j;
    exception_t e;
    tcb_t *thread;

    thread = ksCurThread;

    if (suspendSource) {
        suspend(tcb_src);
    }

    e = Arch_performTransfer(arch, tcb_src, ksCurThread);
    if (e != EXCEPTION_NONE) {
        return e;
    }

    if (call) {
        word_t *ipcBuffer;

        ipcBuffer = lookupIPCBuffer(true, thread);

        setRegister(thread, badgeRegister, 0);

        for (i = 0; i < n && i < n_frameRegisters && i < n_msgRegisters; i++) {
            setRegister(thread, msgRegisters[i],
                        getRegister(tcb_src, frameRegisters[i]));
        }

        if (ipcBuffer != NULL && i < n && i < n_frameRegisters) {
            for (; i < n && i < n_frameRegisters; i++) {
                ipcBuffer[i + 1] = getRegister(tcb_src, frameRegisters[i]);
            }
        }

        j = i;

        for (i = 0; i < n_gpRegisters && i + n_frameRegisters < n
                && i + n_frameRegisters < n_msgRegisters; i++) {
            setRegister(thread, msgRegisters[i + n_frameRegisters],
                        getRegister(tcb_src, gpRegisters[i]));
        }

        if (ipcBuffer != NULL && i < n_gpRegisters
                && i + n_frameRegisters < n) {
            for (; i < n_gpRegisters && i + n_frameRegisters < n; i++) {
                ipcBuffer[i + n_frameRegisters + 1] =
                    getRegister(tcb_src, gpRegisters[i]);
            }
        }

        setRegister(thread, msgInfoRegister, wordFromMessageInfo(
                        message_info_new(0, 0, 0, i + j)));
    }
    setThreadState(thread, ThreadState_Running);

    return EXCEPTION_NONE;
}

exception_t
invokeTCB_WriteRegisters(tcb_t *dest, bool_t resumeTarget,
                         unsigned int n, word_t arch, word_t *buffer)
{
    unsigned int i;
    word_t pc;
    exception_t e;

    e = Arch_performTransfer(arch, ksCurThread, dest);
    if (e != EXCEPTION_NONE) {
        return e;
    }

    if (n > n_frameRegisters + n_gpRegisters) {
        n = n_frameRegisters + n_gpRegisters;
    }

    for (i = 0; i < n_frameRegisters && i < n; i++) {
        /* Offset of 2 to get past the initial syscall arguments */
        setRegister(dest, frameRegisters[i],
                    sanitiseRegister(frameRegisters[i],
                                     getSyscallArg(i + 2, buffer)));
    }

    for (i = 0; i < n_gpRegisters && i + n_frameRegisters < n; i++) {
        setRegister(dest, gpRegisters[i],
                    sanitiseRegister(gpRegisters[i],
                                     getSyscallArg(i + n_frameRegisters + 2,
                                                   buffer)));
    }

    pc = getRestartPC(dest);
    setNextPC(dest, pc);

    if (resumeTarget) {
        restart(dest);
    }

    return EXCEPTION_NONE;
}

exception_t
invokeTCB_AEPControl(tcb_t *tcb, async_endpoint_t *aepptr)
{
    if (aepptr) {
        bindAsyncEndpoint(tcb, aepptr);
    } else {
        unbindAsyncEndpoint(tcb);
    }

    return EXCEPTION_NONE;
}

