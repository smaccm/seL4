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
#include <benchmark.h>
#include <api/syscall.h>
#include <api/failures.h>
#include <api/faults.h>
#include <kernel/cspace.h>
#include <kernel/faulthandler.h>
#include <kernel/thread.h>
#include <kernel/vspace.h>
#include <machine/io.h>
#include <object/interrupt.h>
#include <model/statedata.h>
#include <object/schedcontext.h>

#ifdef CONFIG_ARCH_ARM
#include <arch/machine/priv_timer.h>
#endif

#ifdef DEBUG
#include <arch/machine/capdl.h>
#endif

/* The haskell function 'handleEvent' is split into 'handleXXX' variants
 * for each event causing a kernel entry */

exception_t
handleInterruptEntry(void)
{
    irq_t irq;
    ksCurrentTime = getCurrentTime();

    irq = getActiveIRQ();
    if (irq != irqInvalid) {
        handleInterrupt(irq);
    } else {
        printf("Spurious interrupt\n");
        handleSpuriousIRQ();
    }

    schedule();
    activateThread();

    return EXCEPTION_NONE;
}

exception_t
handleUnknownSyscall(word_t w)
{
#ifdef DEBUG
    if (w == SysDebugPutChar) {
        kernel_putchar(getRegister(ksCurThread, capRegister));
        return EXCEPTION_NONE;
    }
    if (w == SysDebugHalt) {
        printf("Debug halt syscall from user thread 0x%x\n", (unsigned int)ksCurThread);
        halt();
    }
    if (w == SysDebugSnapshot) {
        printf("Debug snapshot syscall from user thread 0x%x\n", (unsigned int)ksCurThread);
        capDL();
        return EXCEPTION_NONE;
    }
    if (w == SysDebugCapIdentify) {
        word_t cptr = getRegister(ksCurThread, capRegister);
        lookupCapAndSlot_ret_t lu_ret = lookupCapAndSlot(ksCurThread, cptr);
        uint32_t cap_type = cap_get_capType(lu_ret.cap);
        setRegister(ksCurThread, capRegister, cap_type);
        return EXCEPTION_NONE;
    }
#endif

#ifdef DANGEROUS_CODE_INJECTION
    if (w == SysDebugRun) {
        ((void (*) (void *))getRegister(ksCurThread, capRegister))((void*)getRegister(ksCurThread, msgInfoRegister));
        return EXCEPTION_NONE;
    }
#endif

#ifdef CONFIG_BENCHMARK
    if (w == SysBenchmarkResetLog) {
        /* benchmark hack: resume all threads in the edf scheduling priority */
        for (tcb_t *tcb = ksReadyQueues[seL4_EDFPrio].head; tcb != NULL; tcb = tcb->tcbSchedNext) {
            assert(tcb != NULL);
            restart(tcb);
        }
        /* reset the log */
        ksLogIndex = 0;
        return EXCEPTION_NONE;
    } else if (w == SysBenchmarkDumpLog) {
        int i;
        word_t *buffer = lookupIPCBuffer(true, ksCurThread);
        word_t start = getRegister(ksCurThread, capRegister);
        word_t size = getRegister(ksCurThread, msgInfoRegister);
        word_t logSize = ksLogIndex > MAX_LOG_SIZE ? MAX_LOG_SIZE : ksLogIndex;

        if (buffer == NULL) {
            userError("Cannot dump benchmarking log to a thread without an ipc buffer\n");
            current_syscall_error.type = seL4_IllegalOperation;
            return EXCEPTION_SYSCALL_ERROR;
        }

        if (start > logSize) {
            userError("Start > logsize\n");
            current_syscall_error.type = seL4_InvalidArgument;
            return EXCEPTION_SYSCALL_ERROR;
        }

        /* Assume we have access to an ipc buffer 1024 words big.
         * Do no write to the first 4 bytes as these are overwritten */
        if (size > MAX_IPC_BUFFER_STORAGE) {
            size = MAX_IPC_BUFFER_STORAGE;
        }

        /* trim to size */
        if ((start + size) > logSize) {
            size = logSize - start;
        }

        /* write to ipc buffer */
        for (i = 0; i < size; i++) {
            buffer[i + 1] = ksLog[i + start];
        }

        /* Return the amount written */
        setRegister(ksCurThread, capRegister, size);
        return EXCEPTION_NONE;
    } else if (w == SysBenchmarkLogSize) {
        /* Return the amount of log items we tried to log (may exceed max size) */
        setRegister(ksCurThread, capRegister, ksLogIndex);
        return EXCEPTION_NONE;
    }
#endif /* CONFIG_BENCHMARK */

    current_fault = fault_unknown_syscall_new(w);
    handleFault(ksCurThread);

    schedule();
    activateThread();

    return EXCEPTION_NONE;
}

exception_t
handleUserLevelFault(word_t w_a, word_t w_b)
{
    current_fault = fault_user_exception_new(w_a, w_b);
    handleFault(ksCurThread);

    schedule();
    activateThread();

    return EXCEPTION_NONE;
}

exception_t
handleVMFaultEvent(vm_fault_type_t vm_faultType)
{
    exception_t status;

    status = handleVMFault(ksCurThread, vm_faultType);
    if (status != EXCEPTION_NONE) {
        handleFault(ksCurThread);
    }

    schedule();
    activateThread();

    return EXCEPTION_NONE;
}


static exception_t
handleInvocation(bool_t isCall, bool_t isBlocking)
{
    message_info_t info;
    cptr_t cptr;
    lookupCapAndSlot_ret_t lu_ret;
    word_t *buffer;
    exception_t status;
    word_t length;
    tcb_t *thread;

    thread = ksCurThread;

    info = messageInfoFromWord(getRegister(thread, msgInfoRegister));
    cptr = getRegister(thread, capRegister);

    /* faulting section */
    lu_ret = lookupCapAndSlot(thread, cptr);

    if (unlikely(lu_ret.status != EXCEPTION_NONE)) {
        userError("Invocation of invalid cap #%d.", (int)cptr);
        current_fault = fault_cap_fault_new(cptr, false);

        if (isBlocking) {
            handleFault(thread);
        }

        return EXCEPTION_NONE;
    }

    buffer = lookupIPCBuffer(false, thread);

    status = lookupExtraCaps(thread, buffer, info);

    if (unlikely(status != EXCEPTION_NONE)) {
        userError("Lookup of extra caps failed.");
        if (isBlocking) {
            handleFault(thread);
        }
        return EXCEPTION_NONE;
    }

    /* Syscall error/Preemptible section */
    length = message_info_get_msgLength(info);
    if (unlikely(length > n_msgRegisters && !buffer)) {
        length = n_msgRegisters;
    }

    status = decodeInvocation(message_info_get_msgLabel(info), length,
                              cptr, lu_ret.slot, lu_ret.cap,
                              current_extra_caps, isBlocking, isCall,
                              buffer);

    if (unlikely(status == EXCEPTION_PREEMPTED)) {
        return status;
    }

    if (unlikely(status == EXCEPTION_SYSCALL_ERROR)) {
        if (isCall) {
            replyFromKernel_error(thread);
        }
        return EXCEPTION_NONE;
    }

    if (unlikely(
                thread_state_get_tsType(thread->tcbState) == ThreadState_Restart)) {
        if (isCall) {
            replyFromKernel_success_empty(thread);
        }
        setThreadState(thread, ThreadState_Running);
    }

    return EXCEPTION_NONE;
}

static bool_t
handleReply(bool_t replyWait)
{
    cte_t *callerSlot;
    cap_t callerCap;

    callerSlot = TCB_PTR_CTE_PTR(ksCurThread, tcbCaller);
    callerCap = callerSlot->cap;
    switch (cap_get_capType(callerCap)) {
    case cap_reply_cap: {
        tcb_t *caller;
        bool_t donationOccured;

        if (cap_reply_cap_get_capReplyMaster(callerCap)) {
            break;
        }
        caller = TCB_PTR(cap_reply_cap_get_capTCBPtr(callerCap));
        /* Haskell error:
         * "handleReply: caller must not be the current thread" */
        assert(caller != ksCurThread);
        if (!replyWait && caller->tcbSchedContext == NULL) {
            userError("Attempted to reply to a thread with no scheduling context\n");
            /* TODO send fault */
        }

        donationOccured = replyWait && (caller->tcbSchedContext == NULL);
        doReplyTransfer(ksCurThread, caller, callerSlot, donationOccured);
        return donationOccured;
    }

    case cap_null_cap:
        userError("Attempted reply operation when no reply cap present.");
        return false;

    default:
        break;
    }

    fail("handleReply: invalid caller cap");

    return false;
}

static void
handleSendWait(void)
{

    tcb_t *replyTCB = NULL;
    word_t destCPtr, srcCPtr;
    lookupCapAndSlot_ret_t dest_lu_ret;
    lookupCap_ret_t src_lu_ret;
    /* we receive a donation if the send resulted in us losing our current scheduling context */
    bool_t donationRequired;

#ifdef CONFIG_ARCH_IA32
    word_t *buffer = lookupIPCBuffer(true, ksCurThread);
    srcCPtr = getSyscallArg(seL4_MsgMaxLength - 1, buffer);
#elif CONFIG_ARCH_ARM
    srcCPtr = getRegister(ksCurThread, R8);
#else
#error "not implemented"
#endif

    destCPtr = getRegister(ksCurThread, capRegister);
    dest_lu_ret = lookupCapAndSlot(ksCurThread, destCPtr);
    src_lu_ret = lookupCap(ksCurThread, srcCPtr);

    if (unlikely(src_lu_ret.status != EXCEPTION_NONE)) {
        userError("seL4_SendWait: src cap lookup failed");
        current_fault = fault_cap_fault_new(srcCPtr, true);
        handleFault(ksCurThread);
        return;
    }

    if (unlikely(dest_lu_ret.status != EXCEPTION_NONE)) {
        userError("seL4_SendWait: dest cap lookup failed");
        current_fault = fault_cap_fault_new(destCPtr, true);
        handleFault(ksCurThread);
        return;
    }


    /* check the wait cap is a sync ep cap */
    if (unlikely(cap_get_capType(src_lu_ret.cap) != cap_endpoint_cap)) {
        userError("seL4_SendWait: src cap is not an endpoint cap");
        current_lookup_fault = lookup_fault_missing_capability_new(0);
        current_fault = fault_cap_fault_new(srcCPtr, true);
        handleFault(ksCurThread);
        return;
    }


    /* send action */
    switch (cap_get_capType(dest_lu_ret.cap)) {
    case cap_thread_cap: {
        replyTCB = TCB_PTR(cap_thread_cap_get_capTCBPtr(dest_lu_ret.cap));
        /* can't SendWait to a tcb with a scheduling context */
        if (replyTCB->tcbSchedContext != NULL) {
            userError("seL4_SendWait: tcb already has sched context");
            current_fault = fault_cap_fault_new(destCPtr, true);
            handleFault(ksCurThread);
            return;
        }

        /* switch threads */
        donationRequired = true;
        thread_state_ptr_set_tsType(&replyTCB->tcbState, ThreadState_Running);
        attemptSwitchTo(replyTCB, donationRequired);
        /* we don't need to touch ksCurSchedContext as it isn't changing */
    }
    break;

    case cap_endpoint_cap: {
        /* endpoint must be in recv state */
        endpoint_t *epptr = EP_PTR(cap_endpoint_cap_get_capEPPtr(dest_lu_ret.cap));
        if (unlikely(endpoint_ptr_get_state(epptr) != EPState_Recv)) {
            userError("seL4_SendWait: send EP not ready to receive message\n");
            current_fault = fault_cap_fault_new(destCPtr, true);
            handleFault(ksCurThread);
            return;
        }

        if (unlikely(!cap_endpoint_cap_get_capCanSend(dest_lu_ret.cap))) {
            userError("seL4_SendWait: attempted to send on endpoint cap without send rights\n");
            current_fault = fault_cap_fault_new(destCPtr, true);
            handleFault(ksCurThread);
            return;
        }

        donationRequired = sendIPC(true, false, cap_endpoint_cap_get_capEPBadge(dest_lu_ret.cap),
                                   cap_endpoint_cap_get_capCanGrant(dest_lu_ret.cap),
                                   ksCurThread, epptr);

    }
    break;
    case cap_reply_cap: {
        tcb_t *caller;
        if (cap_reply_cap_get_capReplyMaster(dest_lu_ret.cap)) {
            fail("handleReply: invalid caller cap");
        }

        caller = TCB_PTR(cap_reply_cap_get_capTCBPtr(dest_lu_ret.cap));
        donationRequired = (caller->tcbSchedContext == NULL);
        doReplyTransfer(ksCurThread, caller, dest_lu_ret.slot, caller->tcbSchedContext == NULL);
    }
    break;
    default: {
        userError("seL4_SendWait: called on unsupported cap type\n");
        current_lookup_fault = lookup_fault_missing_capability_new(0);
        current_fault = fault_cap_fault_new(destCPtr, true);
        handleFault(ksCurThread);
        return;
    }
    break;
    }

    /* wait action */
    receiveIPC(ksCurThread, src_lu_ret.cap, donationRequired);
    return;
}


static void
handleReplyWait(void)
{

    /* first look up the cap */
    word_t epCPtr;
    lookupCap_ret_t lu_ret;

    epCPtr = getRegister(ksCurThread, capRegister);
    lu_ret = lookupCap(ksCurThread, epCPtr);

    if (unlikely(lu_ret.status != EXCEPTION_NONE)) {
        current_fault = fault_cap_fault_new(epCPtr, true);
        handleFault(ksCurThread);
        return;
    }

    switch (cap_get_capType(lu_ret.cap)) {
    case cap_endpoint_cap: {
        bool_t donationOccured = handleReply(true);

        /* now handle the wait */
        deleteCallerCap(ksCurThread);
        if (unlikely(!cap_endpoint_cap_get_capCanReceive(lu_ret.cap))) {
            current_lookup_fault = lookup_fault_missing_capability_new(0);
            current_fault = fault_cap_fault_new(epCPtr, true);
            handleFault(ksCurThread);
            break;
        }

        receiveIPC(ksCurThread, lu_ret.cap, donationOccured);
        break;
    }
    case cap_async_endpoint_cap: {
        async_endpoint_t *aepptr;
        tcb_t *boundTCB;

        aepptr = AEP_PTR(cap_async_endpoint_cap_get_capAEPPtr(lu_ret.cap));
        boundTCB = (tcb_t*)async_endpoint_ptr_get_aepBoundTCB(aepptr);

        if (unlikely(!cap_async_endpoint_cap_get_capAEPCanReceive(lu_ret.cap)
                     || (boundTCB && boundTCB != ksCurThread))) {
            current_lookup_fault = lookup_fault_missing_capability_new(0);
            current_fault = fault_cap_fault_new(epCPtr, true);
            handleFault(ksCurThread);
            break;
        }

        handleReply(false);
        receiveAsyncIPC(ksCurThread, lu_ret.cap, true);
        break;
    }
    default:
        current_lookup_fault = lookup_fault_missing_capability_new(0);
        current_fault = fault_cap_fault_new(epCPtr, true);
        handleFault(ksCurThread);
        break;
    }

}

static void
handleWait(bool_t isBlocking)
{
    word_t epCPtr;
    lookupCap_ret_t lu_ret;

    deleteCallerCap(ksCurThread);

    epCPtr = getRegister(ksCurThread, capRegister);

    lu_ret = lookupCap(ksCurThread, epCPtr);
    if (unlikely(lu_ret.status != EXCEPTION_NONE)) {
        /* current_lookup_fault has been set by lookupCap */
        current_fault = fault_cap_fault_new(epCPtr, true);
        handleFault(ksCurThread);
        return;
    }

    switch (cap_get_capType(lu_ret.cap)) {
    case cap_endpoint_cap: {
        if (unlikely(!cap_endpoint_cap_get_capCanReceive(lu_ret.cap)) || !isBlocking) {
            current_lookup_fault = lookup_fault_missing_capability_new(0);
            current_fault = fault_cap_fault_new(epCPtr, true);
            handleFault(ksCurThread);
            break;
        }
        receiveIPC(ksCurThread, lu_ret.cap, false);

        break;
    }
    case cap_async_endpoint_cap: {
        async_endpoint_t *aepptr;
        tcb_t *boundTCB;
        aepptr = AEP_PTR(cap_async_endpoint_cap_get_capAEPPtr(lu_ret.cap));
        boundTCB = (tcb_t*)async_endpoint_ptr_get_aepBoundTCB(aepptr);
        if (unlikely(!cap_async_endpoint_cap_get_capAEPCanReceive(lu_ret.cap)
                     || (boundTCB && boundTCB != ksCurThread))) {
            current_lookup_fault = lookup_fault_missing_capability_new(0);
            current_fault = fault_cap_fault_new(epCPtr, true);
            handleFault(ksCurThread);
            break;
        }

        receiveAsyncIPC(ksCurThread, lu_ret.cap, isBlocking);
        break;
    }
    default:
        current_lookup_fault = lookup_fault_missing_capability_new(0);
        current_fault = fault_cap_fault_new(epCPtr, true);
        handleFault(ksCurThread);
        break;
    }
}

static void
handleYield(void)
{ 
    /* TODO @alyons what should this really do? */ 
    tcbSchedDequeue(ksCurThread);
    tcbSchedAppend(ksCurThread);
    rescheduleRequired();
}

exception_t
handleSyscall(syscall_t syscall)
{
    exception_t ret;
    irq_t irq;
    ksCurrentTime = getCurrentTime();

    switch (syscall) {
    case SysSend:
        ret = handleInvocation(false, true);
        if (unlikely(ret != EXCEPTION_NONE)) {
            irq = getActiveIRQ();
            if (irq != irqInvalid) {
                handleInterrupt(irq);
            }
        }
        break;

    case SysNBSend:
        ret = handleInvocation(false, false);
        if (unlikely(ret != EXCEPTION_NONE)) {
            irq = getActiveIRQ();
            if (irq != irqInvalid) {
                handleInterrupt(irq);
            }
        }
        break;

    case SysCall:
        ret = handleInvocation(true, true);
        if (unlikely(ret != EXCEPTION_NONE)) {
            irq = getActiveIRQ();
            if (irq != irqInvalid) {
                handleInterrupt(irq);
            }
        }
        break;

    case SysWait:
        handleWait(true);
        break;

    case SysReply:
        handleReply(false);
        break;

    case SysReplyWait:
        handleReplyWait();
        break;

    case SysPoll:
        handleWait(false);
        break;

    case SysYield:
        handleYield();
        break;

    case SysSendWait:
        handleSendWait();
        break;

    default:
        fail("Invalid syscall");
    }

    schedule();
    activateThread();

    return EXCEPTION_NONE;
}
