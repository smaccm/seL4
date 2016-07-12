/*
 * Copyright 2014, General Dynamics C4 Systems
 *
 * This software may be distributed and modified according to the terms of
 * the GNU General Public License version 2. Note that NO WARRANTY is provided.
 * See "LICENSE_GPLv2.txt" for details.
 *
 * @TAG(GD_GPL)
 */

#ifndef __OBJECT_TCB_H
#define __OBJECT_TCB_H

#include <types.h>
#include <api/failures.h>
#include <object/structures.h>
#include <object/schedcontext.h>

#include <arch/object/tcb.h>
#include <object/cnode.h>

#ifdef CONFIG_PRINTING
/* Maximum length of the tcb name, including null terminator */
#define TCB_NAME_LENGTH (BIT(seL4_TCBBits) - BIT(seL4_TCBBits) - sizeof(tcb_t))
#endif

struct tcb_queue {
    tcb_t *head;
    tcb_t *end;
};
typedef struct tcb_queue tcb_queue_t;

static inline void
tcbCallStackPush(tcb_t *tcb, tcb_t *stack)
{
    assert(tcb != NULL);
    assert(stack != NULL);
    assert(stack->tcbCallStackNext == NULL);

    stack->tcbCallStackNext = tcb;
    tcb->tcbCallStackPrev = stack;
}

static inline void
tcbCallStackPop(tcb_t *head)
{
    assert(head->tcbCallStackNext == NULL);
    assert(head != NULL);

    if (likely(head->tcbCallStackPrev)) {
        head->tcbCallStackPrev->tcbCallStackNext = NULL;
    }
    head->tcbCallStackPrev = NULL;
}

void tcbCallStackRemove(tcb_t* target);
void tcbSchedEnqueue(tcb_t *tcb);
void tcbSchedAppend(tcb_t *tcb);
void tcbSchedDequeue(tcb_t *tcb);

void tcbReleaseRemove(tcb_t *tcb);
void tcbReleaseEnqueue(tcb_t *tcb);
tcb_t *tcbReleaseDequeue(void);

void tcbCritEnqueue(tcb_t *tcb);
void tcbCritDequeue(tcb_t *tcb);

tcb_queue_t tcbEPAppend(tcb_t *tcb, tcb_queue_t queue);
tcb_queue_t tcbEPDequeue(tcb_t *tcb, tcb_queue_t queue);
tcb_queue_t tcbEPReorder(tcb_t *tcb, tcb_queue_t queue, prio_t oldPrio);

void setupCallerCap(tcb_t *sender, tcb_t *receiver, sched_context_t *sched_context);
void deleteCallerCap(tcb_t *receiver);

word_t copyMRs(tcb_t *sender, word_t *sendBuf, tcb_t *receiver,
               word_t *recvBuf, word_t n);
exception_t decodeTCBInvocation(word_t invLabel, word_t length, cap_t cap,
                                cte_t* slot, extra_caps_t excaps, bool_t call,
                                word_t *buffer);
exception_t decodeCopyRegisters(cap_t cap, word_t length,
                                extra_caps_t excaps, word_t *buffer);
exception_t decodeReadRegisters(cap_t cap, word_t length, bool_t call,
                                word_t *buffer);
exception_t decodeWriteRegisters(cap_t cap, word_t length, word_t *buffer);
exception_t decodeTCBConfigure(cap_t cap, word_t length,
                               cte_t* slot, extra_caps_t rootCaps, word_t *buffer);
exception_t decodeSetPriority(cap_t cap, word_t length, word_t *buffer);
exception_t decodeSetMCPriority(cap_t cap, word_t length, word_t *buffer);
exception_t decodeSetCriticality(cap_t cap, word_t length, word_t *buffer);
exception_t decodeSetMCCriticality(cap_t cap, word_t length, word_t *buffer);


exception_t decodeSetIPCBuffer(cap_t cap, word_t length,
                               cte_t* slot, extra_caps_t excaps, word_t *buffer);
exception_t decodeSetSpace(cap_t cap, word_t length,
                           cte_t* slot, extra_caps_t excaps, word_t *buffer);
exception_t decodeBindNotification(cap_t cap, extra_caps_t excaps);
exception_t decodeUnbindNotification(cap_t cap);

enum thread_control_flag {
    thread_control_update_priority = 0x1,
    thread_control_update_ipc_buffer = 0x2,
    thread_control_update_space = 0x4,
    thread_control_update_sc = 0x8,
    thread_control_update_all = 0xF,
};

typedef word_t thread_control_flag_t;

exception_t invokeTCB_Suspend(tcb_t *thread);
exception_t invokeTCB_Resume(tcb_t *thread);
exception_t invokeTCB_ThreadControl(tcb_t *target, cte_t* slot,
                                    cap_t fepCap, cte_t *fepSlot,
                                    cap_t tfepCap, cte_t *tfepSlot,
                                    seL4_Prio_t priority, cap_t cRoot_newCap,
                                    cte_t *cRoot_srcSlot, cap_t vRoot_newCap,
                                    cte_t *vRoot_srcSlot, word_t bufferAddr,
                                    cap_t bufferCap, cte_t *bufferSrcSlot,
                                    sched_context_t *sched_context,
                                    thread_control_flag_t updateFlags);
exception_t invokeTCB_CopyRegisters(tcb_t *dest, tcb_t *src,
                                    bool_t suspendSource, bool_t resumeTarget,
                                    bool_t transferFrame, bool_t transferInteger,
                                    word_t transferArch);
exception_t invokeTCB_ReadRegisters(tcb_t *src, bool_t suspendSource,
                                    word_t n, word_t arch, bool_t call);
exception_t invokeTCB_WriteRegisters(tcb_t *dest, bool_t resumeTarget,
                                     word_t n, word_t arch, word_t *buffer);
exception_t invokeTCB_NotificationControl(tcb_t *tcb, notification_t *ntfnPtr);

cptr_t PURE getExtraCPtr(word_t *bufferPtr, word_t i);
void setExtraBadge(word_t *bufferPtr, word_t badge, word_t i);

exception_t lookupExtraCaps(tcb_t* thread, word_t *bufferPtr, word_t length);
#ifdef CONFIG_PRINTING
void setThreadName(tcb_t *thread, const char *name);
#endif

#endif
