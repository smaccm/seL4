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

#include <config.h>
#include <types.h>
#include <api/failures.h>
#include <object/structures.h>

#include <arch/object/tcb.h>
#include <object/cnode.h>

struct tcb_queue {
    tcb_t *head;
    tcb_t *end;
};
typedef struct tcb_queue tcb_queue_t;

void tcbSchedEnqueue(tcb_t *tcb);
void tcbSchedAppend(tcb_t *tcb);
void tcbSchedDequeue(tcb_t *tcb);

tcb_queue_t tcbEPAppend(tcb_t *tcb, tcb_queue_t queue);
tcb_queue_t tcbEPDequeue(tcb_t *tcb, tcb_queue_t queue);
tcb_queue_t tcbEPReorder(tcb_t *tcb, tcb_queue_t queue, prio_t oldPrio);

void setupCallerCap(tcb_t *sender, tcb_t *receiver, sched_context_t *donated);
void deleteCallerCap(tcb_t *receiver);

unsigned int copyMRs(tcb_t *sender, word_t *sendBuf, tcb_t *receiver,
                     word_t *recvBuf, unsigned int n);
exception_t decodeTCBInvocation(word_t label, unsigned int length, cap_t cap,
                                cte_t* slot, extra_caps_t extraCaps, bool_t call,
                                word_t *buffer);

exception_t decodeCopyRegisters(cap_t cap, unsigned int length,
                                extra_caps_t extraCaps, word_t *buffer);
exception_t decodeReadRegisters(cap_t cap, unsigned int length, bool_t call,
                                word_t *buffer);
exception_t decodeWriteRegisters(cap_t cap, unsigned int length, word_t *buffer);
exception_t decodeTCBConfigure(cap_t cap, unsigned int length,
                               cte_t* slot, extra_caps_t rootCaps, word_t *buffer);

exception_t decodeClearSchedContext(cap_t cap);
exception_t decodeSetSchedContext(cap_t cap, extra_caps_t extraCaps);
exception_t decodeSetPriority(cap_t cap, unsigned int length, word_t *buffer);
exception_t decodeSetMaxPriority(cap_t cap, unsigned int length, word_t *buffer);
exception_t decodeSetIPCBuffer(cap_t cap, unsigned int length,
                               cte_t* slot, extra_caps_t extraCaps, word_t *buffer);
exception_t decodeSetSpace(cap_t cap, unsigned int length,
                           cte_t* slot, extra_caps_t extraCaps, word_t *buffer);
exception_t decodeBindAEP(cap_t cap, extra_caps_t extraCaps);
exception_t decodeUnbindAEP(cap_t cap);
exception_t decodeDomainInvocation(word_t label, unsigned int length,
                                   extra_caps_t extraCaps, word_t *buffer);

enum thread_control_flag {
    thread_control_update_priority = 0x1,
    thread_control_update_ipc_buffer = 0x2,
    thread_control_update_space = 0x4,
    thread_control_update_sc = 0x8,
    thread_control_update_tcb = 0x7,
    thread_control_update_all = 0xF,
};

typedef uint32_t thread_control_flag_t;

exception_t invokeTCB_Suspend(tcb_t *thread);
exception_t invokeTCB_Resume(tcb_t *thread);

exception_t invokeTCB_ThreadControl(tcb_t *target, cte_t* slot,
                                    cptr_t faultep, cptr_t temporalFep,
                                    tcb_prio_t priority,
                                    cap_t cRoot_newCap, cte_t *cRoot_srcSlot,
                                    cap_t vRoot_newCap, cte_t *vRoot_srcSlot,
                                    word_t bufferAddr, cap_t bufferCap,
                                    cte_t *bufferSrcSlot,
                                    sched_context_t *sched_context,
                                    thread_control_flag_t updateFlags);
exception_t invokeTCB_CopyRegisters(tcb_t *dest, tcb_t *src,
                                    bool_t suspendSource, bool_t resumeTarget,
                                    bool_t transferFrame, bool_t transferInteger,
                                    word_t transferArch);
exception_t invokeTCB_ReadRegisters(tcb_t *src, bool_t suspendSource,
                                    unsigned int n, word_t arch, bool_t call);
exception_t invokeTCB_WriteRegisters(tcb_t *dest, bool_t resumeTarget,
                                     unsigned int n, word_t arch, word_t *buffer);
exception_t invokeTCB_AEPControl(tcb_t *tcb, async_endpoint_t *aepptr);
exception_t invokeTCB_SetSchedContext(tcb_t *tcb, sched_context_t *sched_context);
exception_t invokeTCB_ClearSchedContext(tcb_t *tcb);
cptr_t PURE getExtraCPtr(word_t *bufferPtr, unsigned int i);
void setExtraBadge(word_t *bufferPtr, word_t badge, unsigned int i);

exception_t lookupExtraCaps(tcb_t* thread, word_t *bufferPtr, message_info_t info);

static inline unsigned int
setMR(tcb_t *receiver, word_t* receiveIPCBuffer,
      unsigned int offset, word_t reg)
{
    if (offset >= n_msgRegisters) {
        if (receiveIPCBuffer) {
            receiveIPCBuffer[offset + 1] = reg;
            return offset + 1;
        } else {
            return n_msgRegisters;
        }
    } else {
        setRegister(receiver, msgRegisters[offset], reg);
        return offset + 1;
    }
}
#endif
