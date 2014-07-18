/*
 * Copyright 2014, General Dynamics C4 Systems
 *
 * This software may be distributed and modified according to the terms of
 * the GNU General Public License version 2. Note that NO WARRANTY is provided.
 * See "LICENSE_GPLv2.txt" for details.
 *
 * @TAG(GD_GPL)
 */

#ifndef __OBJECT_ASYNCENDPOINT_H
#define __OBJECT_ASYNCENDPOINT_H

#include <types.h>
#include <object/structures.h>

void sendAsyncIPC(async_endpoint_t *aepptr, word_t badge);
void receiveAsyncIPC(tcb_t *thread, cap_t cap, bool_t isBlocking);
void aepCancelAll(async_endpoint_t *aepptr);
void asyncIPCCancel(tcb_t *threadPtr, async_endpoint_t *aepptr);
void completeAsyncIPC(async_endpoint_t *aepptr, tcb_t *tcb);
void unbindAsyncEndpoint(tcb_t *tcb);
void bindAsyncEndpoint(tcb_t *tcb, async_endpoint_t *aepptr);
void doAsyncTransfer(async_endpoint_t *aepptr, tcb_t *tcb, word_t badge);

static inline tcb_queue_t PURE
aep_ptr_get_queue(async_endpoint_t *aepptr)
{
    tcb_queue_t aep_queue;

    aep_queue.head = (tcb_t*)async_endpoint_ptr_get_aepQueue_head(aepptr);
    aep_queue.end = (tcb_t*)async_endpoint_ptr_get_aepQueue_tail(aepptr);

    return aep_queue;
}

static inline void
aep_ptr_set_queue(async_endpoint_t *aepptr, tcb_queue_t aep_queue)
{
    async_endpoint_ptr_set_aepQueue_head(aepptr, (word_t)aep_queue.head);
    async_endpoint_ptr_set_aepQueue_tail(aepptr, (word_t)aep_queue.end);
}


#endif
