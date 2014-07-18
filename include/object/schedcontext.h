/*
 * Copyright 2014, General Dynamics C4 Systems
 *
 * This software may be distributed and modified according to the terms of
 * the GNU General Public License version 2. Note that NO WARRANTY is provided.
 * See "LICENSE_GPLv2.txt" for details.
 *
 * @TAG(GD_GPL)
 */

#ifndef __OBJECT_SCHED_CTRL_H
#define __OBJECT_SCHED_CTRL_H

#include <config.h>

#include <types.h>

#include <object/structures.h>

#define PRINT_CALLER do {\
    int caller = 0; \
    asm volatile ( \
        "mov %0, lr\n"\
        : "=r" (caller)\
        :\
        :\
        );\
    printf("%s was called by %x\n", __FUNCTION__, caller);\
} while (0)


struct sc_prio_queue {
    sched_context_t *head;
#ifdef CONFIG_EDF_HEAP
    word_t size;
#endif /* CONFIG_EDF_HEAP */
};
typedef struct sc_prio_queue sc_prio_queue_t;

exception_t decodeSchedControlInvocation(word_t label, unsigned int length,
                                         extra_caps_t extra_caps, word_t *buffer);


exception_t invokeSchedControl(sched_context_t *sched_context, uint32_t type, uint64_t p, uint64_t d,
                               uint64_t e, uint64_t r, word_t trigger);
exception_t decodeSchedControl_Configure(unsigned int length, extra_caps_t extra_caps, word_t *buffer);

/*
 * Remove an arbitrary memory of a sched context priority queue
 */
void edfRemove(sc_prio_queue_t *heap, sched_context_t *toRemove);

/*
 * Remove the head of a sched context priority queue.
 * @return the removed head
 */
sched_context_t *edfDequeue(sc_prio_queue_t *heap);

/*
 * Add a thread to a priority queue
 */
void edfEnqueue(sc_prio_queue_t *heap, sched_context_t *toEnqueue);

/*
 * Remove a scheduling context from any queues it is in
 */
void sched_context_purge(sched_context_t *sc);
#endif /* __OBJECT_SCHED_CTRL_H */




