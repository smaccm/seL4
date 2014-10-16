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

#include <object/structures.h>
#include <object/schedcontext.h>

exception_t
invokeSchedControl_Configure(sched_context_t *sched_context, seL4_CBS cbs, uint64_t p, uint64_t d,
                             uint64_t e, word_t trigger, uint32_t data)
{

    /* convert to ticks */
    sched_context->budget = e * ksTicksPerUs;
    sched_context->period = p * ksTicksPerUs;
    sched_context->deadline = d * ksTicksPerUs;
    sched_context->data = data;

    sched_context_status_ptr_set_trigger(&sched_context->status, trigger);
    sched_context_status_ptr_set_cbs(&sched_context->status, cbs);

    return EXCEPTION_NONE;
}

exception_t
invokeSchedControl_Extend(sched_context_t *sched_context, uint64_t p, uint64_t d, uint64_t e)
{

    /* convert to ticks */
    e = e * ksTicksPerUs;
    p = p * ksTicksPerUs;
    d = d * ksTicksPerUs;

    /* if the sched context currently has active budget, extend it by the difference between
     * the old budget (sched_context->budget) and the new budget (e)
     */
    if (!sched_context_status_get_inReleaseHeap(sched_context->status)) {
        /* update budget */
        if (likely(e >= sched_context->budget)) {
            sched_context->budgetRemaining += (e - sched_context->budget);
        } else {
            sched_context->budgetRemaining -= (sched_context->budget - e);
        }

        /* update period */
        if (unlikely(p >= sched_context->period)) {
            sched_context->nextRelease += (p - sched_context->period);
        } else {
            sched_context->nextRelease -= (sched_context->period - p);
        }
    }

#ifdef CONFIG_EDF
    /* update deadline */
    if (sched_context_status_get_inDeadlineHeap(sched_context->status) &&
            d != sched_context->deadline) {

        deadlineRemove(sched_context);

        if (likely(d > sched_context->deadline)) {
            sched_context->nextDeadline += (d - sched_context->deadline);
        } else {
            sched_context->nextDeadline -= (sched_context->deadline - d);
        }

        deadlineAdd(sched_context);
    }
#endif /* CONFIG_EDF */

    sched_context->budget = e;
    sched_context->period = p;
    sched_context->deadline = d;

    return EXCEPTION_NONE;
}

exception_t
invokeSchedControl_SetCriticality(uint32_t criticality)
{
    ksCriticality = criticality;

    if (tcb_prio_get_criticality(ksCurThread->tcbPriority) < ksCriticality) {
        releaseAdd(ksCurThread->tcbSchedContext);
        rescheduleRequired();
    }

    return EXCEPTION_NONE;
}


static seL4_SchedFlags_t
schedFlagsFromWord(uint32_t word)
{
    seL4_SchedFlags_t flags;
    flags.words[0] = word;
    return flags;
}

exception_t
decodeSchedControl_Configure(unsigned int length, extra_caps_t extra_caps, word_t *buffer)
{
    uint64_t e, p, d;
    uint32_t type, data;
    word_t trigger;
    cap_t targetCap;
    sched_context_t *destSc;
    seL4_SchedFlags_t flags;

    /* Ensure message length valid */
    if (length < 7 || extra_caps.excaprefs[0] == NULL) {
        userError("SchedControl Configure: Truncated message.");
        current_syscall_error.type = seL4_TruncatedMessage;
        return EXCEPTION_SYSCALL_ERROR;
    }

    /* Fetch arguments */
    p = (((uint64_t) getSyscallArg(1, buffer)) << 32llu) + getSyscallArg(0, buffer);
    d = (((uint64_t) getSyscallArg(3, buffer)) << 32llu) + getSyscallArg(2, buffer);
    e = (((uint64_t) getSyscallArg(5, buffer)) << 32llu) + getSyscallArg(4, buffer);

    flags = schedFlagsFromWord(getSyscallArg(6, buffer));
    targetCap = extra_caps.excaprefs[0]->cap;

    type = seL4_SchedFlags_get_cbs(flags);
    trigger = seL4_SchedFlags_get_trigger(flags);
    data = seL4_SchedFlags_get_data(flags);

    /* TODO@alyons this is undefined behaviour :( */
    if ((p * ksTicksPerUs) < p) {
        userError("Integer overflow, p too big -- %llx * %llx = %llx\n",
                  p, ksTicksPerUs, p * ksTicksPerUs);
        current_syscall_error.type = seL4_InvalidArgument;
        return EXCEPTION_SYSCALL_ERROR;
    }

    if (type != seL4_HardCBS && type != seL4_SoftCBS) {
        userError("Invalid CBS type\n");
        current_syscall_error.type = seL4_InvalidArgument;
        return EXCEPTION_SYSCALL_ERROR;
    }

    if (trigger != seL4_EventTriggered && trigger != seL4_TimeTriggered) {
        userError("TCB JoinEDF: invalid trigger.");
        current_syscall_error.type = seL4_InvalidArgument;
        return EXCEPTION_SYSCALL_ERROR;
    }

    if (unlikely(e > p)) {
        userError("Execution requirement cannot be greater than period.");
        current_syscall_error.type = seL4_InvalidArgument;
        return EXCEPTION_SYSCALL_ERROR;
    }

    if (unlikely(cap_get_capType(targetCap) != cap_sched_context_cap)) {
        userError("SchedControl_Configure: destination cap is not a sched_context cap.");
        current_syscall_error.type = seL4_InvalidArgument;
        return EXCEPTION_SYSCALL_ERROR;
    }

    destSc = SCHED_CONTEXT_PTR(cap_sched_context_cap_get_capPtr(targetCap));

    setThreadState(ksCurThread, ThreadState_Restart);
    return invokeSchedControl_Configure(destSc, type, p, d, e, trigger, data);
}

exception_t
decodeSchedControl_Extend(unsigned int length, extra_caps_t extra_caps, word_t *buffer)
{
    uint64_t e, p, d;
    cap_t targetCap;
    sched_context_t *destSc;

    /* Ensure message length valid */
    if (length < 6 || extra_caps.excaprefs[0] == NULL) {
        userError("SchedControl Configure: Truncated message.");
        current_syscall_error.type = seL4_TruncatedMessage;
        return EXCEPTION_SYSCALL_ERROR;
    }

    /* Fetch arguments */
    p = (((uint64_t) getSyscallArg(1, buffer)) << 32llu) + getSyscallArg(0, buffer);
    d = (((uint64_t) getSyscallArg(3, buffer)) << 32llu) + getSyscallArg(2, buffer);
    e = (((uint64_t) getSyscallArg(5, buffer)) << 32llu) + getSyscallArg(4, buffer);

    targetCap = extra_caps.excaprefs[0]->cap;


    /* TODO@alyons this is undefined behaviour :( */
    if ((p * ksTicksPerUs) < p) {
        userError("SchedControl_Extend: Integer overflow, p too big -- %llx * %llx = %llx\n",
                  p, ksTicksPerUs, p * ksTicksPerUs);
        current_syscall_error.type = seL4_InvalidArgument;
        return EXCEPTION_SYSCALL_ERROR;
    }

    if (unlikely(e > p)) {
        userError("SchedControl_Extend: Execution requirement cannot be greater than period.");
        current_syscall_error.type = seL4_InvalidArgument;
        return EXCEPTION_SYSCALL_ERROR;
    }

    if (unlikely(cap_get_capType(targetCap) != cap_sched_context_cap)) {
        userError("SchedControl_Extend: destination cap is not a sched_context cap.");
        current_syscall_error.type = seL4_InvalidArgument;
        return EXCEPTION_SYSCALL_ERROR;
    }

    destSc = SCHED_CONTEXT_PTR(cap_sched_context_cap_get_capPtr(targetCap));

    setThreadState(ksCurThread, ThreadState_Restart);
    return invokeSchedControl_Extend(destSc, p, d, e);
}

exception_t
decodeSchedControl_SetCriticality(unsigned int length, word_t *buffer)
{
    uint32_t criticality;

    if (length < 1) {
        userError("SchedControl_SetCriticality: truncated message (expected 1 argument)");
        current_syscall_error.type = seL4_TruncatedMessage;
        return EXCEPTION_SYSCALL_ERROR;
    }

    criticality = getSyscallArg(0, buffer);
    if (criticality > 0xFFu) {
        userError("Criticality %u too high, max %u\n", criticality, 0xFFu);
        current_syscall_error.type = seL4_InvalidArgument;
        return EXCEPTION_SYSCALL_ERROR;
    }

    setThreadState(ksCurThread, ThreadState_Restart);
    return invokeSchedControl_SetCriticality(criticality);
}

exception_t
decodeSchedControlInvocation(word_t label, unsigned int length,
                             extra_caps_t extraCaps, word_t *buffer)
{

    switch (label) {
    case SchedControlConfigure:
        return decodeSchedControl_Configure(length, extraCaps, buffer);
    case SchedControlExtend:
        return decodeSchedControl_Extend(length, extraCaps, buffer);
    case SchedControlSetCriticality:
        return decodeSchedControl_SetCriticality(length, buffer);
    default:
        userError("SchedControl invocation: Illegal operation attempted.");
        current_syscall_error.type = seL4_IllegalOperation;
        return EXCEPTION_SYSCALL_ERROR;
    }
}

#ifdef CONFIG_EDF_HEAP
/* Heap implementation of the sched context priority queues */

#ifdef DEBUG

/* Recursively check the integrity of the heap by
 * making sure all values are in the correct order
 *
 * @param node root node to check
 * @return the size of the heap
 */
static int
heapCheck(sched_context_t* node)
{
    if (node == NULL) {
        return 0;
    }

    if (node->parent != NULL) {
        assert(node->parent->left == node ||
               node->parent->right == node);
    }

    assert(node->left == NULL ||
           node->priority <= node->left->priority);
    assert(node->right == NULL ||
           node->priority <= node->right->priority);

    return 1 + heapCheck(node->left) + heapCheck(node->right);
}
#endif /* DEBUG */

static inline void
heapFixParents(sched_context_t *parent)
{
    if (parent->left != NULL) {
        parent->left->parent = parent;
    }

    if (parent->right != NULL) {
        parent->right->parent = parent;
    }
}

/**
 * Update the parent pointer to the child by working out
 * if it is the left or right child.
 *
 * @child node to update the parent of
 * @value value to update the parent with
 */
static void
heapMaybeSetParentPointer(sched_context_t *child, sched_context_t *value)
{
    if (child->parent != NULL) {
        if (child->parent->left == child) {
            child->parent->left = value;
        } else {
            assert(child->parent->right == child);
            child->parent->right = value;
        }
    }
}

/* Swap two elements in a heap. Lower must be the child of higher, or
 * they can not direct descendants.*/
static inline void
heapSwap(sc_prio_queue_t *heap, sched_context_t *lower, sched_context_t *higher)
{
    sched_context_t *left, *right, *parent;

    assert(heap != NULL);
    assert(heap->head != NULL);
    assert(lower != NULL);
    assert(higher != NULL);

    /* first case - lower is the left child of higher */
    if (higher->left == lower) {
        heapMaybeSetParentPointer(higher, lower);

        lower->parent = higher->parent;
        higher->parent = lower;

        higher->left = lower->left;
        lower->left = higher;

        right = lower->right;
        lower->right = higher->right;
        higher->right = right;
    } else if (higher->right == lower) {
        heapMaybeSetParentPointer(higher, lower);

        /* second case - lower is the right child of higher */
        lower->parent = higher->parent;
        higher->parent = lower;

        higher->right = lower->right;
        lower->right = higher;

        left = lower->left;
        lower->left = higher->left;
        higher->left = left;

    } else {
        /* third case - they are not directly related */
        heapMaybeSetParentPointer(lower, higher);
        heapMaybeSetParentPointer(higher, lower);

        left = lower->left;
        right = lower->right;
        parent = lower->parent;

        lower->left = higher->left;
        lower->right = higher->right;
        lower->parent = higher->parent;

        higher->left = left;
        higher->right = right;
        higher->parent = parent;
    }

    /* fix parents */
    heapFixParents(lower);
    heapFixParents(higher);

    /* fix up the head of the heap */
    if (lower == heap->head) {
        heap->head = higher;
    } else if (higher == heap->head) {
        heap->head = lower;
    }

}

static void
bubbleUp(sc_prio_queue_t *heap, sched_context_t *unordered)
{

    while (unordered->parent != NULL &&
            unordered->priority < unordered->parent->priority) {
        heapSwap(heap, unordered, unordered->parent);
    }

    /* fix the head if it changed */
    if (unordered->parent == NULL) {
        heap->head = unordered;
    }

}

static void
bubbleDown(sc_prio_queue_t *heap, sched_context_t *unordered)
{
    sched_context_t *smallest;

    /* restore order -- bubble down the node we just displaced */
    while (unordered->left != NULL) {

        smallest = unordered->left;
        if (unordered->right != NULL &&
                unordered->right->priority < unordered->left->priority) {
            smallest = unordered->right;
        }

        if (unordered->priority > smallest->priority) {
            assert(unordered != smallest);
            heapSwap(heap, smallest, unordered);
        } else {
            break;
        }
    }

}

void
edfRemove(sc_prio_queue_t *heap, sched_context_t *toRemove)
{
    int msb;
    sched_context_t *current;

    assert(toRemove != NULL);

    heap->size--;

    if (heap->size == 0) {
        assert(toRemove == heap->head);
        heap->head = NULL;
        return;
    }

    /* Find the bottom right most thread */
    msb = MSB(heap->size + 1) - 1;
    current = heap->head;
    assert(current != NULL);
    while (msb >= 0) {
        assert(current != NULL);
        if ((heap->size + 1) & (1 << msb)) {
            current = current->right;
        } else {
            current = current->left;
        }
        msb--;
    }

    assert(current != NULL);
    /* Swap rightmost node with node to be removed */
    heapSwap(heap, current, toRemove);

    /* coup de grace */
    toRemove->left = NULL;
    toRemove->right = NULL;

    heapMaybeSetParentPointer(toRemove, NULL);
    toRemove->parent = NULL;

    /* Two possible cases: either current ended up lower than it should be or
     * higher than it should be (the former will only occur on arbitrary removal,
     * not decapitation. Optimise for the latter. */
    if (unlikely(current->parent != NULL &&
                 current->priority < current->parent->priority)) {
        bubbleUp(heap, current);
    } else {
        bubbleDown(heap, current);
    }

#ifdef DEBUG
    assert(heapCheck(heap->head) == heap->size);
#endif

}

void
edfEnqueue(sc_prio_queue_t *heap, sched_context_t *toEnqueue)
{
    int msb;
    sched_context_t *current;

    assert(heap != NULL);
    assert(heap->size >= 0);

    assert(toEnqueue != NULL);
    assert(toEnqueue->left == NULL);
    assert(toEnqueue->right == NULL);
    assert(toEnqueue->parent == NULL);


    heap->size++;

    /* the queue was empty */
    if (heap->size == 1) {
        assert(heap->head == NULL);
        heap->head = toEnqueue;
        return;
    }

    current = heap->head;
    assert(current != NULL);

    msb = MSB(heap->size) - 1;
    while (msb >= 1) {
        assert(current != NULL);
        if (heap->size & (1 << msb)) {
            assert(current->right != NULL);
            current = current->right;
        } else {
            assert(current->left != NULL);
            current = current->left;
        }
        msb--;
    }

    assert(current != NULL);
    if ((heap->size & 1) == 0) {
        assert(current->left == NULL);
        current->left = toEnqueue;
    } else {
        assert(current->right == NULL);
        current->right = toEnqueue;
    }
    toEnqueue->parent = current;

    while (toEnqueue->parent != NULL &&
            toEnqueue->priority < toEnqueue->parent->priority) {
        heapSwap(heap, toEnqueue, toEnqueue->parent);
    }
#ifdef DEBUG
    assert(heapCheck(heap->head) == heap->size);
#endif /* DEBUG */

}
#elif defined CONFIG_EDF_LIST
/* List implementation of priority queues */
#ifdef DEBUG
static void
listCheck(sc_prio_queue_t *heap)
{
    sched_context_t *node = heap->head;
    sched_context_t *prev = NULL;

    if (heap->head == NULL) {
        return;
    }

    assert(heap->head->prev == NULL);

    while (node->next != NULL) {
        assert(node->priority <= node->next->priority);
        assert(node->prev == prev);
        prev = node;
        node = node->next;
    }

}
#endif /* DEBUG */

void
edfRemove(sc_prio_queue_t *heap, sched_context_t *toRemove)
{
    assert(toRemove != NULL);

    /* removing the head */
    if (toRemove == heap->head) {
        assert(toRemove->prev == NULL);
        heap->head = toRemove->next;
        if (heap->head != NULL) {
            heap->head->prev = NULL;
        }
    } else {
        toRemove->prev->next = toRemove->next;
        if (toRemove->next != NULL) {
            toRemove->next->prev = toRemove->prev;
        }
    }

    toRemove->prev = NULL;
    toRemove->next = NULL;

#ifdef DEBUG
    assert(heap->head != toRemove);
    listCheck(heap);
#endif
}
void
edfEnqueue(sc_prio_queue_t *heap, sched_context_t *toEnqueue)
{

    assert(heap->head == NULL || heap->head->prev == NULL);
    assert(toEnqueue != NULL);
    assert(toEnqueue->prev == NULL);
    assert(toEnqueue->next == NULL);
    assert(toEnqueue != heap->head);

    if (heap->head == NULL || toEnqueue->priority <= heap->head->priority) {
        /* insert at head */
        toEnqueue->next = heap->head;
        heap->head = toEnqueue;
        toEnqueue->prev = NULL;
        if (toEnqueue->next != NULL) {
            toEnqueue->next->prev = toEnqueue;
        }
    } else {
        /* find a place to insert */
        sched_context_t *node = heap->head;
        sched_context_t *prev = NULL;
        while (node != NULL && toEnqueue->priority > node->priority) {
            prev = node;
            node = node->next;
        }

        /* prev can't be null or we would have taken the first branch */
        assert(prev != NULL);

        toEnqueue->prev = prev;
        toEnqueue->next = node;

        prev->next = toEnqueue;
        if (node != NULL) {
            node->prev = toEnqueue;
        }
    }
#ifdef DEBUG
    listCheck(heap);
#endif
}

#endif /* CONFIG_EDF_LIST */

sched_context_t *
edfDequeue(sc_prio_queue_t *heap)
{
    sched_context_t *head = heap->head;
    assert(head != NULL);
    edfRemove(heap, head);
    assert(heap->head != head);
    return head;
}

void
sched_context_purge(sched_context_t *sc)
{
    /* remove sched context from any queues it is in */
    if (sched_context_status_get_inReleaseHeap(sc->status)) {
        releaseRemove(sc);
#ifdef CONFIG_EDF
    } else if (sched_context_status_get_inDeadlineHeap(sc->status)) {
        deadlineRemove(sc);
#endif /* CONFIG_EDF */
    }

}

