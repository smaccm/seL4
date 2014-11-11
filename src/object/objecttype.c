/*
 * Copyright 2014, General Dynamics C4 Systems
 *
 * This software may be distributed and modified according to the terms of
 * the GNU General Public License version 2. Note that NO WARRANTY is provided.
 * See "LICENSE_GPLv2.txt" for details.
 *
 * @TAG(GD_GPL)
 */

#include <assert.h>
#include <config.h>
#include <types.h>
#include <api/failures.h>
#include <api/syscall.h>
#include <arch/object/objecttype.h>
#include <machine/io.h>
#include <object/objecttype.h>
#include <object/structures.h>
#include <object/asyncendpoint.h>
#include <object/endpoint.h>
#include <object/cnode.h>
#include <object/interrupt.h>
#include <object/tcb.h>
#include <object/schedcontext.h>
#include <object/untyped.h>
#include <model/preemption.h>
#include <model/statedata.h>
#include <kernel/thread.h>
#include <kernel/vspace.h>
#include <machine.h>
#include <util.h>

word_t getObjectSize(word_t t, word_t userObjSize)
{
    if (t >= seL4_NonArchObjectTypeCount) {
        return Arch_getObjectSize(t);
    } else {
        switch (t) {
        case seL4_TCBObject:
            return TCB_BLOCK_SIZE_BITS;
        case seL4_EndpointObject:
            return EP_SIZE_BITS;
        case seL4_AsyncEndpointObject:
            return AEP_SIZE_BITS;
        case seL4_CapTableObject:
            return CTE_SIZE_BITS + userObjSize;
        case seL4_UntypedObject:
            return userObjSize;
        case seL4_SchedContextObject:
            return SCHED_CONTEXT_SIZE_BITS;
        default:
            fail("Invalid object type");
            return 0;
        }
    }
}

deriveCap_ret_t
deriveCap(cte_t *slot, cap_t cap)
{
    deriveCap_ret_t ret;

    if (isArchCap(cap)) {
        return Arch_deriveCap(slot, cap);
    }

    switch (cap_get_capType(cap)) {
    case cap_zombie_cap:
        ret.status = EXCEPTION_NONE;
        ret.cap = cap_null_cap_new();
        break;

    case cap_irq_control_cap:
        ret.status = EXCEPTION_NONE;
        ret.cap = cap_null_cap_new();
        break;

    case cap_untyped_cap:
        ret.status = ensureNoChildren(slot);
        if (ret.status != EXCEPTION_NONE) {
            ret.cap = cap_null_cap_new();
        } else {
            ret.cap = cap;
        }
        break;

    case cap_reply_cap:
        ret.status = EXCEPTION_NONE;
        ret.cap = cap_null_cap_new();
        break;

    default:
        ret.status = EXCEPTION_NONE;
        ret.cap = cap;
    }

    return ret;
}

finaliseCap_ret_t
finaliseCap(cap_t cap, bool_t final, bool_t exposed)
{
    finaliseCap_ret_t fc_ret;

    if (isArchCap(cap)) {
        fc_ret.remainder = Arch_finaliseCap(cap, final);
        fc_ret.irq = irqInvalid;
        return fc_ret;
    }

    switch (cap_get_capType(cap)) {
    case cap_endpoint_cap:
        if (final) {
            epCancelAll(EP_PTR(cap_endpoint_cap_get_capEPPtr(cap)));
        }

        fc_ret.remainder = cap_null_cap_new();
        fc_ret.irq = irqInvalid;
        return fc_ret;

    case cap_async_endpoint_cap:
        if (final) {
            async_endpoint_t *aep = AEP_PTR(cap_async_endpoint_cap_get_capAEPPtr(cap));
            tcb_t *boundTCB = (tcb_t*)async_endpoint_ptr_get_aepBoundTCB(aep);;

            if (boundTCB) {
                unbindAsyncEndpoint(boundTCB);
            }

            aepCancelAll(aep);
        }
        fc_ret.remainder = cap_null_cap_new();
        fc_ret.irq = irqInvalid;
        return fc_ret;

    case cap_reply_cap: {
        sched_context_t *sc = SC_PTR(cap_reply_cap_get_schedcontext(cap));
        if (sc) {
            sc->replySlot = NULL;
        }
        fc_ret.remainder = cap_null_cap_new();
        fc_ret.irq = irqInvalid;
        return fc_ret;
    }
    case cap_null_cap:
    case cap_domain_cap:
        fc_ret.remainder = cap_null_cap_new();
        fc_ret.irq = irqInvalid;
        return fc_ret;
    }

    if (exposed) {
        fail("finaliseCap: failed to finalise immediately.");
    }

    switch (cap_get_capType(cap)) {
    case cap_cnode_cap: {
        if (final) {
            fc_ret.remainder =
                Zombie_new(
                    1 << cap_cnode_cap_get_capCNodeRadix(cap),
                    cap_cnode_cap_get_capCNodeRadix(cap),
                    cap_cnode_cap_get_capCNodePtr(cap)
                );
            fc_ret.irq = irqInvalid;
            return fc_ret;
        }
        break;
    }

    case cap_thread_cap: {
        if (final) {
            tcb_t *tcb;
            cte_t *cte_ptr;
            sched_context_t *sc;

            tcb = TCB_PTR(cap_thread_cap_get_capTCBPtr(cap));
            cte_ptr = TCB_PTR_CTE_PTR(tcb, tcbCTable);
            sc = tcb->tcbSchedContext;

            if (sc != NULL) {
                if (sc->home == tcb || sc->home == NULL) {
                    /* sched context is home and tcb is suspended.
                     * take the sched context out of the relevant queues */
                    sched_context_purge(sc);
                } else {
                    cte_t *callerSlot = TCB_PTR_CTE_PTR(tcb, tcbCaller);
                    cap_t callerCap = callerSlot->cap;
                    uint32_t capType = cap_get_capType(callerCap);

                    if (capType == cap_reply_cap && !cap_reply_cap_get_capReplyMaster(callerCap)) {
                        /* tcb has valid reply cap, return the sc there */
                        tcb_t *caller = TCB_PTR(cap_reply_cap_get_capTCBPtr(callerCap));
                        caller->tcbSchedContext = sc;
                        sc->tcb = caller;

                        setThreadState(caller, ThreadState_Running);
                        if (isSchedulable(caller)) {
                            attemptSwitchTo(caller, false);
                        }

                    } else {
                        /* send tcb home */
                        sc->tcb = sc->home;
                        sc->tcb->tcbSchedContext = sc;
                        if (isSchedulable(sc->home)) {
                            attemptSwitchTo(sc->home, false);
                        }
                    }
                }
            }

            unbindAsyncEndpoint(tcb);
            suspend(tcb);

            if (tcb->tcbSchedContext != NULL) {
                tcb->tcbSchedContext->tcb = NULL;
                tcb->tcbSchedContext = NULL;
            }

            if (tcb->tcbHomeSchedContext != NULL) {
                tcb->tcbHomeSchedContext->tcb = NULL;
                tcb->tcbHomeSchedContext = NULL;
            }

            Arch_prepareThreadDelete(tcb);
            fc_ret.remainder =
                Zombie_new(
                    tcbCNodeEntries,
                    ZombieType_ZombieTCB,
                    CTE_REF(cte_ptr)
                );
            fc_ret.irq = irqInvalid;
            return fc_ret;
        }
        break;
    }

    case cap_zombie_cap:
        fc_ret.remainder = cap;
        fc_ret.irq = irqInvalid;
        return fc_ret;
    case cap_sched_context_cap:
        if (final) {
            sched_context_t *sc = SCHED_CONTEXT_PTR(cap_sched_context_cap_get_capPtr(cap));
            if (sc->tcb != NULL) {
                suspend(sc->tcb);
                sc->tcb->tcbSchedContext = NULL;
                sc->tcb = NULL;
            }
            if (sc->home != NULL) {
                sc->home->tcbHomeSchedContext = NULL;
                sc->home = NULL;
            }

            if (sc->replySlot) {
                cap_reply_cap_ptr_set_schedcontext(&sc->replySlot->cap, 0);
            }
            return fc_ret;
        }
        break;
    case cap_irq_handler_cap:
        if (final) {
            irq_t irq = cap_irq_handler_cap_get_capIRQ(cap);

            deletingIRQHandler(irq);

            fc_ret.remainder = cap_null_cap_new();
            fc_ret.irq = irq;
            return fc_ret;
        }
        break;
    }

    fc_ret.remainder = cap_null_cap_new();
    fc_ret.irq = irqInvalid;
    return fc_ret;
}

cap_t
recycleCap(bool_t is_final, cap_t cap)
{
    if (isArchCap(cap)) {
        return Arch_recycleCap(is_final, cap);
    }

    switch (cap_get_capType(cap)) {
    case cap_null_cap:
        fail("recycleCap: can't reconstruct Null");
        break;
    case cap_domain_cap:
        return cap;
    case cap_cnode_cap:
        return cap;
    case cap_thread_cap:
        return cap;
    case cap_zombie_cap: {
        word_t type;

        type = cap_zombie_cap_get_capZombieType(cap);
        if (type == ZombieType_ZombieTCB) {
            tcb_t *tcb;
            _thread_state_t ts UNUSED;

            tcb = TCB_PTR(cap_zombie_cap_get_capZombiePtr(cap)
                          + TCB_OFFSET);
            ts = thread_state_get_tsType(tcb->tcbState);
            /* Haskell error:
             * "Zombie cap should point at inactive thread" */
            assert(ts == ThreadState_Inactive ||
                   ts != ThreadState_IdleThreadState);
            /* Haskell error:
             * "Zombie cap should not point at queued thread" */
            assert(!thread_state_get_tcbQueued(tcb->tcbState));
            /* Haskell error:
             * "Zombie cap should not point at bound thread" */
            assert(tcb->boundAsyncEndpoint == NULL);

            assert(tcb->tcbSchedContext == NULL);
            /* makeObject doesn't exist in C, objects are initialised by
             * zeroing. The effect of recycle in Haskell is to reinitialise
             * the TCB, with the exception of the TCB CTEs.  I achieve this
             * here by zeroing the TCB part of the structure, while leaving
             * the CNode alone. */
            memzero(tcb, sizeof (tcb_t));
            Arch_initContext(&tcb->tcbArch.tcbContext);
            tcb->tcbTimeSlice = CONFIG_TIME_SLICE;
            tcb->tcbDomain = ksCurDomain;

            return cap_thread_cap_new(TCB_REF(tcb));
        } else {
            return cap_cnode_cap_new(type, 0, 0,
                                     cap_zombie_cap_get_capZombiePtr(cap));
        }
    }
    case cap_endpoint_cap: {
        word_t badge = cap_endpoint_cap_get_capEPBadge(cap);
        if (badge) {
            endpoint_t* ep = (endpoint_t*)
                             cap_endpoint_cap_get_capEPPtr(cap);
            epCancelBadgedSends(ep, badge);
        }
        return cap;
    }
    default:
        return cap;
    }
}

bool_t CONST
hasRecycleRights(cap_t cap)
{
    switch (cap_get_capType(cap)) {
    case cap_null_cap:
    case cap_domain_cap:
        return false;

    case cap_endpoint_cap:
        return cap_endpoint_cap_get_capCanSend(cap) &&
               cap_endpoint_cap_get_capCanReceive(cap) &&
               cap_endpoint_cap_get_capCanGrant(cap);

    case cap_async_endpoint_cap:
        return cap_async_endpoint_cap_get_capAEPCanSend(cap) &&
               cap_async_endpoint_cap_get_capAEPCanReceive(cap);

    default:
        if (isArchCap(cap)) {
            return Arch_hasRecycleRights(cap);
        } else {
            return true;
        }
    }
}

bool_t CONST
sameRegionAs(cap_t cap_a, cap_t cap_b)
{
    switch (cap_get_capType(cap_a)) {
    case cap_untyped_cap: {
        word_t aBase, bBase, aTop, bTop;

        aBase = (word_t)WORD_PTR(cap_untyped_cap_get_capPtr(cap_a));
        bBase = (word_t)cap_get_capPtr(cap_b);

        aTop = aBase + MASK(cap_untyped_cap_get_capBlockSize(cap_a));
        bTop = bBase + MASK(cap_get_capSizeBits(cap_b));

        return ((bBase != 0) && (aBase <= bBase) &&
                (bTop <= aTop) && (bBase <= bTop));
    }

    case cap_endpoint_cap:
        if (cap_get_capType(cap_b) == cap_endpoint_cap) {
            return cap_endpoint_cap_get_capEPPtr(cap_a) ==
                   cap_endpoint_cap_get_capEPPtr(cap_b);
        }
        break;

    case cap_async_endpoint_cap:
        if (cap_get_capType(cap_b) == cap_async_endpoint_cap) {
            return cap_async_endpoint_cap_get_capAEPPtr(cap_a) ==
                   cap_async_endpoint_cap_get_capAEPPtr(cap_b);
        }
        break;

    case cap_cnode_cap:
        if (cap_get_capType(cap_b) == cap_cnode_cap) {
            return (cap_cnode_cap_get_capCNodePtr(cap_a) ==
                    cap_cnode_cap_get_capCNodePtr(cap_b)) &&
                   (cap_cnode_cap_get_capCNodeRadix(cap_a) ==
                    cap_cnode_cap_get_capCNodeRadix(cap_b));
        }
        break;

    case cap_thread_cap:
        if (cap_get_capType(cap_b) == cap_thread_cap) {
            return cap_thread_cap_get_capTCBPtr(cap_a) ==
                   cap_thread_cap_get_capTCBPtr(cap_b);
        }
        break;

    case cap_reply_cap:
        if (cap_get_capType(cap_b) == cap_reply_cap) {
            return cap_reply_cap_get_capTCBPtr(cap_a) ==
                   cap_reply_cap_get_capTCBPtr(cap_b);
        }
        break;

    case cap_domain_cap:
        if (cap_get_capType(cap_b) == cap_domain_cap) {
            return true;
        }
        break;

    case cap_irq_control_cap:
        if (cap_get_capType(cap_b) == cap_irq_control_cap ||
                cap_get_capType(cap_b) == cap_irq_handler_cap) {
            return true;
        }
        break;

    case cap_irq_handler_cap:
        if (cap_get_capType(cap_b) == cap_irq_handler_cap) {
            return (irq_t)cap_irq_handler_cap_get_capIRQ(cap_a) ==
                   (irq_t)cap_irq_handler_cap_get_capIRQ(cap_b);
        }
        break;

    case cap_sched_context_cap:
        if (cap_get_capType(cap_b) == cap_sched_context_cap) {
            return cap_sched_context_cap_get_capPtr(cap_a) ==
                   cap_sched_context_cap_get_capPtr(cap_b);
        }
        break;

    default:
        if (isArchCap(cap_a) &&
                isArchCap(cap_b)) {
            return Arch_sameRegionAs(cap_a, cap_b);
        }
        break;
    }

    return false;
}

bool_t CONST
sameObjectAs(cap_t cap_a, cap_t cap_b)
{
    if (cap_get_capType(cap_a) == cap_untyped_cap) {
        return false;
    }
    if (cap_get_capType(cap_a) == cap_irq_control_cap &&
            cap_get_capType(cap_b) == cap_irq_handler_cap) {
        return false;
    }
    if (isArchCap(cap_a) && isArchCap(cap_b)) {
        return Arch_sameObjectAs(cap_a, cap_b);
    }
    return sameRegionAs(cap_a, cap_b);
}

cap_t CONST
updateCapData(bool_t preserve, word_t newData, cap_t cap)
{
    if (isArchCap(cap)) {
        return Arch_updateCapData(preserve, newData, cap);
    }

    switch (cap_get_capType(cap)) {
    case cap_endpoint_cap:
        if (!preserve && cap_endpoint_cap_get_capEPBadge(cap) == 0) {
            return cap_endpoint_cap_set_capEPBadge(cap, newData);
        } else {
            return cap_null_cap_new();
        }

    case cap_async_endpoint_cap:
        if (!preserve && cap_async_endpoint_cap_get_capAEPBadge(cap) == 0) {
            return cap_async_endpoint_cap_set_capAEPBadge(cap, newData);
        } else {
            return cap_null_cap_new();
        }

    case cap_cnode_cap: {
        word_t guard, guardSize;
        cnode_capdata_t w = { .words = { newData } };

        guardSize = cnode_capdata_get_guardSize(w);

        if (guardSize + cap_cnode_cap_get_capCNodeRadix(cap) > wordBits) {
            return cap_null_cap_new();
        } else {
            cap_t new_cap;

            guard = cnode_capdata_get_guard(w) & MASK(guardSize);
            new_cap = cap_cnode_cap_set_capCNodeGuard(cap, guard);
            new_cap = cap_cnode_cap_set_capCNodeGuardSize(new_cap,
                                                          guardSize);

            return new_cap;
        }
    }

    default:
        return cap;
    }
}

cap_t CONST
maskCapRights(cap_rights_t cap_rights, cap_t cap)
{
    if (isArchCap(cap)) {
        return Arch_maskCapRights(cap_rights, cap);
    }

    switch (cap_get_capType(cap)) {
    case cap_null_cap:
    case cap_domain_cap:
    case cap_cnode_cap:
    case cap_untyped_cap:
    case cap_reply_cap:
    case cap_irq_control_cap:
    case cap_irq_handler_cap:
    case cap_zombie_cap:
    case cap_thread_cap:
    case cap_sched_context_cap:
    case cap_sched_control_cap:
        return cap;

    case cap_endpoint_cap: {
        cap_t new_cap;

        new_cap = cap_endpoint_cap_set_capCanSend(
                      cap, cap_endpoint_cap_get_capCanSend(cap) &
                      cap_rights_get_capAllowWrite(cap_rights));
        new_cap = cap_endpoint_cap_set_capCanReceive(
                      new_cap, cap_endpoint_cap_get_capCanReceive(cap) &
                      cap_rights_get_capAllowRead(cap_rights));
        new_cap = cap_endpoint_cap_set_capCanGrant(
                      new_cap, cap_endpoint_cap_get_capCanGrant(cap) &
                      cap_rights_get_capAllowGrant(cap_rights));

        return new_cap;
    }

    case cap_async_endpoint_cap: {
        cap_t new_cap;

        new_cap = cap_async_endpoint_cap_set_capAEPCanSend(
                      cap, cap_async_endpoint_cap_get_capAEPCanSend(cap) &
                      cap_rights_get_capAllowWrite(cap_rights));
        new_cap = cap_async_endpoint_cap_set_capAEPCanReceive(new_cap,
                                                              cap_async_endpoint_cap_get_capAEPCanReceive(cap) &
                                                              cap_rights_get_capAllowRead(cap_rights));

        return new_cap;
    }

    default:
        fail("Invalid cap type"); /* Sentinel for invalid enums */
    }
}

cap_t
createObject(object_t t, void *regionBase, word_t userSize)
{
    /* Handle architecture-specific objects. */
    if (t >= (object_t) seL4_NonArchObjectTypeCount) {
        return Arch_createObject(t, regionBase, userSize);
    }

    /* Create objects. */
    switch ((api_object_t)t) {
    case seL4_TCBObject: {
        tcb_t *tcb;
        memzero(regionBase, 1UL << TCB_BLOCK_SIZE_BITS);
        tcb = TCB_PTR((word_t)regionBase + TCB_OFFSET);
        /** AUXUPD: "(True, ptr_retyps 5
          (Ptr ((ptr_val \<acute>tcb) - 0x100) :: cte_C ptr)
            o (ptr_retyp \<acute>tcb))" */

        /* Setup non-zero parts of the TCB. */

        Arch_initContext(&tcb->tcbArch.tcbContext);
        tcb->tcbTimeSlice = CONFIG_TIME_SLICE;
        tcb->tcbDomain = ksCurDomain;

        return cap_thread_cap_new(TCB_REF(tcb));
    }

    case seL4_EndpointObject:
        memzero(regionBase, 1UL << EP_SIZE_BITS);
        /** AUXUPD: "(True, ptr_retyp
          (Ptr (ptr_val \<acute>regionBase) :: endpoint_C ptr))" */
        return cap_endpoint_cap_new(0, true, true, true,
                                    EP_REF(regionBase));

    case seL4_AsyncEndpointObject:
        memzero(regionBase, 1UL << AEP_SIZE_BITS);
        /** AUXUPD: "(True, ptr_retyp
              (Ptr (ptr_val \<acute>regionBase) :: async_endpoint_C ptr))" */
        return cap_async_endpoint_cap_new(0, true, true,
                                          AEP_REF(regionBase));

    case seL4_CapTableObject:
        memzero(regionBase, 1UL << (CTE_SIZE_BITS + userSize));
        /** AUXUPD: "(True, ptr_retyps (2 ^ (unat \<acute>userSize))
          (Ptr (ptr_val \<acute>regionBase) :: cte_C ptr))" */
        /** GHOSTUPD: "(True, gs_new_cnodes (unat \<acute>userSize)
                                (ptr_val \<acute>regionBase)
                                (4 + unat \<acute>userSize))" */
        return cap_cnode_cap_new(userSize, 0, 0, CTE_REF(regionBase));

    case seL4_UntypedObject:
        /*
         * No objects need to be created; instead, just insert caps into
         * the destination slots.
         */
        return cap_untyped_cap_new(0, userSize, WORD_REF(regionBase));
    case seL4_SchedContextObject:
        memzero(regionBase, 1UL << SCHED_CONTEXT_SIZE_BITS);
        return cap_sched_context_cap_new(SCHED_CONTEXT_REF(regionBase));
    default:
        fail("Invalid object type");
    }
}

void
createNewObjects(object_t t, cte_t *parent, slot_range_t slots,
                 void *regionBase, word_t userSize)
{
    word_t objectSize;
    void *nextFreeArea;
    unsigned int i;

    /* Create the objects. */
    nextFreeArea = regionBase;
    objectSize = getObjectSize(t, userSize);
    for (i = 0; i < slots.length; i++) {
        /* Create the object. */
        /** AUXUPD: "(True, typ_region_bytes (ptr_val \<acute> nextFreeArea + ((\<acute> i) << unat (\<acute> objectSize))) (unat (\<acute> objectSize)))" */
        cap_t cap = createObject(t, (void *)((word_t)nextFreeArea + (i << objectSize)), userSize);

        /* Insert the cap into the user's cspace. */
        insertNewCap(parent, &slots.cnode[slots.offset + i], cap);

        /* Move along to the next region of memory. been merged into a formula of i */
    }
}

exception_t
decodeInvocation(word_t label, unsigned int length,
                 cptr_t capIndex, cte_t *slot, cap_t cap,
                 extra_caps_t extraCaps, bool_t block, bool_t call,
                 word_t *buffer)
{
    if (isArchCap(cap)) {
        return Arch_decodeInvocation(label, length, capIndex,
                                     slot, cap, extraCaps, buffer);
    }

    switch (cap_get_capType(cap)) {
    case cap_null_cap:
        userError("Attempted to invoke a null cap #%u.", capIndex);
        current_syscall_error.type = seL4_InvalidCapability;
        current_syscall_error.invalidCapNumber = 0;
        return EXCEPTION_SYSCALL_ERROR;

    case cap_zombie_cap:
        userError("Attempted to invoke a zombie cap #%u.", capIndex);
        current_syscall_error.type = seL4_InvalidCapability;
        current_syscall_error.invalidCapNumber = 0;
        return EXCEPTION_SYSCALL_ERROR;

    case cap_endpoint_cap:
        if (unlikely(!cap_endpoint_cap_get_capCanSend(cap))) {
            userError("Attempted to invoke a read-only endpoint cap #%u.",
                      capIndex);
            current_syscall_error.type = seL4_InvalidCapability;
            current_syscall_error.invalidCapNumber = 0;
            return EXCEPTION_SYSCALL_ERROR;
        }

        setThreadState(ksCurThread, ThreadState_Restart);
        return performInvocation_Endpoint(
                   EP_PTR(cap_endpoint_cap_get_capEPPtr(cap)),
                   cap_endpoint_cap_get_capEPBadge(cap),
                   cap_endpoint_cap_get_capCanGrant(cap), block, call);

    case cap_async_endpoint_cap: {
        if (unlikely(!cap_async_endpoint_cap_get_capAEPCanSend(cap))) {
            userError("Attempted to invoke a read-only async-endpoint cap #%u.",
                      capIndex);
            current_syscall_error.type = seL4_InvalidCapability;
            current_syscall_error.invalidCapNumber = 0;
            return EXCEPTION_SYSCALL_ERROR;
        }

        setThreadState(ksCurThread, ThreadState_Restart);
        return performInvocation_AsyncEndpoint(
                   AEP_PTR(cap_async_endpoint_cap_get_capAEPPtr(cap)),
                   cap_async_endpoint_cap_get_capAEPBadge(cap));
    }

    case cap_reply_cap:
        if (unlikely(cap_reply_cap_get_capReplyMaster(cap))) {
            userError("Attempted to invoke an invalid reply cap #%u.",
                      capIndex);
            current_syscall_error.type = seL4_InvalidCapability;
            current_syscall_error.invalidCapNumber = 0;
            return EXCEPTION_SYSCALL_ERROR;
        }

        setThreadState(ksCurThread, ThreadState_Restart);
        return performInvocation_Reply(
                   TCB_PTR(cap_reply_cap_get_capTCBPtr(cap)), slot, cap);

    case cap_thread_cap:
        return decodeTCBInvocation(label, length, cap,
                                   slot, extraCaps, call, buffer);

    case cap_domain_cap:
        return decodeDomainInvocation(label, length, extraCaps, buffer);

    case cap_cnode_cap:
        return decodeCNodeInvocation(label, length, cap, extraCaps, buffer);

    case cap_untyped_cap:
        return decodeUntypedInvocation(label, length, slot, cap, extraCaps,
                                       call, buffer);

    case cap_irq_control_cap:
        return decodeIRQControlInvocation(label, length, slot,
                                          extraCaps, buffer);

    case cap_irq_handler_cap:
        return decodeIRQHandlerInvocation(label, length,
                                          cap_irq_handler_cap_get_capIRQ(cap), extraCaps, buffer);

    case cap_sched_control_cap:
        return decodeSchedControlInvocation(label, length, extraCaps, buffer);

    case cap_sched_context_cap:
        /* there are no direct sched context cap invocations */
        userError("Invalid capability type\n");
        return EXCEPTION_SYSCALL_ERROR;

    default:
        fail("Invalid cap type");
    }
}

exception_t
performInvocation_Endpoint(endpoint_t *ep, word_t badge,
                           bool_t canGrant, bool_t block,
                           bool_t call)
{
    sendIPC(block, call, call, badge, canGrant, ksCurThread, ep);

    return EXCEPTION_NONE;
}

exception_t
performInvocation_AsyncEndpoint(async_endpoint_t *aep, word_t badge)
{
    sendAsyncIPC(aep, badge);

    return EXCEPTION_NONE;
}

exception_t
performInvocation_Reply(tcb_t *thread, cte_t *slot, cap_t cap)
{
    doReplyTransfer(ksCurThread, thread, slot, cap);
    return EXCEPTION_NONE;
}
