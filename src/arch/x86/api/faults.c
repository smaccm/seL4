/*
 * Copyright 2014, General Dynamics C4 Systems
 *
 * This software may be distributed and modified according to the terms of
 * the GNU General Public License version 2. Note that NO WARRANTY is provided.
 * See "LICENSE_GPLv2.txt" for details.
 *
 * @TAG(GD_GPL)
 */

#include <types.h>
#include <object.h>
#include <machine/io.h>
#include <machine/debug.h>
#include <plat/api/constants.h>
#include <kernel/vspace.h>
#include <api/faults.h>
#include <api/syscall.h>
#include <util.h>

bool_t handleFaultReply(tcb_t *receiver, tcb_t *sender)
{
    seL4_MessageInfo_t tag;
    word_t         label;
    fault_t        fault;
    unsigned int   length;

    /* These lookups are moved inward from doReplyTransfer */
    tag = messageInfoFromWord(getRegister(sender, msgInfoRegister));
    label = seL4_MessageInfo_get_label(tag);
    length = seL4_MessageInfo_get_length(tag);
    fault = receiver->tcbFault;

    switch (fault_get_faultType(fault)) {
    case fault_cap_fault:
        return true;

    case fault_vm_fault:
        return true;

    case fault_temporal:
        copyMessageToRegisters(sender, receiver, temporalMessage, MIN(length, n_temporalMessage));
        return (label == 0);

    case fault_unknown_syscall:
        copyMessageToRegisters(sender, receiver, syscallMessage, MIN(length, n_syscallMessage));
        /* HACK: Copy NextIP to FaultIP because FaultIP will be copied */
        /* back to NextIP later on (and we don't wanna lose NextIP)     */
        setRegister(receiver, FaultIP, getRegister(receiver, NextIP));
        return (label == 0);

    case fault_user_exception:
        copyMessageToRegisters(sender, receiver, exceptionMessage, MIN(length, n_exceptionMessage));
        return (label == 0);

#ifdef CONFIG_HARDWARE_DEBUG_API
    case fault_debug_exception: {
        word_t n_instrs;

        if (fault_debug_exception_get_exceptionReason(fault) != seL4_SingleStep) {
            /* Only single-step replies are required to set message registers.
             */
            return (label == 0);
        }

        if (length < DEBUG_REPLY_N_EXPECTED_REGISTERS) {
            /* A single-step reply doesn't mean much if it isn't composed of the bp
             * number and number of instructions to skip. But even if both aren't
             * set, we can still allow the thread to continue because replying
             * should uniformly resume thread execution, based on the general seL4
             * API model.
             *
             * If it was single-step, but no reply registers were set, just
             * default to skipping 1 and continuing.
             *
             * On x86, bp_num actually doesn't matter for single-stepping
             * because single-stepping doesn't use a hardware register -- it
             * uses EFLAGS.TF.
             */
            n_instrs = 1;
        } else {
            /* If the reply had all expected registers set, proceed as normal */
            n_instrs = getRegister(sender, msgRegisters[0]);
        }

        syscall_error_t res;

        res = Arch_decodeConfigureSingleStepping(&receiver->tcbArch, 0, n_instrs, true);
        if (res.type != seL4_NoError) {
            return false;
        };

        configureSingleStepping(&receiver->tcbArch, 0, n_instrs, true);

        /* Replying will always resume the thread: the only variant behaviour
         * is whether or not the thread will be resumed with stepping still
         * enabled.
         */
        return (label == 0);
    }
#endif

    default:
        fail("Invalid fault");
    }
}

#ifdef DEBUG

void handleKernelException(
    word_t vector,
    word_t errcode,
    word_t ip,
    word_t sp,
    word_t flags,
    word_t cr0,
    word_t cr2,
    word_t cr3,
    word_t cr4
);

extern char kernel_stack_alloc[];

VISIBLE
void handleKernelException(
    word_t vector,
    word_t errcode,
    word_t ip,
    word_t sp,
    word_t flags,
    word_t cr0,
    word_t cr2,
    word_t cr3,
    word_t cr4
)
{
    word_t i;

    printf("\n========== KERNEL EXCEPTION ==========\n");
    printf("Vector:  0x%lx\n", vector);
    printf("ErrCode: 0x%lx\n", errcode);
    printf("IP:      0x%lx\n", ip);
    printf("SP:      0x%lx\n", sp);
    printf("FLAGS:   0x%lx\n", flags);
    printf("CR0:     0x%lx\n", cr0);
    printf("CR2:     0x%lx (page-fault address)\n", cr2);
    printf("CR3:     0x%lx (page-directory physical address)\n", cr3);
    printf("CR4:     0x%lx\n", cr4);
    printf("\nStack Dump:\n");
    for (i = 0; i < 20; i++) {
        word_t UNUSED stack = sp + i * sizeof(word_t);
        printf("*0x%lx == 0x%lx\n", stack, *(word_t*)stack);
    }
    printf("\nHalting...\n");
}

#endif
