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
#include <model/statedata.h>
#include <arch/kernel/lock.h>
#include <arch/machine/fpu.h>
#include <arch/fastpath/fastpath.h>
#include <arch/object/vcpu.h>
#include <arch/kernel/traps.h>
#include <machine/debug.h>
#include <api/syscall.h>

void __attribute__((noreturn)) __attribute__((externally_visible)) slowpath_irq(irq_t irq);
void __attribute__((noreturn)) __attribute__((externally_visible)) restore_user_context(void);
void __attribute__((noreturn)) restore_vmx(void);

void __attribute__((externally_visible)) c_handle_interrupt(int irq, int syscall);
void __attribute__((externally_visible)) c_handle_interrupt(int irq, int syscall)
{
    if (irq == int_unimpl_dev) {
        handleUnimplementedDevice();
    } else if (irq == int_page_fault) {
        /* Error code is in Error. Pull out bit 5, which is whether it was instruction or data */
        handleVMFaultEvent((ksCurThread->tcbArch.tcbContext.registers[Error] >> 4) & 1);
        vm_fault_type_t type = (ksCurThread->tcbArch.tcbContext.registers[Error] >> 4u) & 1u;
#ifdef CONFIG_HARDWARE_DEBUG_API
    } else if (irq == int_debug || irq == int_software_break_request) {
        /* Debug exception */
        handleUserLevelDebugException(irq);
#endif /* CONFIG_HARDWARE_DEBUG_API */
    } else if (irq < int_irq_min) {
        handleUserLevelFault(irq, ksCurThread->tcbArch.tcbContext.registers[Error]);
    } else if (likely(irq < int_trap_min)) {
        x86KScurInterrupt = irq;
        fastpath_irq();
    } else if (irq == int_spurious) {
        /* fall through to restore_user_context and do nothing */
    } else {
        /* Interpret a trap as an unknown syscall */
        /* Adjust FaultIP to point to trapping INT
         * instruction by subtracting 2 */
        int sys_num;
        ksCurThread->tcbArch.tcbContext.registers[FaultIP] -= 2;
        /* trap number is MSBs of the syscall number and the LSBS of EAX */
        sys_num = (irq << 24) | (syscall & 0x00ffffff);
        handleUnknownSyscall(sys_num);
    }
    restore_user_context();
}

void __attribute__((noreturn))
slowpath_irq(irq_t irq)
{
    handleInterruptEntry(irq);
    restore_user_context();
}

void __attribute__((noreturn))
slowpath(syscall_t syscall)
{
    x86KScurInterrupt = -1;
    if (config_set(CONFIG_SYSENTER)) {
        /* increment NextIP to skip sysenter */
        ksCurThread->tcbArch.tcbContext.registers[NextIP] += 2;
    } else {
        /* set FaultIP */
        setRegister(ksCurThread, FaultIP, getRegister(ksCurThread, NextIP) - 2);
    }
#ifdef CONFIG_VTX
    if (syscall == SysVMEnter) {
        vcpu_update_vmenter_state(ksCurThread->tcbArch.vcpu);
        if (ksCurThread->tcbBoundNotification && notification_ptr_get_state(ksCurThread->tcbBoundNotification) == NtfnState_Active) {
            completeSignal(ksCurThread->tcbBoundNotification, ksCurThread);
            setRegister(ksCurThread, msgInfoRegister, 0);
            /* Any guest state that we should return is in the same
             * register position as sent to us, so we can just return
             * and let the user pick up the values they put in */
            restore_user_context();
        } else {
            setThreadState(ksCurThread, ThreadState_RunningVM);
            restore_vmx();
        }
    }
#endif
    /* check for undefined syscall */
    if (unlikely(syscall < SYSCALL_MIN || syscall > SYSCALL_MAX)) {
        handleUnknownSyscall(syscall);
    } else {
        handleSyscall(syscall);
    }
    restore_user_context();
}

void __attribute__((externally_visible)) c_handle_syscall(word_t cptr, word_t msgInfo, syscall_t syscall);
void __attribute__((externally_visible)) c_handle_syscall(word_t cptr, word_t msgInfo, syscall_t syscall)
{
#ifdef FASTPATH
    if (syscall == (syscall_t)SysCall) {
        fastpath_call(cptr, msgInfo);
    } else if (syscall == (syscall_t)SysReplyRecv) {
        fastpath_reply_recv(cptr, msgInfo);
    } else if (syscall == SysSend) {
//        fastpath_signal(cptr);
    }
#endif
    slowpath(syscall);
}
