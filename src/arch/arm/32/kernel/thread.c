/*
 * Copyright 2014, General Dynamics C4 Systems
 *
 * This software may be distributed and modified according to the terms of
 * the GNU General Public License version 2. Note that NO WARRANTY is provided.
 * See "LICENSE_GPLv2.txt" for details.
 *
 * @TAG(GD_GPL)
 */

#include <object.h>
#include <machine.h>
#include <arch/model/statedata.h>
#include <arch/kernel/vspace.h>
#include <arch/kernel/thread.h>
#include <arch/linker.h>

void
Arch_switchToThread(tcb_t *tcb)
{
    setVMRoot(tcb);
    *armKSGlobalsFrame = tcb->tcbIPCBuffer;
    clearExMonitor();
}

BOOT_CODE void
Arch_configureIdleThread(tcb_t *tcb)
{
    setRegister(tcb, CPSR, PMODE_IDLE);
    setRegister(tcb, LR_svc, (word_t)idleThreadStart);
}

void
Arch_switchToIdleThread(void)
{
    *armKSGlobalsFrame = 0;
}

