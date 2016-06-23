/*
 * Copyright 2014, General Dynamics C4 Systems
 *
 * This software may be distributed and modified according to the terms of
 * the GNU General Public License version 2. Note that NO WARRANTY is provided.
 * See "LICENSE_GPLv2.txt" for details.
 *
 * @TAG(GD_GPL)
 */

#ifndef __ARCH_MACHINE_HARDWARE_DEBUG_H
#define __ARCH_MACHINE_HARDWARE_DEBUG_H

#include <types.h>
#include <api/types.h>
#include <arch/machine/registerset.h>

/* Arch specific setup functions */
bool_t Arch_initHardwareBreakpoints(void);
void Arch_initBreakpointContext(user_breakpoint_state_t *context);
void Arch_breakpointThreadDelete(tcb_t *thread);

/** Save the current thread's debug register context before a context switch.
 *@param dest The register save memory block to use.
 */
void saveBreakpointState(arch_tcb_t *dest);

/** Restore debug register context from a block of memory.
 *@param source The memory block from which to load the register values.
 */
void loadBreakpointState(arch_tcb_t *source);

/** Program the debug registers with values that will disable all breakpoints.
 *
 * This is an optimization for threads that don't use any breakpoints: we won't
 * try to pop all the context from a block of memory, but just unset all the
 * "enable" bits in the registers.
 * @param source The memory block from which to load the register values.
 */
void loadAllDisabledBreakpointState(void);

/** Sets up (and overwrites) the current configuration of a hardware breakpoint.
 * @param bp_num Hardware breakpoint ID. Usually an integer from 0..N.
 * @param vaddr Address that the breakpoint should be triggered by.
 * @param type Type of operation that should trigger the breakpoint.
 * @param size Operand size that should trigger the breakpoint.
 * @param rwx Access type (read/write) that should trigger the breakpoint.
 * @param uds If NULL, this function call will write directly to the hardware
 *            registers.
 *            If non-NULL, 'uds' is assumed to be a pointer to a debug register
 *            context-saving memory block, and this function will write to that
 *            context-saving memory block instead.
 * @return Filled out syscall_error_t for convenient use in decoding
 *         invocations.
 */
syscall_error_t setBreakpoint(arch_tcb_t *uds,
                              uint16_t bp_num,
                              word_t vaddr, word_t type, word_t size, word_t rw);

/** Reads and returns the current configuration of a hardware breakpoint.
 * @param bp_num Hardware breakpoint ID. Usually an integer from 0..N.
 *
 * @return Filled out getBreakpoint_t with the following fields:
 * @param vaddr[out] Address that the breakpoint is set to trigger on.
 * @param type[out] Type of operation that will trigger the breakpoint.
 * @param size[out] operand size that will trigger the breakpoint.
 * @param rw[out] Access type (read/write) that will trigger thr breakpoint.
 * @param uds If NULL, this function call will read directly from the hardware
 *            registers.
 *            If non-NULL, 'uds' is assumed to be a pointer to a debug register
 *            context-saving memory block, and this function will read from that
 *            context-saving memory block instead.
 * @param is_enabled Bool stating whether or not the breakpoint is enabled.
 */
typedef struct _getBreakpoint {
    seL4_Error error;
    word_t vaddr, type, size, rw;
    bool_t is_enabled;
} getBreakpoint_t;

getBreakpoint_t getBreakpoint(arch_tcb_t *uds, uint16_t bp_num);

/** Clears a breakpoint's configuration and disables it.
 * @param bp_num Hardware breakpoint ID. Usually an integer from 0..N.
 * @param uds If NULL, this function call will write directly to the hardware
 *            registers.
 *            If non-NULL, 'uds' is assumed to be a pointer to a debug register
 *            context-saving memory block, and this function will write to that
 *            context-saving memory block instead.
 */
syscall_error_t unsetBreakpoint(arch_tcb_t *uds, uint16_t bp_num);

/** Enables a breakpoint.
 * @param bp_num Hardware breakpoint ID. Usually an integer from 0..N.
 */
syscall_error_t enableBreakpoint(arch_tcb_t *uds, uint16_t bp_num);

/** Disables a breakpoint without clearing its configuration.
 * @param bp_num Hardware breakpoint ID. Usually an integer from 0..N.
 */
syscall_error_t disableBreakpoint(arch_tcb_t *uds, uint16_t bp_num);

/** Returns a boolean for whether or not a breakpoint is enabled.
 * @param bp_num Hardware breakpoint ID. Usually an integer from 0..N.
 */
bool_t breakpointIsEnabled(arch_tcb_t *uds, uint16_t bp_num);

/** Used in fault context, to generate the fault_t sent to userspace.
 *
 * First, checks to see which hardware breakpoint was triggered, and saves
 * the ID of that breakpoint. Secondly, "acknowledges" that breakpoint and
 * resets it to its "inactive" state -- whatever that means for the arch. So on
 * x86, that means clearing the indicator bit in DR6.
 *
 * @param vaddr[out] Virtual address that triggered the breakpoint, whether
 *                   instruction or data. For an I/O breakpoint, this will be
 *                   the I/O port number.
 * @return Negative integer if the hardware says no breakpoint was active, or
 *         other anomally.
 *         Positive integer ID for the breakpoint that was triggered otherwise.
 */
int getAndResetActiveBreakpoint(arch_tcb_t *uc, word_t *vaddr, word_t *reason);
bool_t testAndResetSingleStepException(arch_tcb_t *uds, word_t *vaddr);

typedef struct _configureSingleStepping{
    syscall_error_t syscall_error;
    bool_t bp_consumed_or_released;
} configureSingleStepping_t;

configureSingleStepping_t configureSingleStepping(arch_tcb_t *uc,
                                                  uint8_t bp_num,
                                                  word_t n_instr);

static bool_t
singleStepFaultCounterReady(arch_tcb_t *uc)
{
    /* For a single-step exception, the user may have specified a certain
     * number of instructions to skip over before the next stop-point, so
     * we need to decrement the counter.
     *
     * We will check the counter's value when deciding whether or not to
     * actually send a fault message to userspace.
     */
    if (uc->tcbContext.breakpointState.n_instructions > 0) {
        uc->tcbContext.breakpointState.n_instructions--;
    }
    return uc->tcbContext.breakpointState.n_instructions == 0;
}

static inline void
setBreakpointUsedFlag(arch_tcb_t *uds, uint16_t bp_num)
{
    if (uds != NULL) {
        uds->tcbContext.breakpointState.used_breakpoints_bf |= BIT(bp_num);
    }
}

static inline void
unsetBreakpointUsedFlag(arch_tcb_t *uds, uint16_t bp_num)
{
    if (uds != NULL) {
        uds->tcbContext.breakpointState.used_breakpoints_bf &= ~BIT(bp_num);
    }
}

#endif
