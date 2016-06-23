/*
 * Copyright 2016, General Dynamics C4 Systems
 *
 * This software may be distributed and modified according to the terms of
 * the GNU General Public License version 2. Note that NO WARRANTY is provided.
 * See "LICENSE_GPLv2.txt" for details.
 *
 * @TAG(GD_GPL)
 */
#include <arch/model/statedata.h>
#include <arch/machine/debug.h>
#include <arch/machine.h>
#include <machine/registerset.h>
#include <api/constants.h> /* seL4_NumBreakpoints */

#define X86_DEBUG_BP_N_REGS                 (seL4_NumBreakpoints)

/* Intel manual Vol3, 17.2.4 */
#define X86_DEBUG_BP_SIZE_1B                (0x0u)
#define X86_DEBUG_BP_SIZE_2B                (0x1u)
#define X86_DEBUG_BP_SIZE_4B                (0x3u)
#define X86_DEBUG_BP_SIZE_8B                (0x2u)

#define X86_DEBUG_BP0_SIZE_SHIFT            (18)
#define X86_DEBUG_BP1_SIZE_SHIFT            (22)
#define X86_DEBUG_BP2_SIZE_SHIFT            (26)
#define X86_DEBUG_BP3_SIZE_SHIFT            (30)

/* NOTE: Intel manual 17.2.4:
 * I/O breakpoints are supported by every processor later than i486, but only
 * when CR4.DE=1.
 * When CR4.DE=0, or if processor is earlier than i586, this bit is "Undefined",
 * which is not the same as "Reserved", so it won't trigger an exception - it
 * will just cause an undefined reaction from the CPU.
 */
#define X86_DEBUG_BP_TYPE_IO                (0x2u)
#define X86_DEBUG_BP_TYPE_INSTR             (0x0u)
#define X86_DEBUG_BP_TYPE_DATA_WRITE        (0x1u)
#define X86_DEBUG_BP_TYPE_DATA_READWRITE    (0x3u)

#define X86_DEBUG_BP0_TYPE_SHIFT            (16)
#define X86_DEBUG_BP1_TYPE_SHIFT            (20)
#define X86_DEBUG_BP2_TYPE_SHIFT            (24)
#define X86_DEBUG_BP3_TYPE_SHIFT            (28)

/* Bit in DR7 that will enable each BP respectively. */
#define X86_DEBUG_BP0_ENABLE_BIT  ((word_t)BIT(1))
#define X86_DEBUG_BP1_ENABLE_BIT  ((word_t)BIT(3))
#define X86_DEBUG_BP2_ENABLE_BIT  ((word_t)BIT(5))
#define X86_DEBUG_BP3_ENABLE_BIT  ((word_t)BIT(7))

#define X86_DEBUG_EFLAGS_TRAP_FLAG    ((word_t)BIT(8))
#define X86_DEBUG_EFLAGS_RESUME_FLAG    ((word_t)BIT(16))
#define X86_DEBUG_DR6_SINGLE_STEP_FLAG  ((word_t)BIT(14))

#define X86_DEBUG_DR6_BP_MASK     (0xFu)

#ifdef CONFIG_ARCH_X86_64
    #define PTR_INCREMENT_NBYTES      "8"
#else
    #define PTR_INCREMENT_NBYTES      "4"
#endif

static bool_t byte8BreakpointsSupported = false;

static inline word_t
readDr6(arch_tcb_t *uds)
{
    if (uds == NULL) {
        word_t dr6;

        asm volatile(
            "movl %%dr6, %0 \n\t"
            : "=r" (dr6));
        return dr6;
    } else {
        return uds->tcbContext.breakpointState.dr[4];
    }
}

static inline void
bitwiseAndDr6(arch_tcb_t *uds, word_t mask)
{
    if (uds == NULL) {
        asm volatile(
            "movl %%dr6, %%edx \n\t"
            "andl %0, %%edx \n\t"
            "movl %%edx, %%dr6 \n\t"
            :
            : "r" (mask)
            : "edx");
    } else {
        uds->tcbContext.breakpointState.dr[4] &= mask;
    }
}

static inline word_t
readDr7(arch_tcb_t *uds)
{
    return uds->tcbContext.breakpointState.dr[5];
}

static inline void
bitwiseOrDr7(arch_tcb_t *uds, word_t val)
{
    uds->tcbContext.breakpointState.dr[5] |= val;
}

static inline void
bitwiseAndDr7(arch_tcb_t *uds, word_t mask)
{
    uds->tcbContext.breakpointState.dr[5] &= mask;
}

static void
unsetDr7BitsFor(arch_tcb_t *uds, uint16_t bp_num)
{
    word_t mask;

    switch (bp_num) {
    case 0:
        mask = (0x3u << X86_DEBUG_BP0_SIZE_SHIFT) | (0x3u << X86_DEBUG_BP0_TYPE_SHIFT);
        break;
    case 1:
        mask = (0x3u << X86_DEBUG_BP1_SIZE_SHIFT) | (0x3u << X86_DEBUG_BP1_TYPE_SHIFT);
        break;
    case 2:
        mask = (0x3u << X86_DEBUG_BP2_SIZE_SHIFT) | (0x3u << X86_DEBUG_BP2_TYPE_SHIFT);
        break;
    default:
        mask = (0x3u << X86_DEBUG_BP3_SIZE_SHIFT) | (0x3u << X86_DEBUG_BP3_TYPE_SHIFT);
        break;
    }

    mask = ~mask;
    bitwiseAndDr7(uds, mask);
}

/** Converts an seL4_BreakpointType value into the underlying hardware
 * equivalent.
 * @param bp_num Breakpoint number.
 * @param type One of the values of seL4_BreakpointType.
 * @param rw Access trigger condition (read/write).
 * @return Hardware specific register value representing the inputs.
 */
static inline word_t
convertTypeAndAccessToArch(uint16_t bp_num, word_t type, word_t rw)
{
    switch (type) {
    case seL4_IOBreakpoint:
        type = X86_DEBUG_BP_TYPE_IO;
        break;
    case seL4_InstructionBreakpoint:
        type = X86_DEBUG_BP_TYPE_INSTR;
        break;
    default:
        /* seL4_DataBreakpoint */
        type = (rw == seL4_BreakOnWrite)
            ? X86_DEBUG_BP_TYPE_DATA_WRITE
            : X86_DEBUG_BP_TYPE_DATA_READWRITE;
    }

    switch (bp_num) {
    case 0:
        return type << X86_DEBUG_BP0_TYPE_SHIFT;
    case 1:
        return type << X86_DEBUG_BP1_TYPE_SHIFT;
    case 2:
        return type << X86_DEBUG_BP2_TYPE_SHIFT;
    default:
        return type << X86_DEBUG_BP3_TYPE_SHIFT;
    }
}

/** Reverse of convertTypeAndAccessToArch(): converts hardware values into
 * seL4 API values.
 * @param dr7 Hardware register value as input for conversion.
 * @param bp_num Breakpoint number.
 * @param type[out] Converted type value.
 * @param rw[out] Converted output access trigger value.
 */
typedef struct _convertedTypeAndAccess {
    word_t type, rw;
} convertedTypeAndAccess_t;

static inline convertedTypeAndAccess_t
convertArchToTypeAndAccess(word_t dr7, uint16_t bp_num)
{
    convertedTypeAndAccess_t ret;

    switch (bp_num) {
    case 0:
        dr7 &= 0x3u << X86_DEBUG_BP0_TYPE_SHIFT;
        dr7 >>= X86_DEBUG_BP0_TYPE_SHIFT;
        break;
    case 1:
        dr7 &= 0x3u << X86_DEBUG_BP1_TYPE_SHIFT;
        dr7 >>= X86_DEBUG_BP1_TYPE_SHIFT;
        break;
    case 2:
        dr7 &= 0x3u << X86_DEBUG_BP2_TYPE_SHIFT;
        dr7 >>= X86_DEBUG_BP2_TYPE_SHIFT;
        break;
    default:
        dr7 &= 0x3u << X86_DEBUG_BP3_TYPE_SHIFT;
        dr7 >>= X86_DEBUG_BP3_TYPE_SHIFT;
    }

    switch (dr7) {
    case X86_DEBUG_BP_TYPE_INSTR:
        ret.type = seL4_InstructionBreakpoint;
        ret.rw = seL4_BreakOnRead;
        break;
    case X86_DEBUG_BP_TYPE_DATA_WRITE:
        ret.type = seL4_DataBreakpoint;
        ret.rw = seL4_BreakOnWrite;
        break;
    case X86_DEBUG_BP_TYPE_DATA_READWRITE:
        ret.type = seL4_DataBreakpoint;
        ret.rw = seL4_BreakOnReadWrite;
        break;
    default:
        /* X86_DEBUG_BP_TYPE_IO */
        ret.type = seL4_IOBreakpoint;
        ret.rw = seL4_BreakOnReadWrite;
        break;
    }
    return ret;
}

/** Converts an integer size number into an equivalent hardware register value.
 * @param n Breakpoint number.
 * @param type One value from seL4_BreakpointType.
 * @param size An integer for the operand size of the breakpoint.
 * @return Converted, hardware-specific value.
 */
static inline word_t
convertSizeToArch(uint16_t bp_num, word_t type, word_t size)
{
    if (type == seL4_InstructionBreakpoint) {
        /* Intel manual vol3 17.2.4:
         * "If the corresponding RWn field in register DR7 is 00 (instruction
         * execution), then the LENn field should also be 00"
         */
        size = 0;
    } else {
        switch (size) {
        case 1:
            size = X86_DEBUG_BP_SIZE_1B;
            break;
        case 2:
            size = X86_DEBUG_BP_SIZE_2B;
            break;
        case 8:
            size = X86_DEBUG_BP_SIZE_8B;
            break;
        default: /* 4B */
            size = X86_DEBUG_BP_SIZE_4B;
        }
    }

    switch (bp_num) {
    case 0:
        return size << X86_DEBUG_BP0_SIZE_SHIFT;
    case 1:
        return size << X86_DEBUG_BP1_SIZE_SHIFT;
    case 2:
        return size << X86_DEBUG_BP2_SIZE_SHIFT;
    default:
        return size << X86_DEBUG_BP3_SIZE_SHIFT;
    }
}

/** Reverse of convertSizeToArch(): converts a hardware-specific size value
 * into an integer representation.
 * @param dr7 Hardware register value as input.
 * @param n Breakpoint number.
 * @return Converted size value.
 */
static inline word_t
convertArchToSize(word_t dr7, uint16_t bp_num)
{
    word_t type;

    switch (bp_num) {
    case 0:
        type = dr7 & (0x3u << X86_DEBUG_BP0_TYPE_SHIFT);
        type >>= X86_DEBUG_BP0_TYPE_SHIFT;
        dr7 &= 0x3u << X86_DEBUG_BP0_SIZE_SHIFT;
        dr7 >>= X86_DEBUG_BP0_SIZE_SHIFT;
        break;
    case 1:
        type = dr7 & (0x3u << X86_DEBUG_BP1_TYPE_SHIFT);
        type >>= X86_DEBUG_BP1_TYPE_SHIFT;
        dr7 &= 0x3u << X86_DEBUG_BP1_SIZE_SHIFT;
        dr7 >>= X86_DEBUG_BP1_SIZE_SHIFT;
        break;
    case 2:
        type = dr7 & (0x3u << X86_DEBUG_BP2_TYPE_SHIFT);
        type >>= X86_DEBUG_BP2_TYPE_SHIFT;
        dr7 &= 0x3u << X86_DEBUG_BP2_SIZE_SHIFT;
        dr7 >>= X86_DEBUG_BP2_SIZE_SHIFT;
        break;
    default:
        type = dr7 & (0x3u << X86_DEBUG_BP3_TYPE_SHIFT);
        type >>= X86_DEBUG_BP3_TYPE_SHIFT;
        dr7 &= 0x3u << X86_DEBUG_BP3_SIZE_SHIFT;
        dr7 >>= X86_DEBUG_BP3_SIZE_SHIFT;
    }

    /* Force size to 0 if type is instruction breakpoint. */
    if (type == X86_DEBUG_BP_TYPE_INSTR) {
        return 0;
    }

    switch (dr7) {
    case X86_DEBUG_BP_SIZE_1B:
        return 1;
    case X86_DEBUG_BP_SIZE_2B:
        return 2;
    case X86_DEBUG_BP_SIZE_8B:
        return 8;
    default: /* 4B */
        return 4;
    }
}

typedef void (setBreakpointVaddrFn)(user_breakpoint_state_t *uds, word_t vaddr);
static setBreakpointVaddrFn setBp0, setBp1, setBp2, setBp3;
setBreakpointVaddrFn *set_breakpoint_vaddr_fns[] = {
    &setBp0, &setBp1, &setBp2, &setBp3
};

syscall_error_t
setBreakpoint(arch_tcb_t *uds,
              uint16_t bp_num, word_t vaddr, word_t types, word_t size, word_t rw)
{
    syscall_error_t ret;
    word_t dr7val = 0;

    if (bp_num >= X86_DEBUG_BP_N_REGS) {
        ret.rangeErrorMin = 0;
        ret.rangeErrorMax = 3;
        ret.type = seL4_RangeError;
        return ret;
    }
    if (size == 8 && !byte8BreakpointsSupported) {
        ret.invalidArgumentNumber = 3;
        ret.type = seL4_InvalidArgument;
        return ret;
    }

    dr7val = convertTypeAndAccessToArch(bp_num, types, rw);
    dr7val |= convertSizeToArch(bp_num, types, size);

    disableBreakpoint(uds, bp_num);
    set_breakpoint_vaddr_fns[bp_num](&uds->tcbContext.breakpointState, vaddr);
    unsetDr7BitsFor(uds, bp_num);
    bitwiseOrDr7(uds, dr7val);
    enableBreakpoint(uds, bp_num);
    ret.type = seL4_NoError;
    return ret;
}

typedef word_t (getBreakpointVaddrFn)(user_breakpoint_state_t *uds);
static getBreakpointVaddrFn getBp0, getBp1, getBp2, getBp3;
getBreakpointVaddrFn *get_breakpoint_vaddr_fns[] = {
    &getBp0, &getBp1, &getBp2, &getBp3
};

int
getAndResetActiveBreakpoint(arch_tcb_t *uc, word_t *vaddr, word_t *reason)
{
    int ret;
    convertedTypeAndAccess_t tmp;

    /* Read from the hardware regs, not user context */
    word_t dr6 = readDr6(NULL);
    if (dr6 & BIT(0)) {
        ret = 0;
    } else if (dr6 & BIT(1)) {
        ret = 1;
    } else if (dr6 & BIT(2)) {
        ret = 2;
    } else if (dr6 & BIT(3)) {
        ret = 3;
    } else {
        return -1;
    }

    tmp = convertArchToTypeAndAccess(readDr7(uc), ret);
    *vaddr = get_breakpoint_vaddr_fns[ret](NULL);
    *reason = (tmp.type == seL4_InstructionBreakpoint)
        ? seL4_DebugException_Breakpoint
        : seL4_DebugException_Watchpoint;

    bitwiseAndDr6(NULL, ~(1 << ret));
    return ret;
}

getBreakpoint_t
getBreakpoint(arch_tcb_t *uds, uint16_t bp_num)
{
    word_t dr7val;
    getBreakpoint_t ret;
    convertedTypeAndAccess_t res;

    if (bp_num >= X86_DEBUG_BP_N_REGS) {
        ret.error = seL4_IllegalOperation;
        return ret;
    }

    dr7val = readDr7(uds);
    ret.vaddr = get_breakpoint_vaddr_fns[bp_num](&uds->tcbContext.breakpointState);
    ret.size = convertArchToSize(dr7val, bp_num);
    res = convertArchToTypeAndAccess(dr7val, bp_num);
    ret.type = res.type;
    ret.rw = res.rw;
    ret.error = seL4_NoError;
    return ret;
}

syscall_error_t
unsetBreakpoint(arch_tcb_t *uds, uint16_t bp_num)
{
    syscall_error_t ret;

    if (bp_num >= X86_DEBUG_BP_N_REGS) {
        ret.rangeErrorMin = 0;
        ret.rangeErrorMax = 3;
        ret.type = seL4_RangeError;
        return ret;
    }

    disableBreakpoint(uds, bp_num);
    unsetDr7BitsFor(uds, bp_num);
    set_breakpoint_vaddr_fns[bp_num](&uds->tcbContext.breakpointState, 0);
    ret.type = seL4_NoError;
    return ret;
}

syscall_error_t
enableBreakpoint(arch_tcb_t *uds, uint16_t bp_num)
{
    syscall_error_t ret;
    word_t enable_bit;

    if (bp_num >= X86_DEBUG_BP_N_REGS) {
        ret.rangeErrorMin = 0;
        ret.rangeErrorMax = 3;
        ret.type = seL4_RangeError;
        return ret;
    }

    switch (bp_num) {
    case 0:
        enable_bit = X86_DEBUG_BP0_ENABLE_BIT;
        break;
    case 1:
        enable_bit = X86_DEBUG_BP1_ENABLE_BIT;
        break;
    case 2:
        enable_bit = X86_DEBUG_BP2_ENABLE_BIT;
        break;
    default:
        enable_bit = X86_DEBUG_BP3_ENABLE_BIT;
        break;
    }

    bitwiseOrDr7(uds, enable_bit);
    ret.type = seL4_NoError;
    return ret;
}

syscall_error_t
disableBreakpoint(arch_tcb_t *uds, uint16_t bp_num)
{
    syscall_error_t ret;
    word_t disable_mask;

    if (bp_num >= X86_DEBUG_BP_N_REGS) {
        ret.rangeErrorMin = 0;
        ret.rangeErrorMax = 3;
        ret.type = seL4_RangeError;
        return ret;
    }

    switch (bp_num) {
    case 0:
        disable_mask = ~X86_DEBUG_BP0_ENABLE_BIT;
        break;
    case 1:
        disable_mask = ~X86_DEBUG_BP1_ENABLE_BIT;
        break;
    case 2:
        disable_mask = ~X86_DEBUG_BP2_ENABLE_BIT;
        break;
    default:
        disable_mask = ~X86_DEBUG_BP3_ENABLE_BIT;
        break;
    }

    bitwiseAndDr7(uds, disable_mask);
    ret.type = seL4_NoError;
    return ret;
}

bool_t
breakpointIsEnabled(arch_tcb_t *uds, uint16_t bp_num)
{
    word_t dr7;

    if (bp_num >= X86_DEBUG_BP_N_REGS) {
        return false;
    }

    dr7 = readDr7(uds);
    switch (bp_num) {
    case 0:
        return !!(dr7 & X86_DEBUG_BP0_ENABLE_BIT);
    case 1:
        return !!(dr7 & X86_DEBUG_BP1_ENABLE_BIT);
    case 2:
        return !!(dr7 & X86_DEBUG_BP2_ENABLE_BIT);
    default:
        return !!(dr7 & X86_DEBUG_BP3_ENABLE_BIT);
    }
}

bool_t
testAndResetSingleStepException(arch_tcb_t *uc, word_t *vaddr)
{
    bool_t ret=false;
    word_t dr6;

    dr6 = readDr6(NULL);
    if (dr6 & X86_DEBUG_DR6_SINGLE_STEP_FLAG) {
        ret = true;
    }

    *vaddr = uc->tcbContext.registers[FaultIP];
    bitwiseAndDr6(NULL, ~X86_DEBUG_DR6_SINGLE_STEP_FLAG);

    /* And that's not all: if the breakpoint is an instruction breakpoint, we
     * also need to set EFLAGS.RF. The processor raises the #DB exception BEFORE
     * the instruction executes. This means that when we IRET to userspace, the
     * SAME breakpoint will trigger again, and so on ad infinitum. EFLAGS.RF
     * solves this problem:
     *
     * When EFLAGS.RF is set, the processor will ignore instruction breakpoints
     * that should be raised, for one instruction. After that instruction
     * executes, the processor will also automatically unset EFLAGS.RF. See
     * Intel manuals, vol3, section 17.3.1.1.
     */
    /* This will automatically be popped by restore_user_context() */
    uc->tcbContext.registers[EFLAGS] |= X86_DEBUG_EFLAGS_RESUME_FLAG;
    return ret;
}

configureSingleStepping_t
configureSingleStepping(arch_tcb_t *uc,
                        uint8_t bp_num, word_t n_instr)
{
    (void)bp_num;
    configureSingleStepping_t ret;

    /* If n_instr (number of instructions to single-step) is 0, that is the
     * same as requesting that single-stepping be disabled.
     */
    if (n_instr == 0) {
        uc->tcbContext.registers[EFLAGS] &= ~X86_DEBUG_EFLAGS_TRAP_FLAG;
    } else {
        uc->tcbContext.registers[EFLAGS] |= X86_DEBUG_EFLAGS_TRAP_FLAG;
    }

    uc->tcbContext.breakpointState.n_instructions = n_instr;
    /* On x86 no hardware breakpoint are needed for single stepping. */
    ret.bp_consumed_or_released = false;
    ret.syscall_error.type = seL4_NoError;
    return ret;
}

static word_t
getBp0(user_breakpoint_state_t *uds)
{
    if (uds == NULL) {
        word_t ret;

        asm volatile(
            "movl %%dr0, %0 \n\t"
            : "=r" (ret));
        return ret;
    } else {
        return uds->dr[0];
    }
}

static word_t
getBp1(user_breakpoint_state_t *uds)
{
    if (uds == NULL) {
        word_t ret;

        asm volatile(
            "movl %%dr1, %0 \n\t"
            : "=r" (ret));
        return ret;
    } else {
        return uds->dr[1];
    }
}

static word_t
getBp2(user_breakpoint_state_t *uds)
{
    if (uds == NULL) {
        word_t ret;

        asm volatile(
            "movl %%dr2, %0 \n\t"
            : "=r" (ret));
        return ret;
    } else {
        return uds->dr[2];
    }
}

static word_t
getBp3(user_breakpoint_state_t *uds)
{
    if (uds == NULL) {
        word_t ret;

        asm volatile(
            "movl %%dr3, %0 \n\t"
            : "=r" (ret));
        return ret;
    } else {
        return uds->dr[3];
    }
}

static void
setBp0(user_breakpoint_state_t *uds, word_t vaddr)
{
    uds->dr[0] = vaddr;
}

static void
setBp1(user_breakpoint_state_t *uds, word_t vaddr)
{
    uds->dr[1] = vaddr;
}

static void
setBp2(user_breakpoint_state_t *uds, word_t vaddr)
{
    uds->dr[2] = vaddr;
}

static void
setBp3(user_breakpoint_state_t *uds, word_t vaddr)
{
    uds->dr[3] = vaddr;
}

BOOT_CODE bool_t
Arch_initHardwareBreakpoints(void)
{
    x86_cpu_identity_t *modelinfo;

    modelinfo = x86_cpuid_get_model_info();
    /* Intel manuals, vol3, section 17.2.4, "NOTES". */
    if (modelinfo->family == 15) {
        if (modelinfo->model == 3 || modelinfo->model == 4
            || modelinfo->model == 6) {
            byte8BreakpointsSupported = true;
        }
    }
    if (modelinfo->family == 6) {
        if (modelinfo->model == 15 || modelinfo->model == 23
            || modelinfo->model == 0x1C) {
            byte8BreakpointsSupported = true;
        }
    }
    return true;
}

void
Arch_initBreakpointContext(user_breakpoint_state_t *uds)
{
    *uds = x86KSnullBreakpointState;
}

void loadBreakpointState(arch_tcb_t *source)
{
    /* Order does matter when restoring the registers: we want to restore the
     * breakpoint control register (DR7) last since it is what "activates" the
     * effects of the configuration described by the other registers.
     */
    asm volatile (
        "movl %0, %%edx \n\t"
        "movl (%%edx), %%ecx \n\t"
        "movl %%ecx, %%dr0 \n\t"
        "addl $"PTR_INCREMENT_NBYTES", %%edx \n\t"
        "movl (%%edx), %%ecx \n\t"
        "movl %%ecx, %%dr1 \n\t"
        "addl $"PTR_INCREMENT_NBYTES", %%edx \n\t"
        "movl (%%edx), %%ecx \n\t"
        "movl %%ecx, %%dr2 \n\t"
        "addl $"PTR_INCREMENT_NBYTES", %%edx \n\t"
        "movl (%%edx), %%ecx \n\t"
        "movl %%ecx, %%dr3 \n\t"
        "addl $"PTR_INCREMENT_NBYTES", %%edx \n\t"
        "movl (%%edx), %%ecx \n\t"
        "movl %%ecx, %%dr6 \n\t"
        "addl $"PTR_INCREMENT_NBYTES", %%edx \n\t"
        "movl (%%edx), %%ecx \n\t"
        "movl %%ecx, %%dr7 \n\t"
        :
        : "r" (source->tcbContext.breakpointState.dr)
        : "edx", "ecx");
}

void loadAllDisabledBreakpointState(void)
{
    word_t mask;

    mask = ~(X86_DEBUG_BP0_ENABLE_BIT | X86_DEBUG_BP1_ENABLE_BIT
            | X86_DEBUG_BP2_ENABLE_BIT | X86_DEBUG_BP3_ENABLE_BIT);

    /* Just disable all the breakpoints. This allows us to do context switches
     * without having to */
    asm volatile(
        "movl %%dr7, %%edx \n\t"
        "andl %0, %%edx \n\t"
        "movl %%edx, %%dr7 \n\t"
        :
        : "r" (mask)
        : "edx");
}
