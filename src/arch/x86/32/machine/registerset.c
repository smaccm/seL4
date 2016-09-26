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
#include <arch/machine/registerset.h>
#include <arch/machine/fpu.h>
#include <arch/object/structures.h>
#include <machine/debug.h>

const register_t msgRegisters[] = {
    EDI, EBP
};

const register_t frameRegisters[] = {
    FaultIP, ESP, EFLAGS, EAX, EBX, ECX, EDX, ESI, EDI, EBP
};

const register_t gpRegisters[] = {
    TLS_BASE, FS, GS
};

const register_t exceptionMessage[] = {
    FaultIP, ESP, EFLAGS
};

const register_t syscallMessage[] = {
    EAX, EBX, ECX, EDX, ESI, EDI, EBP, NextIP, ESP, EFLAGS
};

const register_t temporalMessage[] = {
    FaultIP, ESP, EFLAGS, EAX, EBX, ECX, EDX, ESI, EDI, EBP, TLS_BASE, FS, GS
};

#ifdef CONFIG_VTX
const register_t crExitRegs[] = {
    EAX, ECX, EDX, EBX, ESP, EBP, ESI, EDI
};
#endif

void Arch_initContext(user_context_t* context)
{
    context->registers[EAX] = 0;
    context->registers[EBX] = 0;
    context->registers[ECX] = 0;
    context->registers[EDX] = 0;
    context->registers[ESI] = 0;
    context->registers[EDI] = 0;
    context->registers[EBP] = 0;
    context->registers[DS] = SEL_DS_3;
    context->registers[ES] = SEL_DS_3;
    context->registers[FS] = SEL_NULL;
    context->registers[GS] = SEL_NULL;
    context->registers[TLS_BASE] = 0;
    context->registers[Error] = 0;
    context->registers[FaultIP] = 0;
    context->registers[NextIP] = 0;            /* overwritten by setNextPC() later on */
    context->registers[CS] = SEL_CS_3;
    context->registers[EFLAGS] = BIT(9) | BIT(1); /* enable interrupts and set bit 1 which is always 1 */
    context->registers[ESP] = 0;                /* userland has to set it after entry */
    context->registers[SS] = SEL_DS_3;

    Arch_initFpuContext(context);
#ifdef CONFIG_HARDWARE_DEBUG_API
    Arch_initBreakpointContext(&context->breakpointState);
#endif
}

word_t sanitiseRegister(register_t reg, word_t v)
{
    if (reg == EFLAGS) {
        v |=  BIT(1);   /* reserved bit that must be set to 1 */
        v &= ~BIT(3);   /* reserved bit that must be set to 0 */
        v &= ~BIT(5);   /* reserved bit that must be set to 0 */
#ifdef CONFIG_HARDWARE_DEBUG_API
        /* Disallow setting Trap Flag: use the API instead */
        v &= ~BIT(X86_EFLAGS_TRAP_FLAG_SHIFT);
#endif
        v |=  BIT(9);   /* interrupts must be enabled in userland */
        v &=  MASK(12); /* bits 12:31 have to be 0 */
    }
    if (reg == FS || reg == GS) {
        if (v != SEL_TLS && v != SEL_IPCBUF) {
            v = 0;
        }
    }
    return v;
}
