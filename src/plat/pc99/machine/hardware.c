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
#include <machine/io.h>
#include <arch/kernel/apic.h>
#include <arch/model/statedata.h>
#include <arch/linker.h>
#include <plat/machine/pic.h>
#include <plat/machine/ioapic.h>
#include <plat/machine.h>

#include <plat/machine/intel-vtd.h>

BOOT_CODE bool_t platAddDevices(void)
{
    /* remove the MSI region as poking at this is undefined and may allow for
     * the user to generate arbitrary MSI interrupts. Only need to consider
     * this if it would actually be in the user device region */
    if (PADDR_USER_DEVICE_TOP > 0xFFFFFFF8) {
        if (!add_allocated_p_region( (p_region_t) {
        (word_t)0xFFFFFFF8, (word_t)0xFFFFFFF8 + 8
        })) {
            return false;
        }
    }
    return true;
}

/* ============================== interrupts/IRQs ============================== */

/* Handle a platform-reserved IRQ. */
void handleReservedIRQ(irq_t irq)
{
    if (config_set(CONFIG_IOMMU) && irq == irq_iommu) {
        vtd_handle_fault();
        return;
    }
}

/* Get the IRQ number currently working on. */
irq_t getActiveIRQ(void)
{
    if (x86KScurInterrupt == int_invalid) {
        return irqInvalid;
    } else {
        return x86KScurInterrupt - IRQ_INT_OFFSET;
    }
}

/* Checks for pending IRQ */
bool_t isIRQPending(void)
{
    if (apic_is_interrupt_pending()) {
        return true;
    }
#ifdef CONFIG_IRQ_PIC
    if (pic_is_irq_pending()) {
        return true;
    }
#endif
    return false;
}

void ackInterrupt(irq_t irq)
{
#ifdef CONFIG_IRQ_PIC
    if (irq <= irq_isa_max) {
        pic_ack_active_irq();
    } else
#endif
    {
        apic_ack_active_interrupt();
    }
}

void handleSpuriousIRQ(void)
{
    /* Do nothing */
}

/* ============================== timer ============================== */

void resetTimer(void)
{
    /* not necessary */
}
