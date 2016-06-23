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
#include <arch/machine.h>
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
static CONST uint32_t
clz64(uint64_t n)
{
    uint32_t upper_n = (uint32_t) (n >> 32llu);
    uint32_t lz = 0;

    if (upper_n != 0) {
        lz += clzl(upper_n);
    }

    return lz + clzl((uint32_t) n);
}

static CONST uint64_t
div64(uint64_t numerator, uint32_t denominator)
{
    uint64_t c;
    uint64_t quotient;
    uint64_t long_denom;

    quotient = 0llu;
    long_denom = (uint64_t) denominator;

    if (unlikely(denominator > numerator)) {
        return 0;
    }

    assert(denominator > 0);

    /* align denominator to numerator */
    c = 32u + clzl(denominator) - clz64(numerator);
    long_denom = long_denom << c;

    /* perform binary long division */
    while (c < UINT64_MAX) {
        if (numerator >= long_denom) {
            numerator -= long_denom;
            quotient |= (1llu << c);
        }
        c--;
        long_denom = long_denom >> 1llu;
    }

    return quotient;
}

BOOT_CODE VISIBLE uint32_t
tsc_init(void)
{
    uint32_t tsc_mhz;
    x86_cpu_identity_t *model_info;

    model_info = x86_cpuid_get_model_info();

    if ((model_info->family == 6 && model_info->model == HASWELL_1_MODEL_ID) ||
            (model_info->family == 6 && model_info->model == IVY_BRIDGE_1_MODEL_ID)) {
        uint64_t info;
        uint32_t ratio;

        /* read tsc freq from the platform info msr */
        info = x86_rdmsr(IA32_PLATFORM_INFO_MSR);
        ratio = (((uint32_t) info) & 0xFF00) >> 8u;
        tsc_mhz = ratio * 100u; // this gives Mhz
    } else {
        /* use the pit to find out the tsc freq */
        time_t old_ticks, new_ticks, diff;
        uint32_t cycles_per_ms;

        pit_init();

        /* wait for pit to wraparound */
        pit_wait_wraparound();

        /* read tsc */
        old_ticks = x86_rdtsc();

        /* measure how many tsc cycles pass while PIT wrapsaround */
        pit_wait_wraparound();

        new_ticks = x86_rdtsc();

        diff = new_ticks - old_ticks;

        /* sanity checks */
        assert((uint32_t) diff == diff);
        assert(new_ticks > old_ticks);

        /* bravo, khz */
        cycles_per_ms = (uint32_t) diff / PIT_WRAPAROUND_MS;

        /* finally, return mhz */
        tsc_mhz = cycles_per_ms / 1000u;
    }

    return tsc_mhz;
}

PURE time_t
getMaxTimerUs(void)
{
    return div64(UINT64_MAX, x86KStscMhz);
}

CONST time_t
getKernelWcetUs(void)
{
    return  10u;
}

PURE ticks_t
getTimerPrecision(void)
{
    return x86KStscMhz;
}

PURE ticks_t
usToTicks(time_t us)
{
    assert(x86KStscMhz > 0);
    assert(us >= getKernelWcetUs() && us <= getMaxTimerUs());
    return us * x86KStscMhz;
}

PURE time_t
ticksToUs(ticks_t ticks)
{
    return div64(ticks, x86KStscMhz);
}

void
ackDeadlineIRQ(void)
{
}

ticks_t
getCurrentTime(void)
{
    return x86_rdtsc();
}

void
setDeadline(ticks_t deadline)
{
    assert(deadline > ksCurrentTime);
    x86_wrmsr(IA32_TSC_DEADLINE_MSR, deadline);
}


