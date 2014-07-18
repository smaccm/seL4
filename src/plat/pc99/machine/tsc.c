/*
 * Copyright 2014, General Dynamics C4 Systems
 *
 * This software may be distributed and modified according to the terms of
 * the GNU General Public License version 2. Note that NO WARRANTY is provided.
 * See "LICENSE_GPLv2.txt" for details.
 *
 * @TAG(GD_GPL)
 */


#include <plat/machine/pit.h>
#include <plat/machine.h>

static inline uint64_t
rdtsc(void)
{
    uint32_t hi, lo;

    __asm__ __volatile__ (
        "rdtsc"
        : "=a" (lo),
        "=d" (hi)
    );

    return ((uint64_t) hi) << 32llu | (uint64_t) lo;
}

PHYS_CODE VISIBLE uint32_t
tsc_init(void)
{
    uint64_t old_ticks, new_ticks, diff;
    uint32_t cycles_per_ms;

    /* wait for pit to wraparound */
    pit_wait_wraparound();

    /* read tsc */
    old_ticks = rdtsc();

    /* measure how many tsc cycles pass while PIT wrapsaround */
    pit_wait_wraparound();

    new_ticks = rdtsc();

    diff = new_ticks - old_ticks;

    /* sanity checks */
    assert((uint32_t) diff == diff);
    assert(new_ticks > old_ticks);

    /* bravo, khz */
    cycles_per_ms = (uint32_t) diff / PIT_WRAPAROUND_MS;

    return cycles_per_ms;
}

uint64_t
tsc_getCurrentTime()
{

    return rdtsc();
}

int
tsc_setDeadline(uint64_t deadline)
{
    /* deadline is in tsc ticks */
#ifdef CONFIG_DEBUG_BUILD
    uint64_t current_time = rdtsc();
    if (deadline < current_time) {
        printf("deadline %llx, current_time %llx, diff %llx\n",
               deadline, current_time, current_time - deadline);
        return 1;
    }
#endif
    ia32_wrmsr(IA32_TSC_DEADLINE_MSR, (uint32_t) (deadline >> 32llu), (uint32_t) (deadline));

    return 0;
}


