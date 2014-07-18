/*
 * Copyright 2014, General Dynamics C4 Systems
 *
 * This software may be distributed and modified according to the terms of
 * the GNU General Public License version 2. Note that NO WARRANTY is provided.
 * See "LICENSE_GPLv2.txt" for details.
 *
 * @TAG(GD_GPL)
 */

/* A9 MPCORE private timer */

/* 32 bit down counter */
volatile struct priv_timer {
    uint32_t load;
    uint32_t count;
    uint32_t ctrl;
    uint32_t ints;
} *priv_timer = (volatile struct priv_timer*)ARM_MP_PRIV_TIMER_PPTR;

#define TMR_CTRL_ENABLE      BIT(0)
#define TMR_CTRL_AUTORELOAD  BIT(1)
#define TMR_CTRL_IRQEN       BIT(2)
#define TMR_CTRL_PRESCALE    8

#define TMR_INTS_EVENT       BIT(0)


#define CLK_MHZ 498ULL
#define TIMER_INTERVAL_MS    (CONFIG_TIMER_TICK_MS)
#define TIMER_COUNT_BITS 32

#define PRESCALE ((CLK_MHZ*1000 * TIMER_INTERVAL_MS) >> TIMER_COUNT_BITS)
#define TMR_LOAD ((CLK_MHZ*1000 * TIMER_INTERVAL_MS) / (PRESCALE + 1))


