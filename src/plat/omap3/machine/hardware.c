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
#include <types.h>
#include <machine/io.h>
#include <kernel/vspace.h>
#include <arch/machine.h>
#include <arch/kernel/vspace.h>
#include <plat/machine.h>
#include <arch/linker.h>
#include <plat/machine/devices.h>
#include <plat/machine/hardware.h>

/* Available physical memory regions on platform (RAM minus kernel image). */
/* NOTE: Regions are not allowed to be adjacent! */

const p_region_t BOOT_RODATA avail_p_regs[] = {
    /* 512 MiB */
    { .start = 0x80000000, .end = 0xA0000000 }
};

BOOT_CODE int get_num_avail_p_regs(void)
{
    return sizeof(avail_p_regs) / sizeof(p_region_t);
}

BOOT_CODE p_region_t get_avail_p_reg(word_t i)
{
    return avail_p_regs[i];
}

const p_region_t BOOT_RODATA dev_p_regs[] = {
    /* sorted by increasing memory address */
    /* region caps must be a power of 2. */


    /* Boot space */
    /* 0x00000000 - 0x40000000 */
//  { GPMC_PADDR                    , GPMC_PADDR                     + ( 1 << 30       ) },

    /* TODO: Board specific devices should ultimately be replaced with a more general solution. */
    { ETHERNET_BASE_PADDR           , ETHERNET_BASE_PADDR            + ( 1 << PAGE_BITS) },

    /* On-chip memory */
    /* 0x40000000 - 0x48000000 */
//  { BOOT_ROM0_PADDR               , BOOT_ROM0_PADDR                + (20 << PAGE_BITS) },
//  { BOOT_ROM1_PADDR               , BOOT_ROM1_PADDR                + ( 8 << PAGE_BITS) },
//  { SRAM_INTERNAL_PADDR           , SRAM_INTERNAL_PADDR            + (16 << PAGE_BITS) },

    /* L4 core (2 pages each unless specified) */
    /* 0x48000000 - 0x48300000 */
    { SYSTEM_CONTROL_MODULE_PADDR   , SYSTEM_CONTROL_MODULE_PADDR    + ( 2 << PAGE_BITS) },
    { CLOCK_MANAGER_PADDR           , CLOCK_MANAGER_PADDR            + ( 2 << PAGE_BITS) },
    { L4_CORE_CONFIG_PADDR          , L4_CORE_CONFIG_PADDR           + ( 2 << PAGE_BITS) },
    { DISPLAY_SUBSYSTEM_PADDR       , DISPLAY_SUBSYSTEM_PADDR        + ( 2 << PAGE_BITS) },
    { SDMA_PADDR                    , SDMA_PADDR                     + ( 2 << PAGE_BITS) },
    { I2C3_PADDR                    , I2C3_PADDR                     + ( 2 << PAGE_BITS) },
    { USBTLL_PADDR                  , USBTLL_PADDR                   + ( 2 << PAGE_BITS) },
    { HS_USB_HOST_PADDR             , HS_USB_HOST_PADDR              + ( 2 << PAGE_BITS) },
    { UART1_PADDR                   , UART1_PADDR                    + ( 2 << PAGE_BITS) },
    { UART2_PADDR                   , UART2_PADDR                    + ( 2 << PAGE_BITS) },
    { I2C1_PADDR                    , I2C1_PADDR                     + ( 2 << PAGE_BITS) },
    { I2C2_PADDR                    , I2C2_PADDR                     + ( 2 << PAGE_BITS) },
    { MCBSP1_PADDR                  , MCBSP1_PADDR                   + ( 2 << PAGE_BITS) },
    { GPTIMER10_PADDR               , GPTIMER10_PADDR                + ( 2 << PAGE_BITS) },
    { GPTIMER11_PADDR               , GPTIMER11_PADDR                + ( 2 << PAGE_BITS) },
    { MAILBOX_PADDR                 , MAILBOX_PADDR                  + ( 2 << PAGE_BITS) },
    { MCBSP5_PADDR                  , MCBSP5_PADDR                   + ( 2 << PAGE_BITS) },
    { MCSPI1_PADDR                  , MCSPI1_PADDR                   + ( 2 << PAGE_BITS) },
    { MCSPI2_PADDR                  , MCSPI2_PADDR                   + ( 2 << PAGE_BITS) },
    { MMC_SD_SDIO1_PADDR            , MMC_SD_SDIO1_PADDR             + ( 2 << PAGE_BITS) },
    { HS_USB_OTG_PADDR              , HS_USB_OTG_PADDR               + ( 2 << PAGE_BITS) },
    { MMC_SD_SDIO3_PADDR            , MMC_SD_SDIO3_PADDR             + ( 2 << PAGE_BITS) },
    { HDQ_TM_1WIRE_PADDR            , HDQ_TM_1WIRE_PADDR             + ( 2 << PAGE_BITS) },
    { MMC_SD_SDIO2_PADDR            , MMC_SD_SDIO2_PADDR             + ( 2 << PAGE_BITS) },
    { ICR_MPU_PORT_PADDR            , ICR_MPU_PORT_PADDR             + ( 2 << PAGE_BITS) },
    { MCSPI3_PADDR                  , MCSPI3_PADDR                   + ( 2 << PAGE_BITS) },
    { MCSPI4_PADDR                  , MCSPI4_PADDR                   + ( 2 << PAGE_BITS) },
    { CAMERA_ISP_PADDR              , CAMERA_ISP_PADDR               + ( 2 << PAGE_BITS) },
    { SR1_PADDR                     , SR1_PADDR                      + ( 2 << PAGE_BITS) },
    { SR2_PADDR                     , SR2_PADDR                      + ( 2 << PAGE_BITS) },
    { ICR_MODEM_PORT_PADDR          , ICR_MODEM_PORT_PADDR           + ( 2 << PAGE_BITS) },
//  { INTC_PADDR                    , INTC_PADDR                     + ( 1 << PAGE_BITS) },
    { L4_WAKEUP_INTERCONNECT_A_PADDR, L4_WAKEUP_INTERCONNECT_A_PADDR + ( 2 << PAGE_BITS) },
    { CONTROL_MODULE_ID_CODE_PADDR  , CONTROL_MODULE_ID_CODE_PADDR   + ( 2 << PAGE_BITS) },
    { L4_WAKEUP_INTERCONNECT_B_PADDR, L4_WAKEUP_INTERCONNECT_B_PADDR + ( 2 << PAGE_BITS) },

    /* L4 Wakeup (2 pages each unless specified) */
    /* 0x48300000 - 0x49000000 */
    { PWR_AND_RST_MANAGER_PADDR     , PWR_AND_RST_MANAGER_PADDR      + ( 2 << PAGE_BITS) },
    { GPIO1_PADDR                   , GPIO1_PADDR                    + ( 2 << PAGE_BITS) },
    { WDT2_PADDR                    , WDT2_PADDR                     + ( 2 << PAGE_BITS) },
    { GPTIMER1_PADDR                , GPTIMER1_PADDR                 + ( 2 << PAGE_BITS) },
    { TIMER32K_PADDR                , TIMER32K_PADDR                 + ( 2 << PAGE_BITS) },
    { L4_WAKEUP_CONFIG_PADDR        , L4_WAKEUP_CONFIG_PADDR         + ( 2 << PAGE_BITS) },

    /* L4 peripherals (2 pages each) */
    /* 0x49000000 - 0x50000000 */
    { L4_PER_CONFIG_PADDR           , L4_PER_CONFIG_PADDR            + ( 2 << PAGE_BITS) },
    { UART3_PADDR                   , UART3_PADDR                    + ( 2 << PAGE_BITS) },
    { MCBSP2_PADDR                  , MCBSP2_PADDR                   + ( 2 << PAGE_BITS) },
    { MCBSP3_PADDR                  , MCBSP3_PADDR                   + ( 2 << PAGE_BITS) },
    { MCBSP4_PADDR                  , MCBSP4_PADDR                   + ( 2 << PAGE_BITS) },
    { MCBSP2_SIDETONE_PADDR         , MCBSP2_SIDETONE_PADDR          + ( 2 << PAGE_BITS) },
    { MCBSP3_SIDETONE_PADDR         , MCBSP3_SIDETONE_PADDR          + ( 2 << PAGE_BITS) },
    { WDT3_PADDR                    , WDT3_PADDR                     + ( 2 << PAGE_BITS) },
    { GPTIMER2_PADDR                , GPTIMER2_PADDR                 + ( 2 << PAGE_BITS) },
    { GPTIMER3_PADDR                , GPTIMER3_PADDR                 + ( 2 << PAGE_BITS) },
    { GPTIMER4_PADDR                , GPTIMER4_PADDR                 + ( 2 << PAGE_BITS) },
    { GPTIMER5_PADDR                , GPTIMER5_PADDR                 + ( 2 << PAGE_BITS) },
    { GPTIMER6_PADDR                , GPTIMER6_PADDR                 + ( 2 << PAGE_BITS) },
    { GPTIMER7_PADDR                , GPTIMER7_PADDR                 + ( 2 << PAGE_BITS) },
    { GPTIMER8_PADDR                , GPTIMER8_PADDR                 + ( 2 << PAGE_BITS) },
//  { GPTIMER9_PADDR                , GPTIMER9_PADDR                 + ( 2 << PAGE_BITS) },
    { UART4_PADDR                   , UART4_PADDR                    + ( 2 << PAGE_BITS) },
    { GPIO2_PADDR                   , GPIO2_PADDR                    + ( 2 << PAGE_BITS) },
    { GPIO3_PADDR                   , GPIO3_PADDR                    + ( 2 << PAGE_BITS) },
    { GPIO4_PADDR                   , GPIO4_PADDR                    + ( 2 << PAGE_BITS) },
    { GPIO5_PADDR                   , GPIO5_PADDR                    + ( 2 << PAGE_BITS) },
    { GPIO6_PADDR                   , GPIO6_PADDR                    + ( 2 << PAGE_BITS) },

    /* SGX */
    /* 0x50000000 - 0x54000000 */
    { SGX_PADDR                     , SGX_PADDR                      + (16 << PAGE_BITS) },

    /* L4 emu (2 pages each unless specified) */
    /* 0x54000000 - 0x58000000 */
    { EMU_TPIU_PADDR                , EMU_TPIU_PADDR                 + ( 2 << PAGE_BITS) },
    { EMU_ETB_PADDR                 , EMU_ETB_PADDR                  + ( 2 << PAGE_BITS) },
    { EMU_DAPCTL_PADDR              , EMU_DAPCTL_PADDR               + ( 2 << PAGE_BITS) },
    { EMU_SDTI_L4_INTERCONNECT_PADDR, EMU_SDTI_L4_INTERCONNECT_PADDR + ( 1 << PAGE_BITS) },
    { EMU_SDTI_CONFIG_PADDR         , EMU_SDTI_CONFIG_PADDR          + ( 1 << PAGE_BITS) },
    { EMU_SDTI_WINDOW_PADDR         , EMU_SDTI_WINDOW_PADDR          + ( 1 << 20       ) },
    { EMU_PWR_AND_RST_MANAGER_PADDR , EMU_PWR_AND_RST_MANAGER_PADDR  + ( 4 << PAGE_BITS) },
    { EMU_GPIO1_PADDR               , EMU_GPIO1_PADDR                + ( 2 << PAGE_BITS) },
//  { EMU_WDT2_PADDR                , EMU_WDT2_PADDR                 + ( 2 << PAGE_BITS) },
    { EMU_GPTIMER1_PADDR            , EMU_GPTIMER1_PADDR             + ( 2 << PAGE_BITS) },
    { EMU_32KTIMER_PADDR            , EMU_32KTIMER_PADDR             + ( 2 << PAGE_BITS) },
    { EMU_L4_WAKEUP_CONFIG_PADDR    , EMU_L4_WAKEUP_CONFIG_PADDR     + ( 3 << PAGE_BITS) },

    /* IVA 2.2 Subsystem */
    /* 0x5C000000 - 0x60000000 */
    { IVA_22_PADDR                  , IVA_22_PADDR                   + (48 << 20       ) },

    /* Level 3 Interconnect */
    /* 0x68000000 - 0x70000000 */
    { L3_CONTROL_PADDR              , L3_CONTROL_PADDR               + (16 << 20       ) },
//  { L3_SMS_CONFIG                 , L3_SMS_CONFIG                  + (16 << 20       ) },
//  { L3_SDRC_CONFIG                , L3_SDRC_CONFIG                 + (16 << 20       ) },
//  { L3_GPMC_CONFIG                , L3_GPMC_CONFIG                 + (16 << 20       ) }

};

BOOT_CODE int get_num_dev_p_regs(void)
{
    return sizeof(dev_p_regs) / sizeof(p_region_t);
}

BOOT_CODE p_region_t get_dev_p_reg(word_t i)
{
    return dev_p_regs[i];
}


BOOT_CODE void
map_kernel_devices(void)
{
    /* map kernel device: GP Timer 9 */
    map_kernel_frame(
        GPTIMER9_PADDR,
        GPTIMER9_PPTR,
        VMKernelOnly,
        vm_attributes_new(
            true,  /* armExecuteNever */
            false, /* armParityEnabled */
            false  /* armPageCacheable */
        )
    );

    /* map kernel device: INTC */
    map_kernel_frame(
        INTC_PADDR,
        INTC_PPTR,
        VMKernelOnly,
        vm_attributes_new(
            true,  /* armExecuteNever */
            false, /* armParityEnabled */
            false  /* armPageCacheable */
        )
    );

#ifdef CONFIG_PRINTING
    /* map kernel device: UART */
    map_kernel_frame(
        UART3_PADDR,
        UART3_PPTR,
        VMKernelOnly,
        vm_attributes_new(
            true,  /* armExecuteNever */
            false, /* armParityEnabled */
            false  /* armPageCacheable */
        )
    );
#endif
}

#define INTCPS_SYSCONFIG_SOFTRESET BIT(1)
#define INTCPS_SYSSTATUS_RESETDONE BIT(0)
#define INTCPS_SIR_IRQ_SPURIOUSIRQFLAG 0xFF0000

/*
 * The struct below is used to discourage the compiler from generating literals
 * for every single address we might access.
 */
volatile struct INTC_map {
    uint32_t padding[4];
    uint32_t intcps_sysconfig;
    uint32_t intcps_sysstatus;
    uint32_t padding2[10];
    uint32_t intcps_sir_irq;
    uint32_t intcps_sir_fiq;
    uint32_t intcps_control;
    uint32_t intcps_protection;
    uint32_t intcps_idle;
    uint32_t padding3[3];
    uint32_t intcps_irq_priority;
    uint32_t intcps_fiq_priority;
    uint32_t intcps_threshold;
    uint32_t padding4[5];
    struct {
        uint32_t intcps_itr;
        uint32_t intcps_mir;
        uint32_t intcps_mir_clear;
        uint32_t intcps_mir_set;
        uint32_t intcps_isr_set;
        uint32_t intcps_isr_clear;
        uint32_t intcps_pending_irq;
        uint32_t intcps_pending_fiq;
    } intcps_n[3];
    uint32_t padding5[8];
    uint32_t intcps_ilr[96];
} *intc = (volatile void*)INTC_PPTR;

/**
   DONT_TRANSLATE
 */

interrupt_t
getActiveIRQ(void)
{
    uint32_t intcps_sir_irq = intc->intcps_sir_irq;
    interrupt_t irq = (interrupt_t)(intcps_sir_irq & 0x7f);

    /* Ignore spurious interrupts. */
    if ((intcps_sir_irq & INTCPS_SIR_IRQ_SPURIOUSIRQFLAG) == 0) {
        assert(irq <= maxIRQ);
        if (intc->intcps_n[irq / 32].intcps_pending_irq & (1 << (irq & 31))) {
            return irq;
        }
    }

    /* No interrupt. */
    return 0xff;
}

/* Check for pending IRQ */
bool_t isIRQPending(void)
{
    return getActiveIRQ() != 0xff;
}

/* Enable or disable irq according to the 'disable' flag. */
/**
   DONT_TRANSLATE
*/
void
maskInterrupt(bool_t disable, interrupt_t irq)
{
    if (disable) {
        intc->intcps_n[irq / 32].intcps_mir_set = 1 << (irq & 31);
    } else {
        intc->intcps_n[irq / 32].intcps_mir_clear = 1 << (irq & 31);
    }
}

/* Handle a platform-reserved IRQ. */
void handleReservedIRQ(irq_t irq)
{
    /* We shouldn't be receiving any reserved IRQs anyway. */
    maskInterrupt(true, irq);

    return;
}

void
ackInterrupt(irq_t irq)
{
    intc->intcps_control = 1;
    /* Ensure the ack has hit the interrupt controller before potentially
     * re-enabling interrupts. */
    dsb();
}


#define TISR_OVERFLOW BIT(1u)
#define TISR_MATCH    BIT(0u)
#define TIER_OVERFLOW BIT(1u)
#define TIER_MATCH    BIT(0u)

#define TIOCP_CFG_SOFTRESET BIT(1u)
#define TCLR_AUTORELOAD     BIT(1u)
#define TCLR_COMPAREENABLE  BIT(6u)
#define TCLR_STARTTIMER     BIT(0u)

/* this is configured and detected by uboot */
#define CLK_MHZ 13
/* constants for division by reciprocal multiplication, for calculation
 * see tools/reciprocal.py */
#define CLK_MAGIC 1321528399llu
#define CLK_SHIFT 34u
compile_assert(magic_will_work, CLK_MHZ == 13u)

static volatile struct TIMER_map {
    uint32_t tidr;   /* GPTIMER_TIDR 0x00 */
    uint32_t padding1[3];
    uint32_t cfg;    /* GPTIMER_CFG 0x10 */
    uint32_t tistat; /* GPTIMER_TISTAT 0x14 */
    uint32_t tisr;   /* GPTIMER_TISR 0x18 */
    uint32_t tier;   /* GPTIMER_TIER 0x1C */
    uint32_t twer;   /* GPTIMER_TWER 0x20 */
    uint32_t tclr;   /* GPTIMER_TCLR 0x24 */
    uint32_t tcrr;   /* GPTIMER_TCRR 0x28 */
    uint32_t tldr;   /* GPTIMER_TLDR 0x2C */
    uint32_t ttgr;   /* GPTIMER_TTGR 0x30 */
    uint32_t twps;   /* GPTIMER_TWPS 0x34 */
    uint32_t tmar;   /* GPTIMER_TMAR 0x38 */
    uint32_t tcar1;  /* GPTIMER_TCAR1 0x3C */
    uint32_t tsicr;  /* GPTIMER_TSICR 0x40 */
    uint32_t tcar2;  /* GPTIMER_TCAR2 0x44 */
    uint32_t tpir;   /* GPTIMER_TPIR 0x48 */
    uint32_t tnir;   /* GPTIMER_TNIR 0x4C */
    uint32_t tcvr;   /* GPTIMER_TCVR 0x50 */
    uint32_t tocr;   /* GPTIMER_TOCR 0x54 */
    uint32_t towr;   /* GPTIMER_TOWR 0x58 */
} *timer = (volatile void*)GPTIMER9_PPTR;

static uint32_t high_bits = 0u;

/**
   DONT_TRANSLATE
 */
BOOT_CODE void
initTimer(void)
{
    /* Configure gptimer9 as kernel timer */

    /* disable */
    timer->tclr = 0;

    /* perform a soft reset */
    timer->cfg = TIOCP_CFG_SOFTRESET;

    /* wait for reset */
    while (!timer->tistat);

    maskInterrupt(/*disable*/ true, GPT9_IRQ);

    /* Set the reload value */
    timer->tldr = 0u;

    /* Enables interrupt on overflow and match */
    timer->tier |= (TIER_OVERFLOW | TIER_MATCH);

    /* Clear the read register */
    timer->tcrr = 0u;

    /* start the timer */
    timer->tclr = TCLR_AUTORELOAD | TCLR_STARTTIMER | TCLR_COMPAREENABLE;
}

void
setDeadline(ticks_t deadline)
{
    assert(deadline > ksCurrentTime);
    timer->tmar = (uint32_t) deadline;
}

ticks_t
getCurrentTime(void)
{
    bool_t overflow = !!(timer->tisr & TISR_OVERFLOW);
    return (((uint64_t) high_bits + overflow) << 32llu) + timer->tcrr;
}

void
ackDeadlineIRQ(void)
{
    /* check if this is an overflow irq */
    if (timer->tisr & TISR_OVERFLOW) {
        high_bits++;
    }

    /* ack everything */
    timer->tisr = TISR_OVERFLOW | TISR_MATCH;
    assert((timer->tisr & TISR_OVERFLOW) == 0);
    ackInterrupt(GPT9_IRQ);
}

time_t
getMaxTimerUs(void)
{
    return UINT64_MAX / CLK_MAGIC;
}

ticks_t
getTimerPrecision(void)
{
    return 2 * CLK_MHZ;
}

time_t
getKernelWcetUs(void)
{
    return 10u;
}

ticks_t
usToTicks(time_t us)
{
    return (us * CLK_MHZ);
}

time_t
ticksToUs(ticks_t ticks)
{
    /* emulate ticks / CLK_MHZ with multiplication by reciprocal */
    return (ticks * CLK_MAGIC) >> CLK_SHIFT;
}

/**
   DONT_TRANSLATE
 */
BOOT_CODE void
initIRQController(void)
{
    intc->intcps_sysconfig = INTCPS_SYSCONFIG_SOFTRESET;
    while (!(intc->intcps_sysstatus & INTCPS_SYSSTATUS_RESETDONE)) ;
}

/**
   DONT_TRANSLATE
 */
void
handleSpuriousIRQ(void)
{
    /* Reset and re-enable IRQs. */
    intc->intcps_control = 1;
    dsb();
}


void plat_cleanL2Range(paddr_t start, paddr_t end) {}
void plat_invalidateL2Range(paddr_t start, paddr_t end) {}
void plat_cleanInvalidateL2Range(paddr_t start, paddr_t end) {}

