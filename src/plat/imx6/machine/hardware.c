/*
 * Copyright 2014, General Dynamics C4 Systems
 *
 * This software may be distributed and modified according to the terms of
 * the GNU General Public License version 2. Note that NO WARRANTY is provided.
 * See "LICENSE_GPLv2.txt" for details.
 *
 * @TAG(GD_GPL)
 */

#include <types.h>
#include <machine/io.h>
#include <kernel/vspace.h>
#include <arch/machine.h>
#include <arch/kernel/vspace.h>
#include <plat/machine.h>
#include <arch/linker.h>
#include <plat/machine/devices.h>
#include <plat/machine/hardware.h>

/* Available physical memory regions on platform (RAM) */
/* NOTE: Regions are not allowed to be adjacent! */
const p_region_t BOOT_RODATA avail_p_regs[] = {
    /* 1 GiB */
#ifdef CONFIG_BENCHMARK
    /* 1MB stolen for logging */
    { /* .start = */ 0x10000000, /* .end = */ 0x2fd00000 }
#else
    { /* .start = */ 0x10000000, /* .end = */ 0x2fe00000 }
#endif /* CONFIG_BENCHMARK */
};

BOOT_CODE int
get_num_avail_p_regs(void)
{
    return sizeof(avail_p_regs) / sizeof(p_region_t);
}

BOOT_CODE p_region_t
get_avail_p_reg(unsigned int i)
{
    return avail_p_regs[i];
}

const p_region_t BOOT_RODATA dev_p_regs[] = {
//  { /* .start = */ EIM_CS0_PADDR          , /* .end = */ EIM_CS0_PADDR           + (128 << 20)},
    { /* .start = */ IPU2_PADDR             , /* .end = */ IPU2_PADDR              + (  4 << 20)},
    { /* .start = */ IPU1_PADDR             , /* .end = */ IPU1_PADDR              + (  4 << 20)},
    { /* .start = */ MIPI_HSI_PADDR         , /* .end = */ MIPI_HSI_PADDR          + (  4 << 12)},
    { /* .start = */ OPENVG_PADDR           , /* .end = */ OPENVG_PADDR            + (  4 << 12)},
    { /* .start = */ SATA_PADDR             , /* .end = */ SATA_PADDR              + (  4 << 12)},
    { /* .start = */ UART5_PADDR            , /* .end = */ UART5_PADDR             + (  4 << 12)},
    { /* .start = */ UART4_PADDR            , /* .end = */ UART4_PADDR             + (  4 << 12)},
    { /* .start = */ UART3_PADDR            , /* .end = */ UART3_PADDR             + (  4 << 12)},
    { /* .start = */ UART2_PADDR            , /* .end = */ UART2_PADDR             + (  4 << 12)},
    { /* .start = */ VDOA_PADDR             , /* .end = */ VDOA_PADDR              + (  4 << 12)},
    { /* .start = */ MIPI_DSI_PADDR         , /* .end = */ MIPI_DSI_PADDR          + (  4 << 12)},
    { /* .start = */ MIPI_CSI_PADDR         , /* .end = */ MIPI_CSI_PADDR          + (  4 << 12)},
    { /* .start = */ AUDMUX_PADDR           , /* .end = */ AUDMUX_PADDR            + (  4 << 12)},
    { /* .start = */ TZASC2_PADDR           , /* .end = */ TZASC2_PADDR            + (  4 << 12)},
    { /* .start = */ TZASC1_PADDR           , /* .end = */ TZASC1_PADDR            + (  4 << 12)},
    { /* .start = */ CSU_PADDR              , /* .end = */ CSU_PADDR               + (  4 << 12)},
    { /* .start = */ OCOTP_CTRL_PADDR       , /* .end = */ OCOTP_CTRL_PADDR        + (  4 << 12)},
    { /* .start = */ EIM_PADDR              , /* .end = */ EIM_PADDR               + (  4 << 12)},
    { /* .start = */ MMDC1_PADDR            , /* .end = */ MMDC1_PADDR             + (  4 << 12)},
    { /* .start = */ MMDC0_PADDR            , /* .end = */ MMDC0_PADDR             + (  4 << 12)},
    { /* .start = */ ROMCP_PADDR            , /* .end = */ ROMCP_PADDR             + (  4 << 12)},
    { /* .start = */ I2C3_PADDR             , /* .end = */ I2C3_PADDR              + (  4 << 12)},
    { /* .start = */ I2C2_PADDR             , /* .end = */ I2C2_PADDR              + (  4 << 12)},
    { /* .start = */ I2C1_PADDR             , /* .end = */ I2C1_PADDR              + (  4 << 12)},
    { /* .start = */ USDHC4_PADDR           , /* .end = */ USDHC4_PADDR            + (  4 << 12)},
    { /* .start = */ USDHC3_PADDR           , /* .end = */ USDHC3_PADDR            + (  4 << 12)},
    { /* .start = */ USDHC2_PADDR           , /* .end = */ USDHC2_PADDR            + (  4 << 12)},
    { /* .start = */ USDHC1_PADDR           , /* .end = */ USDHC1_PADDR            + (  4 << 12)},
    { /* .start = */ MLB150_PADDR           , /* .end = */ MLB150_PADDR            + (  4 << 12)},
    { /* .start = */ ENET_PADDR             , /* .end = */ ENET_PADDR              + (  4 << 12)},
    { /* .start = */ USBOH3_PADDR           , /* .end = */ USBOH3_PADDR            + (  4 << 12)},
    { /* .start = */ AIPS2_CONFIG_PADDR     , /* .end = */ AIPS2_CONFIG_PADDR      + (  4 << 12)},
//  { /* .start = */ ARM_MPCORE_PADDR       , /* .end = */ ARM_MPCORE_PADDR        + ( 27 << 12)},
    { /* .start = */ PLATFORM_CONTROL_PADDR , /* .end = */ PLATFORM_CONTROL_PADDR  + (  1 << 12)},
    { /* .start = */ PTM3_PADDR             , /* .end = */ PTM3_PADDR              + (  1 << 12)},
    { /* .start = */ PTM2_PADDR             , /* .end = */ PTM2_PADDR              + (  1 << 12)},
    { /* .start = */ PTM1_PADDR             , /* .end = */ PTM1_PADDR              + (  1 << 12)},
    { /* .start = */ PTM0_PADDR             , /* .end = */ PTM0_PADDR              + (  1 << 12)},
    { /* .start = */ CTI3_PADDR             , /* .end = */ CTI3_PADDR              + (  1 << 12)},
    { /* .start = */ CTI2_PADDR             , /* .end = */ CTI2_PADDR              + (  1 << 12)},
    { /* .start = */ CTI1_PADDR             , /* .end = */ CTI1_PADDR              + (  1 << 12)},
    { /* .start = */ CTI0_PADDR             , /* .end = */ CTI0_PADDR              + (  1 << 12)},
    { /* .start = */ CPU3_PMU_PADDR         , /* .end = */ CPU3_PMU_PADDR          + (  1 << 12)},
    { /* .start = */ CPU3_DEBUG_PADDR       , /* .end = */ CPU3_DEBUG_PADDR        + (  1 << 12)},
    { /* .start = */ CPU2_PMU_PADDR         , /* .end = */ CPU2_PMU_PADDR          + (  1 << 12)},
    { /* .start = */ CPU2_DEBUG_PADDR       , /* .end = */ CPU2_DEBUG_PADDR        + (  1 << 12)},
    { /* .start = */ CPU1_PMU_PADDR         , /* .end = */ CPU1_PMU_PADDR          + (  1 << 12)},
    { /* .start = */ CPU1_PADDR             , /* .end = */ CPU1_PADDR              + (  1 << 12)},
    { /* .start = */ CPU0_PMU_PADDR         , /* .end = */ CPU0_PMU_PADDR          + (  1 << 12)},
    { /* .start = */ CPU0_DEBUG_PADDR       , /* .end = */ CPU0_DEBUG_PADDR        + (  1 << 12)},
    { /* .start = */ CA9_INTEG_PADDR        , /* .end = */ CA9_INTEG_PADDR         + (  1 << 12)},
    { /* .start = */ FUNNEL_PADDR           , /* .end = */ FUNNEL_PADDR            + (  1 << 12)},
    { /* .start = */ TPIU_PADDR             , /* .end = */ TPIU_PADDR              + (  1 << 12)},
    { /* .start = */ CTI_PADDR              , /* .end = */ CTI_PADDR               + (  1 << 12)},
    { /* .start = */ ETB_PADDR              , /* .end = */ ETB_PADDR               + (  1 << 12)},
    { /* .start = */ DAP_ROM_TABLE_PADDR    , /* .end = */ DAP_ROM_TABLE_PADDR     + (  1 << 12)},
    { /* .start = */ CAAM_PADDR             , /* .end = */ CAAM_PADDR              + ( 16 << 12)},
    { /* .start = */ SDMA_PADDR             , /* .end = */ SDMA_PADDR              + (  4 << 12)},
    { /* .start = */ DCIC2_PADDR            , /* .end = */ DCIC2_PADDR             + (  4 << 12)},
    { /* .start = */ DCIC1_PADDR            , /* .end = */ DCIC1_PADDR             + (  4 << 12)},
    { /* .start = */ IOMUXC_PADDR           , /* .end = */ IOMUXC_PADDR            + (  4 << 12)},
    { /* .start = */ GPC_PADDR              , /* .end = */ GPC_PADDR               + (  1 << 12)},
    { /* .start = */ SRC_PADDR              , /* .end = */ SRC_PADDR               + (  4 << 12)},
    { /* .start = */ EPIT2_PADDR            , /* .end = */ EPIT2_PADDR             + (  4 << 12)},
    { /* .start = */ EPIT1_PADDR            , /* .end = */ EPIT1_PADDR             + (  4 << 12)},
    { /* .start = */ SNVS_HP_PADDR          , /* .end = */ SNVS_HP_PADDR           + (  4 << 12)},
    { /* .start = */ USBPHY2_PADDR          , /* .end = */ USBPHY2_PADDR           + (  1 << 12)},
    { /* .start = */ USBPHY1_PADDR          , /* .end = */ USBPHY1_PADDR           + (  1 << 12)},
    { /* .start = */ ANALOG_PADDR           , /* .end = */ ANALOG_PADDR            + (  1 << 12)},
    { /* .start = */ CCM_PADDR              , /* .end = */ CCM_PADDR               + (  4 << 12)},
    { /* .start = */ WDOG2_PADDR            , /* .end = */ WDOG2_PADDR             + (  4 << 12)},
    { /* .start = */ WDOG1_PADDR            , /* .end = */ WDOG1_PADDR             + (  4 << 12)},
    { /* .start = */ KPP_PADDR              , /* .end = */ KPP_PADDR               + (  4 << 12)},
    { /* .start = */ GPIO7_PADDR            , /* .end = */ GPIO7_PADDR             + (  4 << 12)},
    { /* .start = */ GPIO6_PADDR            , /* .end = */ GPIO6_PADDR             + (  4 << 12)},
    { /* .start = */ GPIO5_PADDR            , /* .end = */ GPIO5_PADDR             + (  4 << 12)},
    { /* .start = */ GPIO4_PADDR            , /* .end = */ GPIO4_PADDR             + (  4 << 12)},
    { /* .start = */ GPIO3_PADDR            , /* .end = */ GPIO3_PADDR             + (  4 << 12)},
    { /* .start = */ GPIO2_PADDR            , /* .end = */ GPIO2_PADDR             + (  4 << 12)},
    { /* .start = */ GPIO1_PADDR            , /* .end = */ GPIO1_PADDR             + (  4 << 12)},
    { /* .start = */ GPT_PADDR              , /* .end = */ GPT_PADDR               + (  4 << 12)},
    { /* .start = */ CAN2_PADDR             , /* .end = */ CAN2_PADDR              + (  4 << 12)},
    { /* .start = */ CAN1_PADDR             , /* .end = */ CAN1_PADDR              + (  4 << 12)},
    { /* .start = */ PWM4_PADDR             , /* .end = */ PWM4_PADDR              + (  4 << 12)},
    { /* .start = */ PWM3_PADDR             , /* .end = */ PWM3_PADDR              + (  4 << 12)},
    { /* .start = */ PWM2_PADDR             , /* .end = */ PWM2_PADDR              + (  4 << 12)},
    { /* .start = */ PWM1_PADDR             , /* .end = */ PWM1_PADDR              + (  4 << 12)},
    { /* .start = */ AIPS1_CONFIG_PADDR     , /* .end = */ AIPS1_CONFIG_PADDR      + (  4 << 12)},
    { /* .start = */ VPU_PADDR              , /* .end = */ VPU_PADDR               + ( 60 << 12)},
    { /* .start = */ AIPS1_SPBA_PADDR       , /* .end = */ AIPS1_SPBA_PADDR        + (  4 << 12)},
    { /* .start = */ ASRC_PADDR             , /* .end = */ ASRC_PADDR              + (  4 << 12)},
    { /* .start = */ SSI3_PADDR             , /* .end = */ SSI3_PADDR              + (  4 << 12)},
    { /* .start = */ SSI2_PADDR             , /* .end = */ SSI2_PADDR              + (  4 << 12)},
    { /* .start = */ SSI1_PADDR             , /* .end = */ SSI1_PADDR              + (  4 << 12)},
    { /* .start = */ ESAI_PADDR             , /* .end = */ ESAI_PADDR              + (  4 << 12)},
    { /* .start = */ UART1_PADDR            , /* .end = */ UART1_PADDR             + (  4 << 12)},
    { /* .start = */ ECSPI5_PADDR           , /* .end = */ ECSPI5_PADDR            + (  4 << 12)},
    { /* .start = */ ECSPI4_PADDR           , /* .end = */ ECSPI4_PADDR            + (  4 << 12)},
    { /* .start = */ ECSPI3_PADDR           , /* .end = */ ECSPI3_PADDR            + (  4 << 12)},
    { /* .start = */ ECSPI2_PADDR           , /* .end = */ ECSPI2_PADDR            + (  4 << 12)},
    { /* .start = */ ECSPI1_PADDR           , /* .end = */ ECSPI1_PADDR            + (  4 << 12)},
    { /* .start = */ SPDIF_PADDR            , /* .end = */ SPDIF_PADDR             + (  4 << 12)},
    { /* .start = */ PCIE_REGISTERS_PADDR   , /* .end = */ PCIE_REGISTERS_PADDR    + (  4 << 12)},
    { /* .start = */ PCIE_PADDR             , /* .end = */ PCIE_PADDR              + ( 15 << 20)},
    { /* .start = */ GPV1_PL301_CONFIG_PADDR, /* .end = */ GPV1_PL301_CONFIG_PADDR + (  1 << 20)},
    { /* .start = */ GPV0_PL301_CONFIG_PADDR, /* .end = */ GPV0_PL301_CONFIG_PADDR + (  1 << 20)},
//  { /* .start = */ L2CC_PL310_PADDR       , /* .end = */ L2CC_PL310_PADDR        + (  1 << 12)},
//  { /* .start = */ ARM_MP_PADDR           , /* .end = */ ARM_MP_PADDR            + (  2 << 12)},
    { /* .start = */ OCRAM_ALIASED_PADDR    , /* .end = */ OCRAM_ALIASED_PADDR     + (192 << 12)},
    { /* .start = */ OCRAM_PADDR            , /* .end = */ OCRAM_PADDR             + ( 64 << 12)},
    { /* .start = */ GPV4_PL301_CONFIG_PADDR, /* .end = */ GPV4_PL301_CONFIG_PADDR + (  1 << 20)},
    { /* .start = */ GPV3_PL301_CONFIG_PADDR, /* .end = */ GPV3_PL301_CONFIG_PADDR + (  1 << 20)},
    { /* .start = */ GPV2_PL301_CONFIG_PADDR, /* .end = */ GPV2_PL301_CONFIG_PADDR + (  1 << 20)},
    { /* .start = */ DTCP_PADDR             , /* .end = */ DTCP_PADDR              + (  4 << 12)},
    { /* .start = */ GPU2D_PADDR            , /* .end = */ GPU2D_PADDR             + (  4 << 12)},
    { /* .start = */ GPU3D_PADDR            , /* .end = */ GPU3D_PADDR             + (  4 << 12)},
    { /* .start = */ HDMI_PADDR             , /* .end = */ HDMI_PADDR              + (  9 << 12)},
    { /* .start = */ BCH_PADDR              , /* .end = */ BCH_PADDR               + (  4 << 12)},
    { /* .start = */ GPMI_PADDR             , /* .end = */ GPMI_PADDR              + (  2 << 12)},
    { /* .start = */ APBH_BRIDGE_DMA_PADDR  , /* .end = */ APBH_BRIDGE_DMA_PADDR   + (  2 << 12)},
    { /* .start = */ CAAM_SECURE_RAM_PADDR  , /* .end = */ CAAM_SECURE_RAM_PADDR   + (  4 << 12)},
//  { /* .start = */ BOOT_ROM_PADDR         , /* .end = */ BOOT_ROM_PADDR          + ( 24 << 12)}
};

BOOT_CODE int
get_num_dev_p_regs(void)
{
    return sizeof(dev_p_regs) / sizeof(p_region_t);
}

BOOT_CODE p_region_t
get_dev_p_reg(unsigned int i)
{
    return dev_p_regs[i];
}


/* Determine if the given IRQ should be reserved by the kernel. */
bool_t CONST
isReservedIRQ(irq_t irq)
{
    return irq == KERNEL_TIMER_IRQ || irq == INTERRUPT_PRIV_TIMER;
}

/* Handle a platform-reserved IRQ. */
void
handleReservedIRQ(irq_t irq)
{
    printf("Received reserved IRQ: %d\n", (int)irq);
}


static volatile struct globalTimerMap {
    uint32_t countLower;
    uint32_t countUpper;
    uint32_t control;
    uint32_t isr;
    uint32_t comparatorLower;
    uint32_t comparatorUpper;
    uint32_t autoInc;
} *globalTimer;

enum control {
    ENABLE = 0,
    COMP_ENABLE = 1,
    IRQ_ENABLE = 2,
    AUTO_INC = 3,
    RESERVED = 4,
    PRESCALER = 8,
    RESERVED_2 = 16
};

int
setDeadline(uint64_t deadline)
{

#ifdef CONFIG_DEBUG_BUILD
    uint64_t currentTime;
    if (deadline < ksCurrentTime) {
        printf("Deadline %llx, ksCurrentTime %llx\n", deadline, ksCurrentTime);
        assert(deadline > ksCurrentTime);
    }
#endif /* CONFIG_DEBUG_BUILD */

    /* write to the lower bits */
    globalTimer->comparatorLower = (uint32_t) deadline;
    /* write to the higher bits */
    globalTimer->comparatorUpper = (uint32_t) (deadline >> 32llu);
    /* set the enable bit */
    globalTimer->control |= (1 << COMP_ENABLE);

    /* turn on the interrupt */
    globalTimer->control |= (1 << IRQ_ENABLE);

#ifdef CONFIG_DEBUG_BUILD
    /* to avoid a race, we check if our deadline has already passed */
    currentTime = getCurrentTime();

    if (currentTime > deadline && globalTimer->isr == 0) {
        /* we missed the deadline and no interrupt fired, turn the timer off */
        globalTimer->control &= ~(1 << COMP_ENABLE);
        globalTimer->control &= ~(1 << IRQ_ENABLE);
        return 1;
    }
#endif /* CONFIG_DEBUG_BUILD */
    /* if the interrupt did fire we will catch it on kernel exit, so return success */
    return 0;
}

void
ackDeadlineIRQ(void)
{
    assert(globalTimer->isr == 1);
    /* clear comparator enable bit and the interrupt enable bit */
    globalTimer->control &= ~(1 << COMP_ENABLE);
    globalTimer->control &= ~(1 << IRQ_ENABLE);
    /* ack the isr */
    globalTimer->isr = 1;
}

uint64_t
getCurrentTime(void)
{
    uint32_t upper, upper2, lower;

    /* these values can't start the same */
    upper = 1;
    upper2 = 2;

    while (upper != upper2) {
        upper = globalTimer->countUpper;
        lower = globalTimer->countLower;
        upper2 = globalTimer->countUpper;
    }

    return (((uint64_t) upper) << 32) + (uint64_t) lower;
}

BOOT_CODE void
initTimer(void)
{
    ksTicksPerUs = 498u;

    globalTimer = (volatile struct globalTimerMap *) ARM_MP_GLOBAL_TIMER_PPTR;
    /* disable the timer */
    globalTimer->control = 0;
    /* zero the timer */
    globalTimer->countLower = 0;
    globalTimer->countUpper = 0;
    /* turn it on again, wih interrupts on, comparator register off,
     * in one-shot mode, with standard prescaler */
    globalTimer->control = (1 << ENABLE) | (1 << IRQ_ENABLE);

    /* BEWARE  this timer will overflow once 1GHz / 2 overflows.
     * Good enough for a prototype */
}


BOOT_CODE void
map_kernel_devices(void)
{
    /* map kernel device: GIC controller and private timers */
    map_kernel_frame(
        ARM_MP_PADDR,
        ARM_MP_PPTR1,
        VMKernelOnly,
        vm_attributes_new(
            false, /* armExecuteNever */
            false, /* armParityEnabled */
            false  /* armPageCacheable */
        )
    );

    /* map kernel device: GIC distributor */
    map_kernel_frame(
        ARM_MP_PADDR + BIT(PAGE_BITS),
        ARM_MP_PPTR2,
        VMKernelOnly,
        vm_attributes_new(
            false, /* armExecuteNever */
            false, /* armParityEnabled */
            false  /* armPageCacheable */
        )
    );

    /* map kernel device: L2CC */
    map_kernel_frame(
        L2CC_PL310_PADDR,
        L2CC_PL310_PPTR,
        VMKernelOnly,
        vm_attributes_new(
            false, /* armExecuteNever */
            false, /* armParityEnabled */
            false  /* armPageCacheable */
        )
    );


#if defined(DEBUG)
    /* map kernel device: UART */
    map_kernel_frame(
        UART_PADDR,
        UART_PPTR,
        VMKernelOnly,
        vm_attributes_new(
            false, /* armExecuteNever */
            false, /* armParityEnabled */
            false  /* armPageCacheable */
        )
    );
#endif /* DEBUG */
}

