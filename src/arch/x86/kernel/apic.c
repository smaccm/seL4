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
#include <arch/machine.h>
#include <arch/kernel/apic.h>
#include <arch/linker.h>
#include <plat/machine/devices.h>
#include <plat/machine/pit.h>

#define CPUID_TSC_DEADLINE_BIT 24u

typedef enum _apic_reg_t {
    APIC_ID             = 0x020,
    APIC_VERSION        = 0x030,
    APIC_TASK_PRIO      = 0x080,
    APIC_ARBITR_PRIO    = 0x090,
    APIC_PROC_PRIO      = 0x0A0,
    APIC_EOI            = 0x0B0,
    APIC_LOCAL_DEST     = 0x0D0,
    APIC_DEST_FORMAT    = 0x0E0,
    APIC_SVR            = 0x0F0,
    APIC_ISR_BASE       = 0x100,
    APIC_TMR_BASE       = 0x180,
    APIC_IRR_BASE       = 0x200,
    APIC_ERR_STATUS     = 0x280,
    APIC_ICR1           = 0x300,
    APIC_ICR2           = 0x310,
    APIC_LVT_TIMER      = 0x320,
    APIC_LVT_THERMAL    = 0x330,
    APIC_LVT_PERF_CNTR  = 0x340,
    APIC_LVT_LINT0      = 0x350,
    APIC_LVT_LINT1      = 0x360,
    APIC_LVT_ERROR      = 0x370,
    APIC_TIMER_COUNT    = 0x380,
    APIC_TIMER_CURRENT  = 0x390,
    APIC_TIMER_DIVIDE   = 0x3E0
} apic_reg_t;

static inline uint32_t
apic_read_reg(apic_reg_t reg)
{
    return *(volatile uint32_t*)(PPTR_APIC + reg);
}

static inline void
apic_write_reg(apic_reg_t reg, uint32_t val)
{
    *(volatile uint32_t*)(PPTR_APIC + reg) = val;
}

BOOT_CODE paddr_t
apic_get_base_paddr(void)
{
    apic_base_msr_t apic_base_msr;

    apic_base_msr.words[0] = x86_rdmsr_low(IA32_APIC_BASE_MSR);
    if (!apic_base_msr_get_enabled(apic_base_msr)) {
        printf("APIC: Enabled bit not set\n");
    }

    return apic_base_msr_get_base_addr(apic_base_msr);
}

BOOT_CODE bool_t
apic_init(bool_t mask_legacy_irqs)
{
    apic_version_t apic_version;
    uint32_t num_lvt_entries;
    uint32_t cpuid_value;

    apic_version.words[0] = apic_read_reg(APIC_VERSION);

    /* check for correct version: 0x1X */
    if (apic_version_get_version(apic_version) >> 4 != 1) {
        printf("APIC: apic_version must be 0x1X\n");
        return false;
    }

    /* check for correct number of LVT entries */
    num_lvt_entries = apic_version_get_max_lvt_entry(apic_version) + 1;
    if (num_lvt_entries < 3) {
        printf("APIC: number of LVT entries: %d\n", num_lvt_entries);
        printf("APIC: number of LVT entries must be >= 3\n");
        return false;
    }

    /* enable APIC using SVR register */
    apic_write_reg(
        APIC_SVR,
        apic_svr_new(
            0,           /* focus_processor_chk */
            1,           /* enabled             */
            int_spurious /* spurious_vector     */
        ).words[0]
    );

    /* mask/unmask LINT0 (used for legacy IRQ delivery) */
    apic_write_reg(
        APIC_LVT_LINT0,
        apic_lvt_new(
            0,                /* timer_mode      */
            mask_legacy_irqs, /* masked          */
            0,                /* trigger_mode    */
            0,                /* remote_irr      */
            0,                /* pin_polarity    */
            0,                /* delivery_status */
            7,                /* delivery_mode   */
            0                 /* vector          */
        ).words[0]
    );

    /* mask LINT1 (used for NMI delivery) */
    apic_write_reg(
        APIC_LVT_LINT1,
        apic_lvt_new(
            0,  /* timer_mode      */
            1,  /* masked          */
            0,  /* trigger_mode    */
            0,  /* remote_irr      */
            0,  /* pin_polarity    */
            0,  /* delivery_status */
            0,  /* delivery_mode   */
            0   /* vector          */
        ).words[0]
    );

    /* Check APIC for tsc-deadline mode support */
    cpuid_value = x86_cpuid_ecx(0x1, 0x0);

    if (!(cpuid_value & BIT(CPUID_TSC_DEADLINE_BIT))) {
        printf("APIC: unsupported platform, TSC-deadline mode is not supported\n");
        return false;
    }

    /* initialise timer */
    apic_write_reg(
        APIC_LVT_TIMER,
        apic_lvt_new(
            2,        /* timer_mode - tsc_deadline */
            0,        /* masked          */
            0,        /* trigger_mode    */
            0,        /* remote_irr      */
            0,        /* pin_polarity    */
            0,        /* delivery_status */
            0,        /* delivery_mode   */
            int_timer /* vector          */
        ).words[0]
    );

    /*
    printf("APIC: ID=0x%x\n", apic_read_reg(APIC_ID) >> 24);
    printf("APIC: SVR=0x%x\n", apic_read_reg(APIC_SVR));
    printf("APIC: LVT_TIMER=0x%x\n", apic_read_reg(APIC_LVT_TIMER));
    printf("APIC: LVT_LINT0=0x%x\n", apic_read_reg(APIC_LVT_LINT0));
    printf("APIC: LVT_LINT1=0x%x\n", apic_read_reg(APIC_LVT_LINT1));
    printf("APIC: LVT_ERROR=0x%x\n", apic_read_reg(APIC_LVT_ERROR));
    printf("APIC: LVT_PERF_CNTR=0x%x\n", apic_read_reg(APIC_LVT_PERF_CNTR));
    printf("APIC: LVT_THERMAL=0x%x\n", apic_read_reg(APIC_LVT_THERMAL));
    */
    return true;
}

bool_t apic_is_interrupt_pending(void)
{
    word_t i;

    /* read 256-bit register: each 32-bit word is 16 byte aligned */
    assert(int_irq_min % 32 == 0);
    for (i = int_irq_min; i <= int_irq_max; i += 32) {
        if (apic_read_reg(APIC_IRR_BASE + i / 2) != 0) {
            return true;
        }
    }
    return false;
}

void apic_ack_active_interrupt(void)
{
    apic_write_reg(APIC_EOI, 0);
}

BOOT_CODE void
apic_send_init_ipi(cpu_id_t cpu_id)
{
    apic_write_reg(
        APIC_ICR2,
        apic_icr2_new(
            cpu_id /* dest */
        ).words[0]
    );
    apic_write_reg(
        APIC_ICR1,
        apic_icr1_new(
            0,  /* dest_shorthand  */
            1,  /* trigger_mode    */
            1,  /* level           */
            0,  /* delivery_status */
            0,  /* dest_mode       */
            5,  /* delivery_mode   */
            0   /* vector          */
        ).words[0]
    );

    apic_write_reg(
        APIC_ICR2,
        apic_icr2_new(
            cpu_id /* dest */
        ).words[0]
    );
    apic_write_reg(
        APIC_ICR1,
        apic_icr1_new(
            0,  /* dest_shorthand  */
            1,  /* trigger_mode    */
            0,  /* level           */
            0,  /* delivery_status */
            0,  /* dest_mode       */
            5,  /* delivery_mode   */
            0   /* vector          */
        ).words[0]
    );
}

BOOT_CODE void
apic_send_startup_ipi(cpu_id_t cpu_id, paddr_t startup_addr)
{
    /* check if 4K aligned */
    assert(IS_ALIGNED(startup_addr, PAGE_BITS));
    /* check if startup_addr < 640K */
    assert(startup_addr < 0xa0000);
    startup_addr >>= PAGE_BITS;

    apic_write_reg(
        APIC_ICR2,
        apic_icr2_new(
            cpu_id /* dest */
        ).words[0]
    );
    apic_write_reg(
        APIC_ICR1,
        apic_icr1_new(
            0,           /* dest_shorthand  */
            0,           /* trigger_mode    */
            0,           /* level           */
            0,           /* delivery_status */
            0,           /* dest_mode       */
            6,           /* delivery_mode   */
            startup_addr /* vector          */
        ).words[0]
    );
}
