/*
 * Copyright 2014, General Dynamics C4 Systems
 *
 * This software may be distributed and modified according to the terms of
 * the GNU General Public License version 2. Note that NO WARRANTY is provided.
 * See "LICENSE_GPLv2.txt" for details.
 *
 * @TAG(GD_GPL)
 */

#ifndef __ARCH_MODEL_STATEDATA_H
#define __ARCH_MODEL_STATEDATA_H

#include <config.h>
#include <types.h>
#include <util.h>
#include <object/structures.h>
#include <arch/types.h>
#include <plat/machine/devices.h>

#include <arch/object/iospace.h>
#include <plat/machine/hardware.h>

#include <mode/model/statedata.h>


#define TSS_IO_MAP_SIZE (65536 / 8 / sizeof(word_t) + 1)

typedef struct {
    tss_t   tss;
    word_t  io_map[TSS_IO_MAP_SIZE];
} PACKED tss_io_t;

extern tss_io_t x86KStss;
extern interrupt_t x86KScurInterrupt;
extern gdt_entry_t x86KSgdt[];
extern asid_pool_t* x86KSASIDTable[];
extern tcb_t *x86KSfpuOwner;
extern uint32_t x86KScacheLineSizeBits;
extern idt_entry_t x86KSidt[];
extern user_fpu_state_t x86KSnullFpuState ALIGN(MIN_FPU_ALIGNMENT);
extern uint32_t x86KStscMhz;

extern uint32_t x86KSnumDrhu;
extern vtd_rte_t* x86KSvtdRootTable;
extern uint32_t x86KSnumIOPTLevels;
extern uint32_t x86KSnumIODomainIDBits;
extern uint32_t x86KSFirstValidIODomain;

#ifdef CONFIG_PRINTING
extern uint16_t x86KSconsolePort;
#endif
#ifdef CONFIG_DEBUG_BUILD
extern uint16_t x86KSdebugPort;
#endif

extern x86_irq_state_t x86KSIRQState[];

#endif
