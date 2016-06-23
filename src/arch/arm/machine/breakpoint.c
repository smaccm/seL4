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
#include <machine/registerset.h>
#include <api/constants.h> /* seL4_NumBreakpoints */

enum breakpoint_type {
    BP_TYPE_UNLINKED_INSTRUCTION_MATCH=0,
    BP_TYPE_LINKED_INSTRUCTION_MATCH=0x1u,
    BP_TYPE_UNLINKED_CONTEXT_MATCH=0x2u,
    BP_TYPE_LINKED_CONTEXT_MATCH=0x3u,

    BP_TYPE_UNLINKED_INSTRUCTION_MISMATCH=0x4u,
    BP_TYPE_LINKED_INSTRUCTION_MISMATCH=0x5u,

    BP_TYPE_UNLINKED_VMID_MATCH=0x8u,
    BP_TYPE_LINKED_VMID_MATCH=0x9u,
    BP_TYPE_UNLINKED_VMID_AND_CONTEXT_MATCH=0xAu,
    BP_TYPE_LINKED_VMID_AND_CONTEXT_MATCH=0xBu
};

enum debug_reg_indexes {
    DBGDIDR=0,
    DBGDSCR,
    DBGWFAR=6,
    DBGDVR_BASE=64,
    DBGBCR_BASE=80,
    DBGWVR_BASE=96,
    DBGWCR_BASE=112,
    DBGBXVR_BASE=144,

    DBGOSLAR=192,
    DBGOSLSR,
    DBGOSBLR,

    DBGDEVID2=1008,
    DBGDEVID1,
    DBGDEVID,
    DBGDEVTYPE,

    DBGDRAR=128,
    DBGDSAR=256,

    DBGAUTHSTATUS=1006
};

#define DBGDVR_OFFSET(n) (DBGDVR_BASE + n)
#define DBGDCR_OFFSET(n) (DBGDCR_BASE + n)
#define DBGWCR_OFFSET(n) (DBGWCR_BASE + n)
#define DBGWVR_OFFSET(n) (DBGWVR_BASE + n)
#define DBGBXVR_OFFSET(n) (DBGBXVR_BASE + n)

/* C3.3.4: "A debugger can use either byte address selection or address range
 *  masking, if it is implemented. However, it must not attempt to use both at
 * the same time"
 *
 * "v7 Debug and v7.1 Debug deprecate any use of the DBGBCR.MASK field."
 * ^ So prefer to use DBGBCR.BAS instead. When using masking, you must set
 * BAS to all 1s, and when using BAS you must set the MASK field to all 0s.
 *
 * To detect support for BPAddrMask:
 *  * When it's unsupported: DBGBCR.MASK is always RAZ/WI, and EITHER:
 *      * DBGIDR.DEVID_tmp is RAZ
 *      * OR DBGIDR.DEVID_tmp is RAO and DBGDEVID.{CIDMask, BPAddrMask} are RAZ.
 *  * OR:
 *      * DBGDEVID.BPAddrMask indicates whether addr masking is supported.
 *      * DBGBCR.MASK is UNK/SBZP.
 *
 * Setting BAS to 0b0000 makes the cpu break on every instruction.
 * Be aware that the processor checks the MASK before the BAS.
 * You must set BAS to 0b1111 for all context match comparisons.
 *
 *
 * DBGDIDR.WRPs states how many watchpoint regs are available, and is between
 * 1 and 16.
 *
 *
 */

/* ARM allows watchpoint trigger on load, load-exclusive, and "swap" accesses.
 * store, store-exclusive and "swap" accesses. All accesses.
 *
 * The mask defines which bits are EXCLUDED from the comparison.
 * Always program the DBGDWVR with a WORD aligned address, and use the BAS to
 * state which bits form part of the match.
 *
 * It seems the BAS works as a bitmask of bytes to select in the range.
 *
 * To detect support for the 8-bit BAS field:
 *  * If the 8-bit BAS is unsupported, then BAS[7:4] is RAZ/WI.
 *
 * When using an 8-byte watchpoint that is not dword aligned, the result is
 * undefined. You should program it as the aligned base of the range, and select
 * only the relevant bytes then.
 *
 * You cannot do sparse byte selection: you either select a single byte in the
 * BAS or you select a contiguous range. ARM has deprecated sparse byte
 * selection.
 */

/* Accesses to the memory mapped interface vs coprocessor interface.
 *
 * DBGSWENABLE: "This controls access to all debug registers through the
 *  memory-mapped interface, and access to
 *  certain debug registers through the CP14 interface."
 *
 *

bool_t Arch_initHardwareBreakpoints(void)
{

    return true;
}
