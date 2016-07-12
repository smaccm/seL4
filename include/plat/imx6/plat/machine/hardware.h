/*
 * Copyright 2014, General Dynamics C4 Systems
 *
 * This software may be distributed and modified according to the terms of
 * the GNU General Public License version 2. Note that NO WARRANTY is provided.
 * See "LICENSE_GPLv2.txt" for details.
 *
 * @TAG(GD_GPL)
 */

#ifndef __PLAT_MACHINE_HARDWARE_H
#define __PLAT_MACHINE_HARDWARE_H

#define physBase          0x10000000
#define kernelBase        0xe0000000
#define physMappingOffset (kernelBase - physBase)
#define BASE_OFFSET       physMappingOffset

/*
 * 0xffe00000 asid id slot (arm/arch/kernel/vspace.h)
 * 0xfff00000 devices      (plat/machine/devices.h)
 * 0xffff0000 vectors      (arch/machine/hardware.h)
 * 0xffffc000 global page  (arch/machine/hardware.h)
 * 0xfffff000 kernel stack (arch/machine/hardware.h)
 */

#define PPTR_TOP          0xfff00000
#define PADDR_TOP         (PPTR_TOP - BASE_OFFSET)
#define CLK_MHZ 498llu

#endif /* !__PLAT_MACHINE_HARDWARE_H */
