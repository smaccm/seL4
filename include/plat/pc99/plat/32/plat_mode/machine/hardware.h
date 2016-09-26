/*
 * Copyright 2014, General Dynamics C4 Systems
 *
 * This software may be distributed and modified according to the terms of
 * the GNU General Public License version 2. Note that NO WARRANTY is provided.
 * See "LICENSE_GPLv2.txt" for details.
 *
 * @TAG(GD_GPL)
 */

#ifndef __PLAT_MODE_MACHINE_HARDWARE_H
#define __PLAT_MODE_MACHINE_HARDWARE_H

/* WARNING: some of these constants are also defined in linker.lds */
#define PADDR_BASE  0x00000000
#define PADDR_LOAD  0x00100000
#define PPTR_BASE   0xe0000000
#ifdef CONFIG_PAE_PAGING
#define PPTR_USER_TOP (PPTR_BASE & (~MASK(X86_1G_bits)))
#else
#define PPTR_USER_TOP (PPTR_BASE & (~MASK(X86_4M_bits)))
#endif
#ifdef CONFIG_BENCHMARK_USE_KERNEL_LOG_BUFFER
#define PPTR_TOP    (-BIT(LARGE_PAGE_BITS + 1))
#else
#define PPTR_TOP    (-BIT(LARGE_PAGE_BITS))
#endif /* CONFIG_BENCHMARK_USE_KERNEL_LOG_BUFFER */
#define PPTR_KDEV   0xffff0000
#define BASE_OFFSET (PPTR_BASE - PADDR_BASE)
#define physMappingOffset BASE_OFFSET
#define kernelBase PPTR_USER_TOP

#define PADDR_TOP   (PPTR_TOP - BASE_OFFSET)

/* The maximum physical address for device untypeds that we export to
 * the user */
#define PADDR_USER_DEVICE_TOP 0xffff0000

/* The kernel base offset is a way to translate the kernel image segment
 * from virtual to physical. This translation must be a single offset for
 * for the entire segment (i.e. the kernel image must be contiguous both
 * virtually and physically) */
#define KERNEL_BASE_OFFSET BASE_OFFSET

/* For a 32-bit system there is no difference in how we translates
 * physical address for the kernel symbols or anything else */
#define paddr_to_kpptr(x) paddr_to_pptr(x)
#define kpptr_to_paddr(x) pptr_to_paddr(x)

#endif
