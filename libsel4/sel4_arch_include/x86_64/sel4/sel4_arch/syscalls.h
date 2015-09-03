/*
 * Copyright 2014, NICTA
 *
 * This software may be distributed and modified according to the terms of
 * the BSD 2-Clause license. Note that NO WARRANTY is provided.
 * See "LICENSE_BSD2.txt" for details.
 *
 * @TAG(NICTA_BSD)
 */

#ifndef __LIBSEL4_SEL4_ARCH_SYSCALLS_H
#define __LIBSEL4_SEL4_ARCH_SYSCALLS_H

#include <autoconf.h>

#ifdef CONFIG_SYSENTER
#include <sel4/sel4_arch/syscalls_sysenter.h>
#endif
#ifdef CONFIG_SYSCALL
#include <sel4/sel4_arch/syscalls_syscall.h>
#endif

#endif
