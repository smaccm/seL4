#
# Copyright 2014, General Dynamics C4 Systems
#
# This software may be distributed and modified according to the terms of
# the GNU General Public License version 2. Note that NO WARRANTY is provided.
# See "LICENSE_GPLv2.txt" for details.
#
# @TAG(GD_GPL)
#

DIRECTORIES += src/arch/$(ARCH)/64

include ${SOURCE_ROOT}/src/arch/$(ARCH)/64/kernel/Makefile
include ${SOURCE_ROOT}/src/arch/$(ARCH)/64/model/Makefile
include ${SOURCE_ROOT}/src/arch/$(ARCH)/64/machine/Makefile

ARCH_C_SOURCES += 64/c_traps.c
ARCH_ASM_SOURCES += 64/machine_asm.S \
                    64/traps.S \
                    64/head.S
