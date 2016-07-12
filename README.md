<!--
  Copyright 2014, General Dynamics C4 Systems

  This software may be distributed and modified according to the terms of
  the GNU General Public License version 2. Note that NO WARRANTY is provided.
  See "LICENSE_GPLv2.txt" for details.

  @TAG(GD_GPL)
-->

The seL4 Repository (with realtime extensions)
=============================================

This repository contains the source code of seL4 microkernel, with currently in development real-time extensions.

There are no verification guarantees on this branch, although it is scheduled to be verifified in the future.

Currently the only platforms supported on this branch are x86 (processors with TSC-deadline mode), imx6 and odroid-xu. Other platforms do not compile.

This repository is usually not used in isolation, but as part of the build
system in a larger project.

  [1]: http://sel4.systems/
  [2]: http://sel4.systems/Info/FAQ/
  [3]: http://sel4.systems/Community/Contributing/
  [4]: https://zenodo.org/badge/doi/10.5281/zenodo.11247.png

Repository Overview
-------------------

  * `include` and `src`: C and ASM source code of seL4
  * `tools`: build tools
  * `libsel4`: C bindings for the seL4 ABI
  * `manual`: LaTeX sources of the seL4 reference manual


Build Instructions
------------------

tl;dr:

    TOOLPREFIX=arm-none-eabi- ARCH=arm PLAT=imx6 ARMV=armv7-a CPU=cortex-a9 \
 	make

The kernel source requires a cross-compiler for the target architecture. To
build using `make`, follow these instructions:

 * Ensure that the appropriate cross-compiler for your target
   architecture is installed.

 * Set the `TOOLPREFIX` environment variable to your cross-compiler's
   prefix. E.g. `arm-none-eabi-`.

 * Set the `ARCH`, `PLAT`, `ARMV` and `CPU` variables for the intended target
   architecture and platform, chosen from the following lists:

    ARCH | PLAT   | ARMV    | CPU
    -----|--------|---------|-----------
    arm  | imx31  | armv6   | arm1136jf-s
    arm  | omap3  | armv7-a | cortex-a8
    arm  | am335x | armv7-a | cortex-a8
    arm  | imx6   | armv7-a | cortex-a9
    ia32 | pc99   |         |

 * For a debug build, append `DEBUG=y` and to see serial output additionally
   append `CONFIG_KERNEL_EXTRA_CPPFLAGS="-DCONFIG_PRINTING=y -DCONFIG_USER_STACK_TRACE_LENGTH=16"`.

See the seL4 website for more [comprehensive build instructions][5].

 [5]: http://sel4.systems/Info/GettingStarted/


License
=======

The files in this repository are released under standard open source licenses.
Please see the individual file headers and `LICENSE_GPLv2.txt` and
`LICENSE_BSD2.txt` files for details.
