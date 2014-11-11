/*
 * Copyright 2014, General Dynamics C4 Systems
 *
 * This software may be distributed and modified according to the terms of
 * the GNU General Public License version 2. Note that NO WARRANTY is provided.
 * See "LICENSE_GPLv2.txt" for details.
 *
 * @TAG(GD_GPL)
 */

#ifndef __ARCH_API_SENDWAIT_H
#define __ARCH_API_SENDWAIT_H

static inline word_t
arch_getSendWaitSrc(void)
{
    return getRegister(ksCurThread, R8);
}

#endif /* __ARCH_API_SENDWAIT_H */
