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
#include <sel4/arch/functions.h>
#include <sel4/types.h>

#define __SEL4_SWINUM(x) ((x) & 0x00ffffff)

#ifndef __OPTIMIZE__
/* With no optimisations (-O0) GCC's register allocator clobbers the
 * syscall arguments before you reach the 'swi' and you invoke the kernel
 * incorrectly.
 * See SELFOUR-187
 */
#warning you are compiling with -O0; syscalls will most likely not work
#endif

static inline void
seL4_Send(seL4_CPtr dest, seL4_MessageInfo_t msgInfo)
{
    register seL4_Word destptr asm("r0") = (seL4_Word)dest;
    register seL4_Word info asm("r1") = msgInfo.words[0];

    /* Load beginning of the message into registers. */
    register seL4_Word msg0 asm("r2") = seL4_GetMR(0);
    register seL4_Word msg1 asm("r3") = seL4_GetMR(1);
    register seL4_Word msg2 asm("r4") = seL4_GetMR(2);
    register seL4_Word msg3 asm("r5") = seL4_GetMR(3);

    /* Perform the system call. */
    register seL4_Word scno asm("r7") = seL4_SysSend;
    asm volatile ("swi %[swi_num]"
                  : "+r" (destptr), "+r" (msg0), "+r" (msg1), "+r" (msg2),
                  "+r" (msg3), "+r" (info)
                  : [swi_num] "i" __SEL4_SWINUM(seL4_SysSend), "r"(scno)
                  : "memory");
}

#ifdef CONFIG_LIB_SEL4_HAVE_REGISTER_STUBS
static inline void
seL4_SendWithMRs(seL4_CPtr dest, seL4_MessageInfo_t msgInfo,
                 seL4_Word *mr0, seL4_Word *mr1, seL4_Word *mr2, seL4_Word *mr3)
{
    register seL4_Word destptr asm("r0") = (seL4_Word)dest;
    register seL4_Word info asm("r1") = msgInfo.words[0];

    /* Load beginning of the message into registers. */
    register seL4_Word msg0 asm("r2");
    register seL4_Word msg1 asm("r3");
    register seL4_Word msg2 asm("r4");
    register seL4_Word msg3 asm("r5");
    register seL4_Word scno asm("r7") = seL4_SysSend;

    if (mr0 != seL4_Null && seL4_MessageInfo_get_length(msgInfo) > 0) {
        msg0 = *mr0;
    }
    if (mr1 != seL4_Null && seL4_MessageInfo_get_length(msgInfo) > 1) {
        msg1 = *mr1;
    }
    if (mr2 != seL4_Null && seL4_MessageInfo_get_length(msgInfo) > 2) {
        msg2 = *mr2;
    }
    if (mr3 != seL4_Null && seL4_MessageInfo_get_length(msgInfo) > 3) {
        msg3 = *mr3;
    }

    /* Perform the system call. */
    asm volatile ("swi %[swi_num]"
                  : "+r" (destptr), "+r" (msg0), "+r" (msg1), "+r" (msg2),
                  "+r" (msg3), "+r" (info)
                  : [swi_num] "i" __SEL4_SWINUM(seL4_SysSend), "r"(scno)
                  : "memory");
}
#endif

static inline void
seL4_NBSend(seL4_CPtr dest, seL4_MessageInfo_t msgInfo)
{
    register seL4_Word destptr asm("r0") = (seL4_Word)dest;
    register seL4_Word info asm("r1") = msgInfo.words[0];

    /* Load beginning of the message into registers. */
    register seL4_Word msg0 asm("r2") = seL4_GetMR(0);
    register seL4_Word msg1 asm("r3") = seL4_GetMR(1);
    register seL4_Word msg2 asm("r4") = seL4_GetMR(2);
    register seL4_Word msg3 asm("r5") = seL4_GetMR(3);

    /* Perform the system call. */
    register seL4_Word scno asm("r7") = seL4_SysNBSend;
    asm volatile ("swi %[swi_num]"
                  : "+r" (destptr), "+r" (msg0), "+r" (msg1), "+r" (msg2),
                  "+r" (msg3), "+r" (info)
                  : [swi_num] "i" __SEL4_SWINUM(seL4_SysNBSend), "r"(scno)
                  : "memory");
}

#ifdef CONFIG_LIB_SEL4_HAVE_REGISTER_STUBS
static inline void
seL4_NBSendWithMRs(seL4_CPtr dest, seL4_MessageInfo_t msgInfo,
                   seL4_Word *mr0, seL4_Word *mr1, seL4_Word *mr2, seL4_Word *mr3)
{
    register seL4_Word destptr asm("r0") = (seL4_Word)dest;
    register seL4_Word info asm("r1") = msgInfo.words[0];

    /* Load beginning of the message into registers. */
    register seL4_Word msg0 asm("r2");
    register seL4_Word msg1 asm("r3");
    register seL4_Word msg2 asm("r4");
    register seL4_Word msg3 asm("r5");
    register seL4_Word scno asm("r7") = seL4_SysNBSend;

    if (mr0 != seL4_Null && seL4_MessageInfo_get_length(msgInfo) > 0) {
        msg0 = *mr0;
    }
    if (mr1 != seL4_Null && seL4_MessageInfo_get_length(msgInfo) > 1) {
        msg1 = *mr1;
    }
    if (mr2 != seL4_Null && seL4_MessageInfo_get_length(msgInfo) > 2) {
        msg2 = *mr2;
    }
    if (mr3 != seL4_Null && seL4_MessageInfo_get_length(msgInfo) > 3) {
        msg3 = *mr3;
    }

    /* Perform the system call. */
    asm volatile ("swi %[swi_num]"
                  : "+r" (destptr), "+r" (msg0), "+r" (msg1), "+r" (msg2),
                  "+r" (msg3), "+r" (info)
                  : [swi_num] "i" __SEL4_SWINUM(seL4_SysNBSend), "r"(scno)
                  : "memory");
}
#endif

static inline void
seL4_Reply(seL4_MessageInfo_t msgInfo)
{
    register seL4_Word info asm("r1") = msgInfo.words[0];

    /* Load beginning of the message into registers. */
    register seL4_Word msg0 asm("r2") = seL4_GetMR(0);
    register seL4_Word msg1 asm("r3") = seL4_GetMR(1);
    register seL4_Word msg2 asm("r4") = seL4_GetMR(2);
    register seL4_Word msg3 asm("r5") = seL4_GetMR(3);

    /* Perform the system call. */
    register seL4_Word scno asm("r7") = seL4_SysReply;
    asm volatile ("swi %[swi_num]"
                  : "+r" (msg0), "+r" (msg1), "+r" (msg2), "+r" (msg3),
                  "+r" (info)
                  : [swi_num] "i" __SEL4_SWINUM(seL4_SysReply), "r"(scno)
                  : "memory");
}

#ifdef CONFIG_LIB_SEL4_HAVE_REGISTER_STUBS
static inline void
seL4_ReplyWithMRs(seL4_MessageInfo_t msgInfo,
                  seL4_Word *mr0, seL4_Word *mr1, seL4_Word *mr2, seL4_Word *mr3)
{
    register seL4_Word info asm("r1") = msgInfo.words[0];

    /* Load beginning of the message into registers. */
    register seL4_Word msg0 asm("r2");
    register seL4_Word msg1 asm("r3");
    register seL4_Word msg2 asm("r4");
    register seL4_Word msg3 asm("r5");
    register seL4_Word scno asm("r7") = seL4_SysReply;

    if (mr0 != seL4_Null && seL4_MessageInfo_get_length(msgInfo) > 0) {
        msg0 = *mr0;
    }
    if (mr1 != seL4_Null && seL4_MessageInfo_get_length(msgInfo) > 1) {
        msg1 = *mr1;
    }
    if (mr2 != seL4_Null && seL4_MessageInfo_get_length(msgInfo) > 2) {
        msg2 = *mr2;
    }
    if (mr3 != seL4_Null && seL4_MessageInfo_get_length(msgInfo) > 3) {
        msg3 = *mr3;
    }

    /* Perform the system call. */
    asm volatile ("swi %[swi_num]"
                  : "+r" (msg0), "+r" (msg1), "+r" (msg2), "+r" (msg3),
                  "+r" (info)
                  : [swi_num] "i" __SEL4_SWINUM(seL4_SysReply), "r"(scno)
                  : "memory");
}
#endif

static inline void
seL4_Signal(seL4_CPtr dest)
{
    register seL4_Word destptr asm("r0") = (seL4_Word)dest;
    register seL4_Word info asm("r1") = seL4_MessageInfo_new(0, 0, 0, 0).words[0];

    /* Perform the system call. */
    register seL4_Word scno asm("r7") = seL4_SysSend;
    asm volatile ("swi %[swi_num]"
                  : "+r" (destptr), "+r" (info)
                  : [swi_num] "i" __SEL4_SWINUM(seL4_SysSend), "r"(scno)
                  : "memory");
}

static inline seL4_MessageInfo_t
seL4_Recv(seL4_CPtr src, seL4_Word* sender)
{
    register seL4_Word src_and_badge asm("r0") = (seL4_Word)src;
    register seL4_MessageInfo_t info asm("r1");

    /* Incoming message registers. */
    register seL4_Word msg0 asm("r2");
    register seL4_Word msg1 asm("r3");
    register seL4_Word msg2 asm("r4");
    register seL4_Word msg3 asm("r5");

    /* Perform the system call. */
    register seL4_Word scno asm("r7") = seL4_SysRecv;
    asm volatile ("swi %[swi_num]"
                  : "=r" (msg0), "=r" (msg1), "=r" (msg2), "=r" (msg3),
                  "=r" (info), "+r" (src_and_badge)
                  : [swi_num] "i" __SEL4_SWINUM(seL4_SysRecv), "r"(scno)
                  : "memory");

    /* Write the message back out to memory. */
    seL4_SetMR(0, msg0);
    seL4_SetMR(1, msg1);
    seL4_SetMR(2, msg2);
    seL4_SetMR(3, msg3);

    /* Return back sender and message information. */
    if (sender) {
        *sender = src_and_badge;
    }
    return (seL4_MessageInfo_t) {
        .words = {info.words[0]}
    };
}

#ifdef CONFIG_LIB_SEL4_HAVE_REGISTER_STUBS
static inline seL4_MessageInfo_t
seL4_RecvWithMRs(seL4_CPtr src, seL4_Word* sender,
                 seL4_Word *mr0, seL4_Word *mr1, seL4_Word *mr2, seL4_Word *mr3)
{
    register seL4_Word src_and_badge asm("r0") = (seL4_Word)src;
    register seL4_MessageInfo_t info asm("r1");

    /* Incoming message registers. */
    register seL4_Word msg0 asm("r2");
    register seL4_Word msg1 asm("r3");
    register seL4_Word msg2 asm("r4");
    register seL4_Word msg3 asm("r5");

    /* Perform the system call. */
    register seL4_Word scno asm("r7") = seL4_SysRecv;
    asm volatile ("swi %[swi_num]"
                  : "=r" (msg0), "=r" (msg1), "=r" (msg2), "=r" (msg3),
                  "=r" (info.words[0]), "+r" (src_and_badge)
                  : [swi_num] "i" __SEL4_SWINUM(seL4_SysRecv), "r"(scno)
                  : "memory");

    /* Write the message back out to memory. */
    if (mr0 != seL4_Null) {
        *mr0 = msg0;
    }
    if (mr1 != seL4_Null) {
        *mr1 = msg1;
    }
    if (mr2 != seL4_Null) {
        *mr2 = msg2;
    }
    if (mr3 != seL4_Null) {
        *mr3 = msg3;
    }

    /* Return back sender and message information. */
    if (sender) {
        *sender = src_and_badge;
    }
    return (seL4_MessageInfo_t) {
        .words = {info.words[0]}
    };
}

#endif

static inline seL4_MessageInfo_t
seL4_NBRecv(seL4_CPtr src, seL4_Word* sender)
{
    register seL4_Word src_and_badge asm("r0") = (seL4_Word)src;
    register seL4_MessageInfo_t info asm("r1");

    /* Incoming message registers. */
    register seL4_Word msg0 asm("r2");
    register seL4_Word msg1 asm("r3");
    register seL4_Word msg2 asm("r4");
    register seL4_Word msg3 asm("r5");

    /* Perform the system call. */
    register seL4_Word scno asm("r7") = seL4_SysNBRecv;
    asm volatile ("swi %[swi_num]"
                  : "=r" (msg0), "=r" (msg1), "=r" (msg2), "=r" (msg3),
                  "=r" (info), "+r" (src_and_badge)
                  : [swi_num] "i" __SEL4_SWINUM(seL4_SysNBRecv), "r"(scno)
                  : "memory");

    /* Write the message back out to memory. */
    seL4_SetMR(0, msg0);
    seL4_SetMR(1, msg1);
    seL4_SetMR(2, msg2);
    seL4_SetMR(3, msg3);

    /* Return back sender and message information. */
    if (sender) {
        *sender = src_and_badge;
    }
    return (seL4_MessageInfo_t) {
        .words = { info.words[0]}
    };
}

static inline seL4_MessageInfo_t
seL4_Call(seL4_CPtr dest, seL4_MessageInfo_t msgInfo)
{
    register seL4_Word destptr asm("r0") = (seL4_Word)dest;
    register seL4_MessageInfo_t info asm("r1") = msgInfo;

    /* Load beginning of the message into registers. */
    register seL4_Word msg0 asm("r2") = seL4_GetMR(0);
    register seL4_Word msg1 asm("r3") = seL4_GetMR(1);
    register seL4_Word msg2 asm("r4") = seL4_GetMR(2);
    register seL4_Word msg3 asm("r5") = seL4_GetMR(3);

    /* Perform the system call. */
    register seL4_Word scno asm("r7") = seL4_SysCall;
    asm volatile ("swi %[swi_num]"
                  : "+r" (msg0), "+r" (msg1), "+r" (msg2), "+r" (msg3),
                  "+r" (info), "+r" (destptr)
                  : [swi_num] "i" __SEL4_SWINUM(seL4_SysCall), "r"(scno)
                  : "memory");

    /* Write out the data back to memory. */
    seL4_SetMR(0, msg0);
    seL4_SetMR(1, msg1);
    seL4_SetMR(2, msg2);
    seL4_SetMR(3, msg3);

    return (seL4_MessageInfo_t) {
        .words = {info.words[0]}
    };
}

#ifdef CONFIG_LIB_SEL4_HAVE_REGISTER_STUBS
static inline seL4_MessageInfo_t
seL4_CallWithMRs(seL4_CPtr dest, seL4_MessageInfo_t msgInfo,
                 seL4_Word *mr0, seL4_Word *mr1, seL4_Word *mr2, seL4_Word *mr3)
{
    register seL4_Word destptr asm("r0") = (seL4_Word)dest;
    register seL4_MessageInfo_t info asm("r1") = msgInfo;

    register seL4_Word msg0 asm("r2");
    register seL4_Word msg1 asm("r3");
    register seL4_Word msg2 asm("r4");
    register seL4_Word msg3 asm("r5");
    register seL4_Word scno asm("r7") = seL4_SysCall;

    /* Load beginning of the message into registers. */
    if (mr0 != seL4_Null && seL4_MessageInfo_get_length(msgInfo) > 0) {
        msg0 = *mr0;
    }
    if (mr1 != seL4_Null && seL4_MessageInfo_get_length(msgInfo) > 1) {
        msg1 = *mr1;
    }
    if (mr2 != seL4_Null && seL4_MessageInfo_get_length(msgInfo) > 2) {
        msg2 = *mr2;
    }
    if (mr3 != seL4_Null && seL4_MessageInfo_get_length(msgInfo) > 3) {
        msg3 = *mr3;
    }

    /* Perform the system call. */
    asm volatile ("swi %[swi_num]"
                  : "+r" (msg0), "+r" (msg1), "+r" (msg2), "+r" (msg3),
                  "+r" (info), "+r" (destptr)
                  : [swi_num] "i" __SEL4_SWINUM(seL4_SysCall), "r"(scno)
                  : "memory");

    /* Write out the data back to memory. */
    if (mr0 != seL4_Null) {
        *mr0 = msg0;
    }
    if (mr1 != seL4_Null) {
        *mr1 = msg1;
    }
    if (mr2 != seL4_Null) {
        *mr2 = msg2;
    }
    if (mr3 != seL4_Null) {
        *mr3 = msg3;
    }

    return (seL4_MessageInfo_t) {
        .words = {info.words[0]}
    };
}
#endif

static inline seL4_MessageInfo_t
seL4_ReplyRecv(seL4_CPtr src, seL4_MessageInfo_t msgInfo, seL4_Word *sender)
{
    register seL4_Word src_and_badge asm("r0") = (seL4_Word)src;
    register seL4_MessageInfo_t info asm("r1") = msgInfo;

    /* Load beginning of the message into registers. */
    register seL4_Word msg0 asm("r2") = seL4_GetMR(0);
    register seL4_Word msg1 asm("r3") = seL4_GetMR(1);
    register seL4_Word msg2 asm("r4") = seL4_GetMR(2);
    register seL4_Word msg3 asm("r5") = seL4_GetMR(3);

    /* Perform the syscall. */
    register seL4_Word scno asm("r7") = seL4_SysReplyRecv;
    asm volatile ("swi %[swi_num]"
                  : "+r" (msg0), "+r" (msg1), "+r" (msg2), "+r" (msg3),
                  "+r" (info), "+r" (src_and_badge)
                  : [swi_num] "i" __SEL4_SWINUM(seL4_SysReplyRecv), "r"(scno)
                  : "memory");

    /* Write the message back out to memory. */
    seL4_SetMR(0, msg0);
    seL4_SetMR(1, msg1);
    seL4_SetMR(2, msg2);
    seL4_SetMR(3, msg3);

    /* Return back sender and message information. */
    if (sender) {
        *sender = src_and_badge;
    }
    return (seL4_MessageInfo_t) {
        .words = {info.words[0]}
    };
}

#ifdef CONFIG_LIB_SEL4_HAVE_REGISTER_STUBS
static inline seL4_MessageInfo_t
seL4_ReplyRecvWithMRs(seL4_CPtr src, seL4_MessageInfo_t msgInfo, seL4_Word *sender,
                      seL4_Word *mr0, seL4_Word *mr1, seL4_Word *mr2, seL4_Word *mr3)
{
    register seL4_Word src_and_badge asm("r0") = (seL4_Word)src;
    register seL4_MessageInfo_t info asm("r1") = msgInfo;

    /* Load beginning of the message into registers. */
    register seL4_Word msg0 asm("r2");
    register seL4_Word msg1 asm("r3");
    register seL4_Word msg2 asm("r4");
    register seL4_Word msg3 asm("r5");
    register seL4_Word scno asm("r7") = seL4_SysReplyRecv;

    if (mr0 != seL4_Null && seL4_MessageInfo_get_length(msgInfo) > 0) {
        msg0 = *mr0;
    }
    if (mr1 != seL4_Null && seL4_MessageInfo_get_length(msgInfo) > 1) {
        msg1 = *mr1;
    }
    if (mr2 != seL4_Null && seL4_MessageInfo_get_length(msgInfo) > 2) {
        msg2 = *mr2;
    }
    if (mr3 != seL4_Null && seL4_MessageInfo_get_length(msgInfo) > 3) {
        msg3 = *mr3;
    }

    /* Perform the syscall. */
    asm volatile ("swi %[swi_num]"
                  : "+r" (msg0), "+r" (msg1), "+r" (msg2), "+r" (msg3),
                  "+r" (info), "+r" (src_and_badge)
                  : [swi_num] "i" __SEL4_SWINUM(seL4_SysReplyRecv), "r"(scno)
                  : "memory");

    /* Write out the data back to memory. */
    if (mr0 != seL4_Null) {
        *mr0 = msg0;
    }
    if (mr1 != seL4_Null) {
        *mr1 = msg1;
    }
    if (mr2 != seL4_Null) {
        *mr2 = msg2;
    }
    if (mr3 != seL4_Null) {
        *mr3 = msg3;
    }

    /* Return back sender and message information. */
    if (sender) {
        *sender = src_and_badge;
    }
    return (seL4_MessageInfo_t) {
        .words = {info.words[0]}
    };
}
#endif

static inline seL4_MessageInfo_t
seL4_SignalRecv(seL4_CPtr dest, seL4_CPtr src, seL4_Word* sender)
{
    register seL4_Word dest_and_badge asm("r0") = (seL4_Word) dest;
    register seL4_Word src_and_info asm("r1") = src;

    /* Load beginning of the message into registers. */
    register seL4_Word msg0 asm("r2");
    register seL4_Word msg1 asm("r3");
    register seL4_Word msg2 asm("r4");
    register seL4_Word msg3 asm("r5");

    register seL4_Word scno asm("r7") = seL4_SysSignalRecv;

    asm volatile (" swi %[swi_num]\n"
                  : "=r" (msg0), "=r" (msg1), "=r" (msg2), "=r" (msg3),
                  "+r" (src_and_info), "+r" (dest_and_badge)
                  : [swi_num] "i" __SEL4_SWINUM(seL4_SysSignalRecv), "r"(scno)
                  : "memory");

    /* Write the message back out to memory. */
    seL4_SetMR(0, msg0);
    seL4_SetMR(1, msg1);
    seL4_SetMR(2, msg2);
    seL4_SetMR(3, msg3);

    /* Return back sender and message information. */
    if (sender) {
        *sender = dest_and_badge;
    }
    return (seL4_MessageInfo_t) {
        .words = {src_and_info}
    };
}

#ifdef CONFIG_LIB_SEL4_HAVE_REGISTER_STUBS
static inline seL4_MessageInfo_t
seL4_SignalRecvWithMRs(seL4_CPtr dest, seL4_CPtr src, seL4_Word *sender,
                       seL4_Word *mr0, seL4_Word *mr1, seL4_Word *mr2, seL4_Word *mr3)
{
    register seL4_Word dest_and_badge asm("r0") = (seL4_Word) dest;
    register seL4_Word src_and_info asm("r1") = src;

    /* Load beginning of the message into registers. */
    register seL4_Word msg0 asm("r2");
    register seL4_Word msg1 asm("r3");
    register seL4_Word msg2 asm("r4");
    register seL4_Word msg3 asm("r5");
    register seL4_Word scno asm("r7") = seL4_SysSignalRecv;

    /* Perform the syscall. */
    asm volatile (" swi %[swi_num]\n"
                  : "=r" (msg0), "=r" (msg1), "=r" (msg2), "=r" (msg3),
                  "+r" (src_and_info), "+r" (dest_and_badge)
                  : [swi_num] "i" __SEL4_SWINUM(seL4_SysSignalRecv), "r"(scno)
                  : "memory");

    /* Write out the data back to memory. */
    if (mr0 != seL4_Null) {
        *mr0 = msg0;
    }
    if (mr1 != seL4_Null) {
        *mr1 = msg1;
    }
    if (mr2 != seL4_Null) {
        *mr2 = msg2;
    }
    if (mr3 != seL4_Null) {
        *mr3 = msg3;
    }

    /* Return back sender and message information. */
    if (sender) {
        *sender = dest_and_badge;
    }

    return (seL4_MessageInfo_t) {
        .words = {src_and_info}
    };
}
#endif

#ifdef SEL4_DEBUG_KERNEL
static inline void
seL4_DebugPutChar(char c)
{
    register seL4_Word arg1 asm("r0") = c;
    register seL4_Word scno asm("r7") = seL4_SysDebugPutChar;
    asm volatile ("swi %[swi_num]"
                  : /* no outputs */
                  : [swi_num] "i" __SEL4_SWINUM(seL4_SysDebugPutChar), "r" (arg1), "r"(scno));
}
#endif

#ifdef SEL4_DEBUG_KERNEL
static inline void
seL4_DebugHalt(void)
{
    register seL4_Word scno asm("r7") = seL4_SysDebugHalt;
    asm volatile ("swi %[swi_num]"
                  : /* no outputs */
                  : [swi_num] "i" __SEL4_SWINUM(seL4_SysDebugHalt), "r"(scno));
}
#endif

#ifdef SEL4_DEBUG_KERNEL
static inline void
seL4_DebugSnapshot(void)
{
    register seL4_Word scno asm("r7") = seL4_SysDebugSnapshot;
    asm volatile ("swi %[swi_num]"
                  : /* no outputs */
                  : [swi_num] "i" __SEL4_SWINUM(seL4_SysDebugSnapshot), "r"(scno));
}
#endif

#ifdef SEL4_DEBUG_KERNEL
static inline seL4_Uint32
seL4_DebugCapIdentify(seL4_CPtr cap)
{
    register seL4_Word arg1 asm("r0") = cap;
    register seL4_Word scno asm("r7") = seL4_SysDebugCapIdentify;
    asm volatile ("swi %[swi_num]"
                  : "+r"(arg1)
                  : [swi_num] "i" __SEL4_SWINUM(seL4_SysDebugCapIdentify), "r"(scno));
    return (seL4_Uint32)arg1;
}
#endif

#ifdef CONFIG_PRINTING

char *strcpy(char *, const char *);
static inline void
seL4_DebugNameThread(seL4_CPtr tcb, const char *name)
{
    strcpy((char*)seL4_GetIPCBuffer()->msg, name);

    register seL4_Word arg1 asm("r0") = tcb;
    register seL4_Word scno asm("r7") = seL4_SysDebugNameThread;
    asm volatile ("swi %[swi_num]"
                  : "+r"(arg1)
                  : [swi_num] "i" __SEL4_SWINUM(seL4_SysDebugNameThread), "r"(scno)
                  : "memory");
}
#endif

#ifdef SEL4_DANGEROUS_CODE_INJECTION_KERNEL
static inline void
seL4_DebugRun(void (* userfn) (void *), void* userarg)
{
    register seL4_Word arg1 asm("r0") = (seL4_Word)userfn;
    register seL4_Word arg2 asm("r1") = (seL4_Word)userarg;
    register seL4_Word scno asm("r7") = seL4_SysDebugRun;
    asm volatile ("swi %[swi_num]"
                  : /* no outputs */
                  : [swi_num] "i" __SEL4_SWINUM(seL4_SysDebugRun), "r" (arg1), "r" (arg2), "r"(scno)
                  : "memory"
                 );
}
#endif

#ifdef CONFIG_ENABLE_BENCHMARKS
/* set the log index back to 0 */
static inline void
seL4_BenchmarkResetLog(void)
{
    register seL4_Word scno asm("r7") = seL4_SysBenchmarkResetLog;
    asm volatile ("swi %[swi_num]"
                  : /* no outputs */
                  : [swi_num] "i" __SEL4_SWINUM(seL4_SysBenchmarkResetLog), "r"(scno)
                 );
}

/* read size words from the log starting from start into the ipc buffer.
 * @return the amount sucessfully read. Will cap at ipc buffer size and at size of
 * recorded log */
static inline seL4_Uint32
seL4_BenchmarkDumpLog(seL4_Word start, seL4_Word size)
{

    register seL4_Word arg1 asm("r0") = (seL4_Word) start;
    register seL4_Word arg2 asm("r1") = (seL4_Word) size;
    register seL4_Word scno asm("r7") = seL4_SysBenchmarkDumpLog;
    asm volatile ("swi %[swi_num]"
                  : "+r" (arg1)
                  : [swi_num] "i" __SEL4_SWINUM(seL4_SysBenchmarkDumpLog), "r" (arg1), "r" (arg2), "r"(scno)
                  : "memory");

    return (seL4_Uint32) arg1;

}

/* Return the amount of things we tried to log. This could be greater than
 * the size of the log itself */
static inline seL4_Uint32
seL4_BenchmarkLogSize(void)
{

    register seL4_Word arg1 asm("r0") = 0; /* required for retval */
    register seL4_Word scno asm("r7") = seL4_SysBenchmarkLogSize;
    asm volatile ("swi %[swi_num]"
                  : "+r" (arg1)
                  : [swi_num] "i" __SEL4_SWINUM(seL4_SysBenchmarkLogSize), "r"(scno));

    return (seL4_Uint32) arg1;

}

static inline void
seL4_BenchmarkFinalizeLog(void)
{
    register seL4_Word scno asm("r7") = seL4_SysBenchmarkFinalizeLog;
    asm volatile ("swi %[swi_num]"
                  : /* no outputs */
                  : [swi_num] "i" __SEL4_SWINUM(seL4_SysBenchmarkFinalizeLog), "r"(scno)
                 );
}

#ifdef CONFIG_BENCHMARK_TRACK_UTILISATION
static inline void
seL4_BenchmarkGetThreadUtilisation(seL4_Word tcp_cptr)
{
    register seL4_Word arg1 asm("r0") = tcp_cptr;
    register seL4_Word scno asm("r7") = seL4_SysBenchmarkGetThreadUtilisation;
    asm volatile ("swi %[swi_num]"
                  : /* no outputs */
                  : [swi_num] "i" __SEL4_SWINUM(seL4_SysBenchmarkGetThreadUtilisation), "r" (arg1), "r"(scno)
                  : "memory");
}
#endif /* CONFIG_BENCHMARK_TRACK_UTILISATION */
#endif /* CONFIG_ENABLE_BENCHMARKS */

#endif
