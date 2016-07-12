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

static inline void
seL4_Send(seL4_CPtr dest, seL4_MessageInfo_t msgInfo)
{
    asm volatile (
        "pushl %%ebp       \n"
        "movl %%ecx, %%ebp \n"
        "movl %%esp, %%ecx \n"
        "leal 1f, %%edx    \n"
        "1:                \n"
        "sysenter          \n"
        "popl %%ebp        \n"
        :
        : "a" (seL4_SysSend),
        "b" (dest),
        "S" (msgInfo.words[0]),
        "D" (seL4_GetMR(0)),
        "c" (seL4_GetMR(1))
        : "%edx"
    );
}

static inline void
seL4_SendWithMRs(seL4_CPtr dest, seL4_MessageInfo_t msgInfo,
                 seL4_Word *mr0, seL4_Word *mr1)
{
    asm volatile (
        "pushl %%ebp       \n"
        "movl %%ecx, %%ebp \n"
        "movl %%esp, %%ecx \n"
        "leal 1f, %%edx    \n"
        "1:                \n"
        "sysenter          \n"
        "popl %%ebp        \n"
        :
        : "a" (seL4_SysSend),
        "b" (dest),
        "S" (msgInfo.words[0]),
        "D" (mr0 != seL4_Null ? *mr0 : 0),
        "c" (mr1 != seL4_Null ? *mr1 : 0)
        : "%edx"
    );
}

static inline void
seL4_NBSend(seL4_CPtr dest, seL4_MessageInfo_t msgInfo)
{
    asm volatile (
        "pushl %%ebp       \n"
        "movl %%ecx, %%ebp \n"
        "movl %%esp, %%ecx \n"
        "leal 1f, %%edx    \n"
        "1:                \n"
        "sysenter          \n"
        "popl %%ebp        \n"
        :
        : "a" (seL4_SysNBSend),
        "b" (dest),
        "S" (msgInfo.words[0]),
        "D" (seL4_GetMR(0)),
        "c" (seL4_GetMR(1))
        : "%edx"
    );
}

static inline void
seL4_NBSendWithMRs(seL4_CPtr dest, seL4_MessageInfo_t msgInfo,
                   seL4_Word *mr0, seL4_Word *mr1)
{
    asm volatile (
        "pushl %%ebp       \n"
        "movl %%ecx, %%ebp \n"
        "movl %%esp, %%ecx \n"
        "leal 1f, %%edx    \n"
        "1:                \n"
        "sysenter          \n"
        "popl %%ebp        \n"
        :
        : "a" (seL4_SysNBSend),
        "b" (dest),
        "S" (msgInfo.words[0]),
        "D" (mr0 != seL4_Null ? *mr0 : 0),
        "c" (mr1 != seL4_Null ? *mr1 : 0)
        : "%edx"
    );
}

static inline void
seL4_Reply(seL4_MessageInfo_t msgInfo)
{
    asm volatile (
        "pushl %%ebp       \n"
        "movl %%ecx, %%ebp \n"
        "movl %%esp, %%ecx \n"
        "leal 1f, %%edx    \n"
        "1:                \n"
        "sysenter          \n"
        "popl %%ebp        \n"
        :
        : "a" (seL4_SysReply),
        "S" (msgInfo.words[0]),
        "D" (seL4_GetMR(0)),
        "c" (seL4_GetMR(1))
        : "%ebx", "%edx"
    );
}

static inline void
seL4_ReplyWithMRs(seL4_MessageInfo_t msgInfo,
                  seL4_Word *mr0, seL4_Word *mr1)
{
    asm volatile (
        "pushl %%ebp       \n"
        "movl %%ecx, %%ebp \n"
        "movl %%esp, %%ecx \n"
        "leal 1f, %%edx    \n"
        "1:                \n"
        "sysenter          \n"
        "popl %%ebp        \n"
        :
        : "a" (seL4_SysReply),
        "S" (msgInfo.words[0]),
        "D" (mr0 != seL4_Null ? *mr0 : 0),
        "c" (mr1 != seL4_Null ? *mr1 : 0)
        : "%ebx", "%edx"
    );
}

static inline void
seL4_Signal(seL4_CPtr dest)
{
    asm volatile (
        "pushl %%ebp       \n"
        "movl %%esp, %%ecx \n"
        "leal 1f, %%edx    \n"
        "1:                \n"
        "sysenter          \n"
        "popl %%ebp        \n"
        :
        : "a" (seL4_SysSend),
        "b" (dest),
        "S" (seL4_MessageInfo_new(0, 0, 0, 0).words[0])
        : "%ecx", "%edx"
    );
}

static inline seL4_MessageInfo_t
seL4_Recv(seL4_CPtr src, seL4_Word* sender)
{
    seL4_MessageInfo_t info;
    seL4_Word badge;
    seL4_Word mr0;
    seL4_Word mr1;

    asm volatile (
        "pushl %%ebp       \n"
        "movl %%esp, %%ecx \n"
        "leal 1f, %%edx    \n"
        "1:                \n"
        "sysenter          \n"
        "movl %%ebp, %%ecx \n"
        "popl %%ebp        \n"
        :
        "=b" (badge),
        "=S" (info.words[0]),
        "=D" (mr0),
        "=c" (mr1)
        : "a" (seL4_SysRecv),
        "b" (src)
        : "%edx", "memory"
    );

    seL4_SetMR(0, mr0);
    seL4_SetMR(1, mr1);

    if (sender) {
        *sender = badge;
    }

    return info;
}

static inline seL4_MessageInfo_t
seL4_RecvWithMRs(seL4_CPtr src, seL4_Word* sender,
                 seL4_Word *mr0, seL4_Word *mr1)
{
    seL4_MessageInfo_t info;
    seL4_Word badge;
    seL4_Word msg0 = 0;
    seL4_Word msg1 = 0;

    asm volatile (
        "pushl %%ebp       \n"
        "movl %%esp, %%ecx \n"
        "leal 1f, %%edx    \n"
        "1:                \n"
        "sysenter          \n"
        "movl %%ebp, %%ecx \n"
        "popl %%ebp        \n"
        :
        "=b" (badge),
        "=S" (info.words[0]),
        "=D" (msg0),
        "=c" (msg1)
        : "a" (seL4_SysRecv),
        "b" (src)
        : "%edx", "memory"
    );

    if (mr0 != seL4_Null) {
        *mr0 = msg0;
    }
    if (mr1 != seL4_Null) {
        *mr1 = msg1;
    }

    if (sender) {
        *sender = badge;
    }

    return info;
}

static inline seL4_MessageInfo_t
seL4_NBRecv(seL4_CPtr src, seL4_Word* sender)
{
    seL4_MessageInfo_t info;
    seL4_Word badge;
    seL4_Word mr0;
    seL4_Word mr1;

    asm volatile (
        "pushl %%ebp       \n"
        "movl %%esp, %%ecx \n"
        "leal 1f, %%edx    \n"
        "1:                \n"
        "sysenter          \n"
        "movl %%ebp, %%ecx \n"
        "popl %%ebp        \n"
        :
        "=b" (badge),
        "=S" (info.words[0]),
        "=D" (mr0),
        "=c" (mr1)
        : "a" (seL4_SysNBRecv),
        "b" (src)
        : "%edx", "memory"
    );

    seL4_SetMR(0, mr0);
    seL4_SetMR(1, mr1);

    if (sender) {
        *sender = badge;
    }

    return info;
}

static inline seL4_MessageInfo_t
seL4_Call(seL4_CPtr dest, seL4_MessageInfo_t msgInfo)
{
    seL4_MessageInfo_t info;
    seL4_Word mr0 = seL4_GetMR(0);
    seL4_Word mr1 = seL4_GetMR(1);

    asm volatile (
        "pushl %%ebp       \n"
        "movl %%ecx, %%ebp \n"
        "movl %%esp, %%ecx \n"
        "leal 1f, %%edx    \n"
        "1:                \n"
        "sysenter          \n"
        "movl %%ebp, %%ecx \n"
        "popl %%ebp        \n"
        :
        "=S" (info.words[0]),
        "=D" (mr0),
        "=c" (mr1),
        "=b" (dest) /* dummy, tells GCC that ebx is clobbered */
        : "a" (seL4_SysCall),
        "b" (dest),
        "S" (msgInfo.words[0]),
        "D" (mr0),
        "c" (mr1)
        : "%edx", "memory"
    );

    seL4_SetMR(0, mr0);
    seL4_SetMR(1, mr1);

    return info;
}

static inline seL4_MessageInfo_t
seL4_CallWithMRs(seL4_CPtr dest, seL4_MessageInfo_t msgInfo,
                 seL4_Word *mr0, seL4_Word *mr1)
{
    seL4_MessageInfo_t info;
    seL4_Word msg0 = 0;
    seL4_Word msg1 = 0;

    if (mr0 != seL4_Null && seL4_MessageInfo_get_length(msgInfo) > 0) {
        msg0 = *mr0;
    }
    if (mr1 != seL4_Null && seL4_MessageInfo_get_length(msgInfo) > 1) {
        msg1 = *mr1;
    }

    asm volatile (
        "pushl %%ebp       \n"
        "movl %%ecx, %%ebp \n"
        "movl %%esp, %%ecx \n"
        "leal 1f, %%edx    \n"
        "1:                \n"
        "sysenter          \n"
        "movl %%ebp, %%ecx \n"
        "popl %%ebp        \n"
        :
        "=S" (info.words[0]),
        "=D" (msg0),
        "=c" (msg1),
        "=b" (dest) /* dummy, tells GCC that ebx is clobbered */
        : "a" (seL4_SysCall),
        "b" (dest),
        "S" (msgInfo.words[0]),
        "D" (msg0),
        "c" (msg1)
        : "%edx", "memory"
    );

    if (mr0 != seL4_Null) {
        *mr0 = msg0;
    }
    if (mr1 != seL4_Null) {
        *mr1 = msg1;
    }

    return info;
}

static inline seL4_MessageInfo_t
seL4_ReplyRecv(seL4_CPtr dest, seL4_MessageInfo_t msgInfo, seL4_Word *sender)
{
    seL4_MessageInfo_t info;
    seL4_Word badge;
    seL4_Word mr0 = seL4_GetMR(0);
    seL4_Word mr1 = seL4_GetMR(1);

    asm volatile (
        "pushl %%ebp       \n"
        "movl %%ecx, %%ebp \n"
        "movl %%esp, %%ecx \n"
        "leal 1f, %%edx    \n"
        "1:                \n"
        "sysenter          \n"
        "movl %%ebp, %%ecx \n"
        "popl %%ebp        \n"
        :
        "=b" (badge),
        "=S" (info.words[0]),
        "=D" (mr0),
        "=c" (mr1)
        : "a" (seL4_SysReplyRecv),
        "b" (dest),
        "S" (msgInfo.words[0]),
        "D" (mr0),
        "c" (mr1)
        : "%edx", "memory"
    );

    seL4_SetMR(0, mr0);
    seL4_SetMR(1, mr1);

    if (sender) {
        *sender = badge;
    }

    return info;
}

static inline seL4_MessageInfo_t
seL4_ReplyRecvWithMRs(seL4_CPtr dest, seL4_MessageInfo_t msgInfo, seL4_Word *sender,
                      seL4_Word *mr0, seL4_Word *mr1)
{
    seL4_MessageInfo_t info;
    seL4_Word badge;
    seL4_Word msg0 = 0;
    seL4_Word msg1 = 0;

    if (mr0 != seL4_Null && seL4_MessageInfo_get_length(msgInfo) > 0) {
        msg0 = *mr0;
    }
    if (mr1 != seL4_Null && seL4_MessageInfo_get_length(msgInfo) > 1) {
        msg1 = *mr1;
    }

    asm volatile (
        "pushl %%ebp       \n"
        "movl %%ecx, %%ebp \n"
        "movl %%esp, %%ecx \n"
        "leal 1f, %%edx    \n"
        "1:                \n"
        "sysenter          \n"
        "movl %%ebp, %%ecx \n"
        "popl %%ebp        \n"
        :
        "=b" (badge),
        "=S" (info.words[0]),
        "=D" (msg0),
        "=c" (msg1)
        : "a" (seL4_SysReplyRecv),
        "b" (dest),
        "S" (msgInfo.words[0]),
        "D" (msg0),
        "c" (msg1)
        : "%edx", "memory"
    );

    if (mr0 != seL4_Null) {
        *mr0 = msg0;
    }
    if (mr1 != seL4_Null) {
        *mr1 = msg1;
    }

    if (sender) {
        *sender = badge;
    }

    return info;
}

static inline void
seL4_Yield(void)
{
    asm volatile (
        "pushl %%ebp       \n"
        "movl %%esp, %%ecx \n"
        "leal 1f, %%edx    \n"
        "1:                \n"
        "sysenter          \n"
        "popl %%ebp        \n"
        :
        : "a" (seL4_SysYield)
        : "%ebx", "%ecx", "%edx", "%esi", "%edi", "memory"
    );
}

static inline seL4_Word
seL4_VMEnter(seL4_CPtr vcpu, seL4_Word *sender)
{
    seL4_Word fault;
    seL4_Word badge;
    seL4_Word mr0 = seL4_GetMR(0);
    seL4_Word mr1 = seL4_GetMR(1);

    asm volatile (
        "pushl %%ebp       \n"
        "movl %%ecx, %%ebp \n"
        "movl %%esp, %%ecx \n"
        "leal 1f, %%edx    \n"
        "1:                \n"
        "sysenter          \n"
        "movl %%ebp, %%ecx \n"
        "popl %%ebp        \n"
        :
        "=b" (badge),
        "=S" (fault),
        "=D" (mr0),
        "=c" (mr1)
        : "a" (seL4_SysVMEnter),
        "b" (vcpu),
        "D" (mr0),
        "c" (mr1)
        : "%edx", "memory"
    );

    seL4_SetMR(0, mr0);
    seL4_SetMR(1, mr1);
    if (!fault && sender) {
        *sender = badge;
    }
    return fault;
}

#if defined(SEL4_DEBUG_KERNEL)
static inline void
seL4_DebugPutChar(char c)
{
    asm volatile (
        "pushl %%ebp       \n"
        "movl %%esp, %%ecx \n"
        "leal 1f, %%edx    \n"
        "1:                \n"
        "sysenter          \n"
        "popl %%ebp        \n"
        :
        : "a" (seL4_SysDebugPutChar),
        "b" (c)
        : "%ecx", "%edx", "%esi", "%edi", "memory"
    );
}
#endif

#ifdef SEL4_DEBUG_KERNEL
static inline void
seL4_DebugHalt(void)
{
    asm volatile (
        "pushl %%ebp       \n"
        "movl %%esp, %%ecx \n"
        "leal 1f, %%edx    \n"
        "1:                \n"
        "sysenter          \n"
        "popl %%ebp        \n"
        :
        : "a" (seL4_SysDebugHalt)
        : "%ebx", "%ecx", "%edx", "%esi", "%edi", "memory"
    );
}
#endif

#if defined(SEL4_DEBUG_KERNEL)
static inline void
seL4_DebugSnapshot(void)
{
    asm volatile (
        "pushl %%ebp       \n"
        "movl %%esp, %%ecx \n"
        "leal 1f, %%edx    \n"
        "1:                \n"
        "sysenter          \n"
        "popl %%ebp        \n"
        :
        : "a" (seL4_SysDebugSnapshot)
        : "%ebx", "%ecx", "%edx", "%esi", "%edi", "memory"
    );
}
#endif

#ifdef SEL4_DEBUG_KERNEL
static inline seL4_Uint32
seL4_DebugCapIdentify(seL4_CPtr cap)
{
    asm volatile (
        "pushl %%ebp       \n"
        "movl %%esp, %%ecx \n"
        "leal 1f, %%edx    \n"
        "1:                \n"
        "sysenter          \n"
        "popl %%ebp        \n"
        : "=b"(cap)
        : "a"(seL4_SysDebugCapIdentify), "b"(cap)
        : "%ecx", "%edx", "%esi", "%edi", "memory"
    );
    return (seL4_Uint32)cap;
}
#endif

#ifdef CONFIG_PRINTING
char *strcpy(char *, const char *);
static inline void
seL4_DebugNameThread(seL4_CPtr tcb, const char *name)
{
    strcpy((char*)seL4_GetIPCBuffer()->msg, name);

    asm volatile (
        "pushl %%ebp       \n"
        "movl %%esp, %%ecx \n"
        "leal 1f, %%edx    \n"
        "1:                \n"
        "sysenter          \n"
        "popl %%ebp        \n"
        :
        : "a"(seL4_SysDebugNameThread), "b"(tcb)
        : "%ecx", "%edx", "%esi", "%edi", "memory"
    );
}
#endif

#if defined(SEL4_DANGEROUS_CODE_INJECTION_KERNEL)
static inline void
seL4_DebugRun(void (*userfn) (void *), void* userarg)
{
    asm volatile (
        "pushl %%ebp       \n"
        "movl %%esp, %%ecx \n"
        "leal 1f, %%edx    \n"
        "1:                \n"
        "sysenter          \n"
        "popl %%ebp        \n"
        :
        : "a" (seL4_SysDebugRun),
        "b" (userfn),
        "S" (userarg)
        : "%ecx", "%edx", "%edi", "memory"
    );
}
#endif

#ifdef CONFIG_ENABLE_BENCHMARKS
static inline void
seL4_BenchmarkResetLog(void)
{
    asm volatile (
        "pushl %%ebp        \n"
        "movl %%esp, %%ecx  \n"
        "leal 1f, %%edx     \n"
        "1:                 \n"
        "sysenter           \n"
        "popl %%ebp         \n"
        :
        : "a" (seL4_SysBenchmarkResetLog)
        : "%ecx", "%edx", "%edi", "memory"
    );
}

static inline seL4_Uint32
seL4_BenchmarkDumpLog(seL4_Word start, seL4_Word size)
{
    asm volatile (
        "pushl %%ebp        \n"
        "movl %%esp, %%ecx  \n"
        "leal 1f, %%edx     \n"
        "1:                 \n"
        "sysenter           \n"
        "popl %%ebp         \n"
        : "=b" (start)
        : "a" (seL4_SysBenchmarkDumpLog),
        "b" (start),
        "S" (size)
        : "%ecx", "%edx", "%edi", "memory"
    );

    return (seL4_Uint32) start;
}


static inline seL4_Uint32
seL4_BenchmarkLogSize(void)
{
    seL4_Uint32 ret = 0;
    asm volatile (
        "pushl %%ebp        \n"
        "movl %%esp, %%ecx  \n"
        "leal 1f, %%edx     \n"
        "1:                 \n"
        "sysenter           \n"
        "popl %%ebp         \n"
        : "=b" (ret)
        : "a" (seL4_SysBenchmarkLogSize)
        : "%ecx", "%edx", "%edi", "memory"
    );

    return ret;
}

static inline void
seL4_BenchmarkFinalizeLog(void)
{
    asm volatile (
        "pushl %%ebp        \n"
        "movl %%esp, %%ecx  \n"
        "leal 1f, %%edx     \n"
        "1:                 \n"
        "sysenter           \n"
        "popl %%ebp         \n"
        :
        : "a" (seL4_SysBenchmarkFinalizeLog)
        : "%ecx", "%edx", "%edi", "memory"
    );
}

#ifdef CONFIG_BENCHMARK_TRACK_UTILISATION
static inline void
seL4_BenchmarkGetThreadUtilisation(seL4_Word tcp_cptr)
{
    asm volatile (
        "pushl %%ebp        \n"
        "movl %%esp, %%ecx  \n"
        "leal 1f, %%edx     \n"
        "1:                 \n"
        "sysenter           \n"
        "popl %%ebp         \n"
        :
        : "a" (seL4_SysBenchmarkGetThreadUtilisation),
        "b"(tcp_cptr)
        : "%ecx", "%edx", "%edi", "memory"
    );
}
#endif /* CONFIG_BENCHMARK_TRACK_UTILISATION */
#endif /* CONFIG_ENABLE_BENCHMARKS */
#endif
