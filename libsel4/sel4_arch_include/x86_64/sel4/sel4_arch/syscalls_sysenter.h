/*
 * Copyright 2014, NICTA
 *
 * This software may be distributed and modified according to the terms of
 * the BSD 2-Clause license. Note that NO WARRANTY is provided.
 * See "LICENSE_BSD2.txt" for details.
 *
 * @TAG(NICTA_BSD)
 */

#ifndef __LIBSEL4_SEL4_ARCH_SYSCALLS_SYSENTER_H
#define __LIBSEL4_SEL4_ARCH_SYSCALLS_SYSENTER_H

#include <autoconf.h>
#include <sel4/arch/functions.h>
#include <sel4/types.h>


static inline void
seL4_Send(seL4_CPtr dest, seL4_MessageInfo_t msgInfo)
{
    register seL4_Word mr0 asm("r10")= seL4_GetMR(0);
    register seL4_Word mr1 asm("r8")= seL4_GetMR(1);

    asm volatile (
            "movq   %%rsp, %%rcx        \n"
            "leaq   1f, %%rdx           \n"
            "1:                         \n"
            "sysenter                   \n"
            :
            : "a" ((seL4_Word)seL4_SysSend),
              "D" (dest),
              "S" (msgInfo.words[0]),
              "r" (mr0),
              "r" (mr1)
            : "%rcx", "%rdx"
            );
}

static inline void
seL4_SendWithMRs(seL4_CPtr dest, seL4_MessageInfo_t msgInfo,
                 seL4_Word *mr0, seL4_Word *mr1)
{
    seL4_Assert(!"not implemented");
    asm volatile (
            "push   %%rbp           \n"
            "movq   %%rcx, %rbp     \n"
            "movq   %%rsp, %%rcx    \n"
            "leaq   1f, %%rdx       \n"
            "1:                     \n"
            "sysenter               \n"
            "pop    %%rbp          \n"
            :
            : "a" ((seL4_Word)seL4_SysSend),
              "b" (dest),
              "S" (msgInfo.words[0]),
              "D" ((mr0 != 0)? *mr0 : 0),
              "c" ((mr1 != 0)? *mr1 : 0)
            : "%rdx"
            ); 
}

static inline void
seL4_NBSend(seL4_CPtr dest, seL4_MessageInfo_t msgInfo)
{
    register seL4_Word mr0 asm("r10")= seL4_GetMR(0);
    register seL4_Word mr1 asm("r8")= seL4_GetMR(1);

    asm volatile (
            "movq   %%rsp, %%rcx        \n"
            "leaq   1f, %%rdx           \n"
            "1:                         \n"
            "sysenter                   \n"
            :
            : "a" ((seL4_Word)seL4_SysNBSend),
              "D" (dest),
              "S" (msgInfo.words[0]),
              "r" (mr0),
              "r" (mr1)
            : "%rdx", "%rcx"
            );
}

static inline void
seL4_NBSendWithMRs(seL4_CPtr dest, seL4_MessageInfo_t msgInfo,
                   seL4_Word *mr0, seL4_Word *mr1)
{
    seL4_Assert(!"not implemented");
    asm volatile (
            "push   %%rbp           \n"
            "movq   %%rcx, %rbp     \n"
            "movq   %%rsp, %%rcx    \n"
            "leaq   1f, %%rdx       \n"
            "1:                     \n"
            "sysenter               \n"
            "pop    %%rbp           \n"
            :
            : "a" ((seL4_Word)seL4_SysNBSend),
              "b" (dest),
              "S" (msgInfo.words[0]),
              "D" ((mr0 != 0)? *mr0 : 0),
              "c" ((mr1 != 0)? *mr1 : 0)
            : "%rdx"
            );
}

static inline void
seL4_Reply(seL4_MessageInfo_t msgInfo)
{
    register seL4_Word mr0 asm("r10") = seL4_GetMR(0);
    register seL4_Word mr1 asm("r8") = seL4_GetMR(1);

    asm volatile (
            "movq   %%rsp, %%rcx        \n"
            "leaq   1f, %%rdx           \n"
            "1:                         \n"
            "sysenter                   \n"
            :
            : "a" ((seL4_Word)seL4_SysReply),
              "S" (msgInfo.words[0]),
              "r" (mr0),
              "r" (mr1)
            : "%rdx", "%rcx"
            );
}

static inline void
seL4_ReplyWithMRs(seL4_MessageInfo_t msgInfo,
                  seL4_Word *mr0, seL4_Word *mr1)
{
    seL4_Assert(!"not implemented");
    asm volatile (
            "push   %%rbp           \n"
            "movq   %%rcx, %rbp     \n"
            "movq   %%rsp, %%rcx    \n"
            "leaq   1f, %%rdx       \n"
            "1:                     \n"
            "sysenter               \n"
            "pop    %%rbp           \n"
            :
            : "a" ((seL4_Word)seL4_SysReply),
              "S" (msgInfo.words[0]),
              "D" ((mr0 != 0)? *mr0 : 0),
              "c" ((mr1 != 0)? *mr1 : 0)
            : "%rdx"
            );
}

static inline void
seL4_Signal(seL4_CPtr dest)
{
    asm volatile (
            "movq   %%rsp, %%rcx        \n"
            "leaq   1f, %%rdx           \n"
            "1:                         \n"
            "sysenter                   \n"
            :
            : "a" ((seL4_Word)seL4_SysSend),
              "D" (dest),
              "S" (seL4_MessageInfo_new(0, 0, 0, 1).words[0])
            : "%rcx", "%rdx"
            );
}

static inline seL4_MessageInfo_t
seL4_Recv(seL4_CPtr src, seL4_Word* sender)
{
    seL4_MessageInfo_t info;
    seL4_Word badge = 0;
    register seL4_Word mr0 asm("r10") = 0;
    register seL4_Word mr1 asm("r8") = 0;

    info.words[0] = 0;

    asm volatile (
            "movq   %%rsp, %%rcx    \n"
            "leaq   1f, %%rdx       \n"
            "1:                     \n"
            "sysenter               \n"
            :
             "=D" (badge),
             "+S" (info.words[0]),
             "=r" (mr0),
             "=r" (mr1)
            :
             "a" ((seL4_Word)seL4_SysRecv),
             "D" (src)
            : "%rcx", "%rdx", "memory"
            );

    seL4_SetMR(0, mr0);
    seL4_SetMR(1, mr1);

    if (sender)
        *sender = badge;

    return info;
}

static inline seL4_MessageInfo_t
seL4_RecvWithMRs(seL4_CPtr src, seL4_Word* sender,
                 seL4_Word *mr0, seL4_Word *mr1)
{
    seL4_MessageInfo_t info;
    seL4_Word badge;
    seL4_Word _mr0;
    seL4_Word _mr1;

    info.words[0] = 0;

    seL4_Assert(!"not implemented");
    asm volatile (
            "push   %%rbp           \n"
            "movq   %%rsp, %%rcx    \n"
            "leaq   1f, %%rdx       \n"
            "1:                     \n"
            "sysenter               \n"
            "movq   %%rbp, %%rcx    \n"
            "pop    %%rbp           \n"
            :
             "=b" (badge),
             "+S" (info.words[0]),
             "=D" (_mr0),
             "=c" (_mr1)
            :
             "a" ((seL4_Word)seL4_SysRecv),
             "b" (src)
            : "%rdx", "memory"
            );

    if (mr0 != 0) { *mr0 = _mr0; }
    if (mr1 != 0) { *mr1 = _mr1; }

    if (sender)
        *sender = badge;
    return info;
}

static inline seL4_MessageInfo_t
seL4_NBRecv(seL4_CPtr src, seL4_Word* sender)
{
    seL4_MessageInfo_t info;
    seL4_Word badge;
    register seL4_Word mr0 asm("r10");
    register seL4_Word mr1 asm("r8");

    asm volatile (
        "movq %%rsp, %%rcx \n"
        "leaq 1f, %%rdx    \n"
        "1:                \n"
        "sysenter          \n"
        :
        "=D" (badge),
        "=S" (info.words[0]),
        "=r" (mr0),
        "=r" (mr1)
        : "a" ((seL4_Word)seL4_SysNBRecv),
        "D" (src)
        : "%rcx", "%rdx", "memory"
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
    info.words[0] = 0;
    register seL4_Word mr0 asm("r10")= seL4_GetMR(0);
    register seL4_Word mr1 asm("r8")= seL4_GetMR(1);

    asm volatile (
            "movq   %%rsp, %%rcx    \n"
            "leaq   1f, %%rdx       \n"
            "1:                     \n"
            "sysenter               \n"
            :
                "=S" (info.words[0]),
                "=r" (mr0),
                "=r" (mr1),
                "=D" (dest)
            :
                "a" ((seL4_Word)seL4_SysCall),
                "D" (dest),
                "S" (msgInfo.words[0]),
                "r" (mr0),
                "r" (mr1)
            :
                "%rcx", "%rdx", "memory"
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
    info.words[0] = 0;
    seL4_Word _r0 = (mr0 != 0? *mr0 : 0);
    seL4_Word _r1 = (mr1 != 0? *mr1 : 0);

    seL4_Assert(!"not implemented");
    asm volatile (
            "push   %%rbp           \n"
            "movq   %%rcx, %%rbp    \n"
            "movq   %%rsp, %%rcx    \n"
            "leaq   1f, %%rdx       \n"
            "1:                     \n"
            "sysenter               \n"
            "movq   %%rbp, %%rcx    \n"
            "pop    %%rbp           \n"
            :
                "=S" (info.words[0]),
                "=D" (_r0),
                "=c" (_r1),
                "=b" (dest)
            :
                "a" ((seL4_Word)seL4_SysCall),
                "b" (dest),
                "S" (msgInfo.words[0]),
                "D" (_r0), 
                "c" (_r1)
            :
                "%rdx", "memory"
                );

    if (mr0 != 0) { *mr0 = _r0; }
    if (mr1 != 0) { *mr1 = _r1; }

    return info;
}

static inline seL4_MessageInfo_t
seL4_ReplyRecv(seL4_CPtr dest, seL4_MessageInfo_t msgInfo, seL4_Word *sender)
{
    seL4_MessageInfo_t info;
    seL4_Word badge;
    register seL4_Word mr0 asm("r10") = seL4_GetMR(0);
    register seL4_Word mr1 asm("r8")= seL4_GetMR(1);

    asm volatile (
            "movq   %%rsp, %%rcx    \n"
            "leaq   1f, %%rdx       \n"
            "1:                     \n"
            "sysenter               \n"
            :
                "=D" (badge),
                "=S" (info.words[0]),
                "=r" (mr0),
                "=r" (mr1)
            :
                "a" ((seL4_Word)seL4_SysReplyRecv),
                "D" (dest),
                "S" (msgInfo.words[0]),
                "r" (mr0),
                "r" (mr1)
            :
                "%rcx", "%rdx", "memory"
                  );

    seL4_SetMR(0, mr0);
    seL4_SetMR(1, mr1);

    if (sender)
        *sender = badge;

    return info;
}

static inline seL4_MessageInfo_t
seL4_ReplyRecvWithMRs(seL4_CPtr dest, seL4_MessageInfo_t msgInfo, seL4_Word *sender,
                      seL4_Word *mr0, seL4_Word *mr1)
{
    seL4_MessageInfo_t info;
    seL4_Word badge;
    seL4_Word _mr0 = (mr0 != 0? *mr0 : 0);
    seL4_Word _mr1 = (mr1 != 0? *mr1 : 0);

    seL4_Assert(!"not implemented");
    asm volatile (
            "push   %%rbp           \n"
            "movq   %%rcx, %%rbp    \n"
            "movq   %%rsp, %%rcx    \n"
            "leaq   1f, %%rdx       \n"
            "1:                     \n"
            "sysenter               \n"
            "movq   %%rbp, %%rcx    \n"
            "pop    %%rbp           \n"
            : 
                "=b" (badge),
                "=S" (info.words[0]),
                "=D" (_mr0),
                "=c" (_mr1)
            :
                "a"  ((seL4_Word)seL4_SysReplyRecv),
                "b"  (dest),
                "S"  (msgInfo.words[0]),
                "D"  (_mr0),
                "c"  (_mr1)
            : "%rdx", "memory"
                );


    if (mr0 != 0) { *mr0 = _mr0; }
    if (mr1 != 0) { *mr1 = _mr1; }

    if (sender) { *sender = badge; }

    return info;
}

static inline void
seL4_Yield(void)
{

    asm volatile (
            "movq   %%rsp, %%rcx    \n"
            "leaq   1f, %%rdx       \n"
            "1:                     \n"
            "sysenter               \n"
            :
            : "a" ((seL4_Word)seL4_SysYield)
            : "%rbx", "%rcx", "%rdx", "%rsi", "%rdi", "memory"
            );
}

#if defined(SEL4_DEBUG_KERNEL)
static inline void
seL4_DebugPutChar(char c)
{
    asm volatile (
            "movq   %%rsp, %%rcx    \n"
            "leaq   1f, %%rdx       \n"
            "1:                     \n"
            "sysenter               \n"
            :
            : "a" ((seL4_Word)seL4_SysDebugPutChar),
              "D" (c)
            : "%rcx", "%rdx", "memory"
            );
}
#endif

#ifdef SEL4_DEBUG_KERNEL
static inline void
seL4_DebugHalt(void)
{
    asm volatile (
            "movq   %%rsp, %%rcx    \n"
            "leaq   1f, %%rdx       \n"
            "1:                     \n"
            "sysenter               \n"
            :
            : "a" ((seL4_Word)seL4_SysDebugHalt)
            : "%rcx", "%rdx"
            );
}
#endif

#if defined(SEL4_DEBUG_KERNEL)
static inline void
seL4_DebugSnapshot(void)
{
    asm volatile (
            "movq   %%rsp, %%rcx    \n"
            "leaq   1f, %%rdx       \n"
            "1:                     \n"
            "sysenter               \n"
            :
            : "a" ((seL4_Word)seL4_SysDebugSnapshot)
            : "%rcx", "%rdx"
            );
}
#endif

#ifdef SEL4_DEBUG_KERNEL
static inline seL4_Uint32
seL4_DebugCapIdentify(seL4_CPtr cap)
{
    asm volatile (
            "movq   %%rsp, %%rcx    \n"
            "leaq   1f, %%rdx       \n"
            "1:                     \n"
            "sysenter               \n"
            : "=D" (cap)
            : "a" ((seL4_Word)seL4_SysDebugCapIdentify),
              "D" (cap)
            : "%rcx", "%rdx"
            );

    return (seL4_Uint32)cap;
}
#endif

#ifdef SEL4_DEBUG_KERNEL
char *strcpy(char *, const char *);
static inline void
seL4_DebugNameThread(seL4_CPtr tcb, const char *name)
{

    strcpy((char*)seL4_GetIPCBuffer()->msg, name);

    asm volatile (
        "movq %%rsp, %%rcx \n"
        "leaq 1f, %%rdx    \n"
        "1:                \n"
        "sysenter          \n"
        :
        : "a"((seL4_Word)seL4_SysDebugNameThread), "D"(tcb)
        : "%rcx", "%rdx",  "memory"
    );
}
#endif

#if defined(SEL4_DANGEROUS_CODE_INJECTION_KERNEL)
static inline void
seL4_DebugRun(void (*userfn) (void *), void* userarg)
{
    asm volatile (
            "movq   %%rsp, %%rcx    \n"
            "leaq   1f, %%rdx       \n"
            "1:                     \n"
            "sysenter               \n"
            :
            : "a" ((seL4_Word)seL4_SysDebugRun),
              "D" (userfn),
              "S" (userarg)
            : "%rcx", "%rdx", "memory"
            );
}
#endif

#if CONFIG_MAX_NUM_TRACE_POINTS > 0
static inline void
seL4_BenchmarkResetLog(void)
{
    asm volatile (
        "movq %%rsp, %%rcx  \n"
        "leaq 1f, %%rdx     \n"
        "1:                 \n"
        "sysenter           \n"
        :
        : "a" ((seL4_Word)seL4_SysBenchmarkResetLog)
        : "%rcx", "%rdx", "memory"
    );
}

static inline seL4_Word 
seL4_BenchmarkDumpLog(seL4_Word start, seL4_Word size)
{

    asm volatile (
        "movq %%rsp, %%rcx  \n"
        "leaq 1f, %%rdx     \n"
        "1:                 \n"
        "sysenter           \n"
        : "=D" (start)
        : "a" ((seL4_Word)seL4_SysBenchmarkDumpLog),
        "D" (start),
        "S" (size)
        : "%rcx", "%rdx", "memory"
    );

    return (seL4_Word) start;
}


static inline seL4_Uint32
seL4_BenchmarkLogSize(void)
{

    seL4_Uint32 ret = 0;
    seL4_Assert(!"not implemented");
    asm volatile (
        "movq %%rsp, %%rcx  \n"
        "leaq 1f, %%rdx     \n"
        "1:                 \n"
        "sysenter           \n"
        : "=D" (ret)
        : "a" ((seL4_Word)seL4_SysBenchmarkLogSize)
        : "%rcx", "%rdx", "memory"
    );

    return ret;
}

#endif /* CONFIG_MAX_NUM_TRACE_POINTS > 0 */
#endif
