/*
 * Copyright 2014, General Dynamics C4 Systems
 *
 * This software may be distributed and modified according to the terms of
 * the GNU General Public License version 2. Note that NO WARRANTY is provided.
 * See "LICENSE_GPLv2.txt" for details.
 *
 * @TAG(GD_GPL)
 */

#ifndef __UTIL_H
#define __UTIL_H

#define BIT(n) (1ul << (n))
#define MASK(n) (BIT(n)-1ul)
#define IS_ALIGNED(n, b) (!((n) & MASK(b)))
#define ROUND_DOWN(n, b) (((n) >> (b)) << (b))
#define ROUND_UP(n, b) (((((n) - 1ul) >> (b)) + 1ul) << (b))
#define ARRAY_SIZE(x) (sizeof(x) / sizeof(x[0]))
#define MIN(a,b) (((a)<(b))?(a):(b))

#ifndef __ASSEMBLER__

#define NULL ((void *)0)

#define PACKED       __attribute__((packed))
#define NORETURN     __attribute__((__noreturn__))
#define CONST        __attribute__((__const__))
#define PURE         __attribute__((__pure__))
#define ALIGN(n)     __attribute__((__aligned__(n)))
#define FASTCALL     __attribute__((fastcall))
#ifdef __clang__
#define VISIBLE      __attribute__((visibility("default")))
#else
#define VISIBLE      __attribute__((externally_visible))
#endif
#define FORCE_INLINE __attribute__((always_inline))
#define SECTION(sec) __attribute__((__section__(sec)))
#define UNUSED       __attribute__((unused))
#define USED         __attribute__((used))
#define FASTCALL     __attribute__((fastcall))

#define OFFSETOF(type, member) \
    __builtin_offsetof(type, member)

#ifdef __GNUC__
/* Borrowed from linux/include/linux/compiler.h */
#define likely(x)   __builtin_expect(!!(x), 1)
#define unlikely(x) __builtin_expect(!!(x), 0)
#else
#define likely(x)   (!!(x))
#define unlikely(x) (!!(x))
#endif

/* need that for compiling with c99 instead of gnu99 */
#define asm __asm__

/* Evaluate a Kconfig-provided configuration setting at compile-time. */
#define config_set(macro) _is_set_(macro)
#define _macrotest_1 ,
#define _is_set_(value) _is_set__(_macrotest_##value)
#define _is_set__(comma) _is_set___(comma 1, 0)
#define _is_set___(_, v, ...) v

/* Evalulate the value of a configuration setting, return a default
 * if it isn't defined */
#define config_default(macro, default) _config_default(macro, default)
#define _config_default(value, default) _config_default_(_macrotest_##value, value, default)
#define _config_default_(comma, value, default) _config_default__(comma value, default)
#define _config_default__(_, v, ...) v

/** MODIFIES:
    FNSPEC
        halt_spec: "\<Gamma> \<turnstile> {} Call halt_'proc {}"
*/
void halt(void) NORETURN;
void memzero(void *s, unsigned long n);
void *memset(void *s, unsigned long c, unsigned long n);
void *memcpy(void* ptr_dst, const void* ptr_src, unsigned long n);
int strncmp(const char *s1, const char *s2, int n);
long CONST char_to_long(char c);
long PURE str_to_long(const char* str);

#endif /* !__ASSEMBLER__ */

/** MODIFIES: */
/** DONT_TRANSLATE */
/** FNSPEC clzl_spec:
  "\<forall>s. \<Gamma> \<turnstile>
    {\<sigma>. s = \<sigma> \<and> x_' s \<noteq> 0 }
      \<acute>ret__long :== PROC clzl(\<acute>x)
    \<lbrace> \<acute>ret__long = of_nat (word_clz (x_' s)) \<rbrace>"
*/
static inline long
CONST clzl(unsigned long x)
{
    return __builtin_clzl(x);
}

#define CTZL(x) __builtin_ctzl(x)

#endif /* __UTIL_H */
