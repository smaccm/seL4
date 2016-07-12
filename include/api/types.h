/*
 * Copyright 2014, General Dynamics C4 Systems
 *
 * This software may be distributed and modified according to the terms of
 * the GNU General Public License version 2. Note that NO WARRANTY is provided.
 * See "LICENSE_GPLv2.txt" for details.
 *
 * @TAG(GD_GPL)
 */

#ifndef __API_TYPES_H
#define __API_TYPES_H

#include <config.h>
#include <stdint.h>
#include <util.h>
#include <api/types_gen.h>
#include <arch/api/types.h>
#include <arch/types.h>
#include <api/constants.h>
#include <api/shared_types.h>

/* cap_rights_t defined in api/types.bf */
typedef word_t prio_t;
typedef uint64_t time_t;
typedef uint64_t ticks_t;
typedef word_t crit_t;

struct cap_transfer {
    cptr_t ctReceiveRoot;
    cptr_t ctReceiveIndex;
    word_t ctReceiveDepth;
};
typedef struct cap_transfer cap_transfer_t;

enum ctLimits {
    capTransferDataSize = 3
};

static inline cap_rights_t CONST
rightsFromWord(word_t w)
{
    cap_rights_t cap_rights;

    cap_rights.words[0] = w;
    return cap_rights;
}

static inline word_t CONST
wordFromRights(cap_rights_t cap_rights)
{
    return cap_rights.words[0] & MASK(3);
}

static inline cap_transfer_t PURE
capTransferFromWords(word_t *wptr)
{
    cap_transfer_t transfer;

    transfer.ctReceiveRoot  = (cptr_t)wptr[0];
    transfer.ctReceiveIndex = (cptr_t)wptr[1];
    transfer.ctReceiveDepth = wptr[2];
    return transfer;
}

static inline seL4_MessageInfo_t CONST
messageInfoFromWord_raw(word_t w)
{
    seL4_MessageInfo_t mi;

    mi.words[0] = w;
    return mi;
}

static inline seL4_MessageInfo_t CONST
messageInfoFromWord(word_t w)
{
    seL4_MessageInfo_t mi;
    word_t len;

    mi.words[0] = w;

    len = seL4_MessageInfo_get_length(mi);
    if (len > seL4_MsgMaxLength) {
        mi = seL4_MessageInfo_set_length(mi, seL4_MsgMaxLength);
    }

    return mi;
}

static inline word_t CONST
wordFromMessageInfo(seL4_MessageInfo_t mi)
{
    return mi.words[0];
}

#define allRights cap_rights_new(true, true, true)
#define noWrite cap_rights_new(true, true, false)

#ifdef CONFIG_PRINTING
#ifdef CONFIG_COLOUR_PRINTING
#define ANSI_RESET "\033[0m"
#define ANSI_GREEN ANSI_RESET "\033[32m"
#define ANSI_DARK  ANSI_RESET "\033[30;1m"
#else
#define ANSI_RESET ""
#define ANSI_GREEN ANSI_RESET ""
#define ANSI_DARK  ANSI_RESET ""
#endif
/*
 * Print to serial a message helping userspace programmers to determine why the
 * kernel is not performing their requested operation.
 */
#define userError(...) \
    do {                                                                     \
        printf(ANSI_DARK "<<" ANSI_GREEN "seL4" ANSI_DARK                    \
                " [%s/%d T%p \"%s\" @%lx]: ",                                \
                __func__, __LINE__, ksCurThread, ksCurThread->tcbName,       \
                (word_t)getRestartPC(ksCurThread));                          \
        printf(__VA_ARGS__);                                                 \
        printf(">>" ANSI_RESET "\n");                                        \
    } while (0)
#else /* !CONFIG_PRINTING */
#define userError(...)
#endif

#endif
