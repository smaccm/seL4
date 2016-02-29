/*
 * Copyright 2014, General Dynamics C4 Systems
 *
 * This software may be distributed and modified according to the terms of
 * the GNU General Public License version 2. Note that NO WARRANTY is provided.
 * See "LICENSE_GPLv2.txt" for details.
 *
 * @TAG(GD_GPL)
 */

#ifndef __OBJECT_UNTYPED_H
#define __OBJECT_UNTYPED_H

#include <config.h>
#include <types.h>
#include <api/failures.h>
#include <api/types.h>
#include <object/structures.h>
#include <object/cnode.h>

/* It is assumed that every untyped is within MIN_SIZE_BITS and MAX_SIZE_BITS
 * (inclusive). This means that every untyped stored as MIN_SIZE_BITS
 * subtracted from its size before it is stored in capBlockSize, and
 * capFreeIndex counts in chunks of size 2^MIN_SIZE_BITS. The MAX_SIZE_BITS
 * is the minimal untyped that can be stored when considering both how
 * many bits of capBlockSize there are, and the largest offset that can
 * be stored in capFreeIndex */
#define MIN_SIZE_BITS 4
#ifdef CONFIG_ARCH_X86_64
#define MAX_SIZE_BITS 47
#else
#define MAX_SIZE_BITS 30
#endif
#define MAX_FREE_INDEX(sizeBits) (BIT((sizeBits) - MIN_SIZE_BITS))
#define FREE_INDEX_TO_OFFSET(freeIndex) ((freeIndex)<<MIN_SIZE_BITS)
#define GET_FREE_REF(base,freeIndex) ((word_t)(((word_t)(base)) + FREE_INDEX_TO_OFFSET(freeIndex)))
#define GET_FREE_INDEX(base,free) (((word_t)(free) - (word_t)(base))>>MIN_SIZE_BITS)

exception_t decodeUntypedInvocation(word_t invLabel, word_t length,
                                    cte_t *slot, cap_t cap,
                                    extra_caps_t excaps, bool_t call,
                                    word_t *buffer);
exception_t invokeUntyped_Retype(cte_t *srcSlot, void* base_ign,
                                 void* freeRegionBase, object_t newType,
                                 word_t userSize, slot_range_t destSlots,
                                 bool_t call, bool_t deviceMemory);

#endif
