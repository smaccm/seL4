%
% Copyright 2014, General Dynamics C4 Systems
%
% This software may be distributed and modified according to the terms of
% the GNU General Public License version 2. Note that NO WARRANTY is provided.
% See "LICENSE_GPLv2.txt" for details.
%
% @TAG(GD_GPL)
%

This module defines the machine-specific invocations for the ARM.

\begin{impdetails}

This module makes use of the GHC extension allowing data types with no constructors.

> {-# LANGUAGE EmptyDataDecls #-}

\end{impdetails}

> module SEL4.API.InvocationLabels.ARM where

\subsection{ARM-Specific Invocation Labels}

> data InvocationLabel
>         = ARMPDClean_Data
>         | ARMPDInvalidate_Data
>         | ARMPDCleanInvalidate_Data
>         | ARMPDUnify_Instruction
>         | ARMPageTableMap
>         | ARMPageTableUnmap
>         | ARMPageMap
>         | ARMPageRemap
>         | ARMPageUnmap
>         | ARMPageClean_Data
>         | ARMPageInvalidate_Data
>         | ARMPageCleanInvalidate_Data
>         | ARMPageUnify_Instruction
>         | ARMPageGetAddress
>         | ARMASIDControlMakePool
>         | ARMASIDPoolAssign
>         deriving (Eq, Enum, Bounded)

> isPDFlush :: InvocationLabel -> Bool
> isPDFlush x = case x of
>       ARMPDClean_Data -> True
>       ARMPDInvalidate_Data -> True
>       ARMPDCleanInvalidate_Data -> True
>       ARMPDUnify_Instruction -> True
>       _ -> False

