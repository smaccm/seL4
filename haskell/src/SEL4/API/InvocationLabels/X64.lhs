%
% Copyright 2014, General Dynamics C4 Systems
%
% This software may be distributed and modified according to the terms of
% the GNU General Public License version 2. Note that NO WARRANTY is provided.
% See "LICENSE_GPLv2.txt" for details.
%
% @TAG(GD_GPL)
%

This module defines the machine-specific invocations for x86 64bit.

\begin{impdetails}

This module makes use of the GHC extension allowing data types with no constructors.

> {-# LANGUAGE EmptyDataDecls #-}

\end{impdetails}

> module SEL4.API.InvocationLabels.X64 where

\subsection{x86-64-Specific Invocation Labels}

FIXME The kernel team is in the process of renaming these. In the XML spec, they are a total confusion between IA32 and X86, even for 64-bit only features! We have renamed them to X64 out of frustration, but some may genuinely be 32-bit only.

> data InvocationLabel
>         = X64PML4Map
>         | X64PML4Unmap
>         | X64PDPTMap
>         | X64PDPTUnmap
>         | X64PageDirectoryMap
>         | X64PageDirectoryUnmap
>         | X64PageTableMap
>         | X64PageTableUnmap
>         | X64IOPageTableMap
>         | X64IOPageTableUnmap
>         | X64PageMap
>         | X64PageRemap
>         | X64PageUnmap
>         | X64PageMapIO
>         | X64PageGetAddress
>         | X64ASIDControlMakePool
>         | X64ASIDPoolAssign
>         | X64IOPortIn8
>         | X64IOPortIn16
>         | X64IOPortIn32
>         | X64IOPortOut8
>         | X64IOPortOut16
>         | X64IOPortOut32
>         | X64IOSpaceRemovePassthrough
>         | X64IOSpaceUnmap
>         | X64IRQIssueIRQHandlerIOAPIC
>         | X64IRQIssueIRQHandlerMSI
>         deriving (Eq, Enum, Bounded)

