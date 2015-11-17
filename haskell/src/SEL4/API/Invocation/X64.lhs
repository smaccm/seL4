FIXME mostly clagged from ARM
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

> module SEL4.API.Invocation.X64 where

\begin{impdetails}

> import SEL4.Machine
> import SEL4.Machine.Hardware.X64 hiding (PAddr)
> import SEL4.Object.Structures

\end{impdetails}

\subsection{ARM-Specific Objects}

There are five ARM-specific object types; however, only four of them may be invoked. These are the page table, page, ASID control, and ASID pool objects.

% REFERENCE ONLY FIXME REMOVE
% data InvocationLabel
%         = X64PDPTMap
%         | X64PDPTUnmap
%         | X64PageDirectoryMap
%         | X64PageDirectoryUnmap
%         | X64PageTableMap
%         | X64PageTableUnmap
%         | X64IOPageTableMap   -- IOPageTableInvocation
%         | X64IOPageTableUnmap -- IOPageTableInvocation
%         | X64PageMap
%         | X64PageRemap
%         | X64PageUnmap
%         | X64PageMapIO -- IOPageInvocation? PageInvocation?
%         | X64PageGetAddress
%         | X64ASIDControlMakePool -- "ASIDs are same as ARM basically"
%         | X64ASIDPoolAssign
%         | X64IOPortIn8
%         | X64IOPortIn16
%         | X64IOPortIn32
%         | X64IOPortOut8
%         | X64IOPortOut16
%         | X64IOPortOut32
%         | X64IRQIssueIRQHandlerIOAPIC -- IRQControlCap
%         | X64IRQIssueIRQHandlerMSI    -- IRQControlCap
%         deriving (Eq, Enum, Bounded)

FIXME All object invocations implicitly involve a cap to some object and a slot?
Presumably if we point at an object the kernel can figure out the cap and the slot?

> data Invocation
>     = InvokePDPT PDPTInvocation
>     | InvokePageDirectory PageDirectoryInvocation
>     | InvokePageTable PageTableInvocation
>     | InvokeIOPageTable IOPageTableInvocation
>     | InvokePage PageInvocation
>     | InvokeASIDControl ASIDControlInvocation
>     | InvokeASIDPool ASIDPoolInvocation
>     | InvokeIOPort IOPortInvocation
>     | InvokeIRQ IRQInvocation -- FIXME this used to be generic, but now we're adding arch-dependent functionality
>     deriving Show

> data PageTableInvocation
>     = PageTableUnmap {
>         ptUnmapCap :: ArchCapability,
>         ptUnmapCapSlot :: PPtr CTE }
>     | PageTableMap {
>         ptMapCap :: Capability,
>         ptMapCTSlot :: PPtr CTE,
>         ptMapPDE :: PDE,
>         ptMapPDSlot :: PPtr PDE }
>     deriving Show

> -- FIXME: should we consolidate start, end into a tuple
> data PageInvocation
>     = PageGetAddr {
>         pageGetBasePtr :: PPtr Word }
>     | PageRemap {
>         pageRemapASID :: ASID,
>         pageRemapEntries :: Either (PTE, [PPtr PTE]) (PDE, [PPtr PDE]) }
>     | PageMap {
>         pageMapASID :: ASID,
>         pageMapCap :: Capability,
>         pageMapCTSlot :: PPtr CTE,
>         pageMapEntries :: Either (PTE, [PPtr PTE]) (PDE, [PPtr PDE]) }
>     | PageUnmap {
>         pageUnmapCap :: ArchCapability,
>         pageUnmapCapSlot :: PPtr CTE }
>     | PageIOMap {
>         pageMapASID :: ASID,
>         pageMapCap :: Capability,
>         pageMapCTSlot :: PPtr CTE,
>         pageMapEntries :: (IOPT, [PPtr IOPTE]) } -- FIXME : IO Types here. there's no ASID we care about?
>     deriving Show

> data ASIDControlInvocation
>     = MakePool {
>         makePoolFrame :: PPtr (),
>         makePoolSlot :: PPtr CTE,
>         makePoolParent :: PPtr CTE,
>         makePoolBase :: ASID }
>     deriving Show

> data ASIDPoolInvocation
>     = Assign {
>         assignASID :: ASID,
>         assignASIDPool :: PPtr ASIDPool,
>         assignASIDCTSlot :: PPtr CTE }
>     deriving Show

\subsection{Interrupt Control}

FIXME x86 64bit has two interrupt control invocations, one each for IOAPIC and MSI interrupt sources.
FIXME TODO arguments to this plus decode

> data InterruptControl
>     = IssueIRQHandlerIOAPIC
>     | IssueIRQHandlerMSI
>     deriving (Show, Eq)

\subsection{Additional Register Subsets}

The ARM platform currently does not define any additional register sets for the "CopyRegisters" operation. This may be changed in future to support a floating point unit.

FIXME this needs to go somewhere else: invocation or invocationlabels?

> data CopyRegisterSets = ARMNoExtraRegisters
>     deriving Show


