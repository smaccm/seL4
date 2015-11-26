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
> import SEL4.Machine.Hardware.X64 as Arch hiding (PAddr, IRQ)
> import SEL4.Object.Structures
> import Data.Word (Word8, Word16, Word32)

\end{impdetails}

\subsection{ARM-Specific Objects}

There are five ARM-specific object types; however, only four of them may be invoked. These are the page table, page, ASID control, and ASID pool objects.


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
>     deriving Show

> data PDPTInvocation
>     = PDPTUnmap {
>         pdptUnmapCap :: ArchCapability,
>         pdptUnmapCapSlot :: PPtr CTE }
>     | PDPTMap {
>         pdptMapCap :: Capability,
>         pdptMapCTSlot :: PPtr CTE,
>         pdptMapPML4E :: PML4E,
>         pdptMapPML4Slot :: PPtr PML4E }
>     deriving Show

> data PageDirectoryInvocation
>     = PageDirectoryUnmap {
>         pdUnmapCap :: ArchCapability,
>         pdUnmapCapSlot :: PPtr CTE }
>     | PageDirectoryMap {
>         pdMapCap :: Capability,
>         pdMapCTSlot :: PPtr CTE,
>         pdMapPDPTE :: PDPTE,
>         pdMapPDPTSlot :: PPtr PDPTE }
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

IO page tables are contained in other IO page tables. The topmost one sits in a
VTD context as a VTD context table entry (IOCTE).  If the context table entry
for a device does not have a page table entry, we must initialise that first to
point to the invoked IO page table.

> data IOPageTableInvocation
>     = IOPageTableUnmap {
>         ioptUnmapCap :: ArchCapability,
>         ioptUnmapCapSlot :: PPtr CTE }
>     | IOPageTableMap {
>         ioptMapCap :: Capability,
>         ioptMapCTSlot :: PPtr CTE,
>         ioptMapContextEntry :: IOCTE, -- FIXME IOCTE type? IO context entry
>         ioptMapPT :: IOPTE,
>         ioptMapPTSlot :: PPtr IOPTE }
>     deriving Show

> data PageInvocation
>     = PageGetAddr {
>         pageGetBasePtr :: PPtr Word }
>     | PageRemap {
>         pageRemapEntries :: (VMPageEntry, VMPageEntryPtr) }
>     | PageMap {
>         pageMapASID :: ASID,
>         pageMapCap :: Capability,
>         pageMapCTSlot :: PPtr CTE,
>         pageMapEntries :: (VMPageEntry, VMPageEntryPtr) }
>     | PageUnmap {
>         pageUnmapCap :: ArchCapability,
>         pageUnmapCapSlot :: PPtr CTE }
>     | PageIOMap {
>         pageIOMapASID :: ASID,
>         pageIOMapCap :: Capability,
>         pageIOMapCTSlot :: PPtr CTE,
>         pageIOMapEntries :: (IOPTE, [PPtr IOPTE]) } -- FIXME : IO Types here. there's no ASID we care about?
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

\subsection{IO Ports}

> data IOPortInvocationData
>     = IOPortIn8 | IOPortIn16 | IOPortIn32
>     | IOPortOut8 Word8 | IOPortOut16 Word16 | IOPortOut32 Word32
>     deriving Show

> data IOPortInvocation = IOPortInvocation IOPort IOPortInvocationData
>     deriving Show

\subsection{Interrupt Control}

FIXME x86 64bit has two interrupt control invocations, one each for IOAPIC and MSI interrupt sources.
FIXME TODO arguments to this plus decode
FIXME Word may be too generic for some of these
FIXME the kernel team is working on this currently, so it doesn't exactly match the C

There are two invocation labels corresponding to these, but no separate arch invocations. The ArchIRQControlInvocation is a special case of IRQControlInvocation.

> data IRQControlInvocation
>     = IssueIRQHandlerIOAPIC {
>         issueHandlerIOAPICIRQ :: IRQ,
>         issueHandlerIOAPICSlot, issueHandlerIOAPICControllerSlot :: PPtr CTE,
>         issueHandlerIOAPICIOAPIC :: Word,
>         issueHandlerIOAPICPin :: Word,
>         issueHandlerIOAPICLevel, issueHandlerIOAPICPolarity :: Word,
>         issueHandlerIOAPICVector :: Word }
>     | IssueIRQHandlerMSI {
>         issueHandlerMSIIRQ :: IRQ,
>         issueHandlerMSISlot, issueHandlerMSIControllerSlot :: PPtr CTE,
>         issueHandlerMSIPCIBus :: Word,
>         issueHandlerMSIPCIDev :: Word,
>         issueHandlerMSIPCIFunc :: Word,
>         issueHandlerMSIHandle :: Word }
>     deriving (Show, Eq)

\subsection{Additional Register Subsets}

The X64 platform currently does not define any additional register sets for the "CopyRegisters" operation. This may be changed in future to support a floating point unit.

> data CopyRegisterSets = X64NoExtraRegisters
>     deriving Show

