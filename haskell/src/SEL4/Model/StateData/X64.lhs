% Copyright 2014, General Dynamics C4 Systems
%
% This software may be distributed and modified according to the terms of
% the GNU General Public License version 2. Note that NO WARRANTY is provided.
% See "LICENSE_GPLv2.txt" for details.
%
% @TAG(GD_GPL)
%

This module contains the architecture-specific kernel global data for the X86-64bit architecture.
 
> module SEL4.Model.StateData.X64 where

\begin{impdetails}

> import SEL4.Machine
> import SEL4.Machine.Hardware.X64
>    (PML4E(..))
> --   (HardwareASID(..), PDPTE(..), PTE(..), PDE(..), PML4E(..), ptBits)
> import SEL4.Object.Structures.X64

> import Data.Array
> import Data.Word (Word16)
> -- import Data.Bits
> -- import Data.Helpers

\end{impdetails}

FIXME move this somewhere more appropriate?

> data GdtSlot
>     = GDT_NULL
>     | GDT_CS_0
>     | GDT_DS_0
>     | GDT_TSS_1
>     | GDT_TSS_2
>     | GDT_CS_3
>     | GDT_DS_3
>     | GDT_TLS
>     | GDT_IPCBUF
>     | GDT_ENTRIES
>     deriving (Eq, Show, Enum)

FIXME the only difference in our GDT entries seems to be the base address (broken up into pieces)
FIXME Right now the gdt entry structure in C only has 32 bits for addresses, so something is fishy

> data GdtEntry = X64GdtEntry {
>     base :: PPtr PML4E}

> data KernelState = X64KernelState {
>     x64KSGdt :: Array GdtSlot GdtEntry,
>     x64KSASIDTable :: Array ASID (Maybe (PPtr ASIDPool)),
>     x64KSGlobalPML4 :: PPtr PML4E}

> newKernelState :: PAddr -> (KernelState, [PAddr])
> newKernelState _data_start = error "No initial state defined for x64"

