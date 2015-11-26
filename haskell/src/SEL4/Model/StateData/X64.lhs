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
> import SEL4.Machine.RegisterSet.X64

> import Data.Array
> import Data.Word (Word16)
> -- import Data.Bits
> -- import Data.Helpers

\end{impdetails}

FIXME the only difference in our GDT entries seems to be the base address (broken up into pieces)
FIXME Right now the gdt entry structure in C only has 32 bits for addresses, so something is fishy

> data GDTE = GDTE {
>     gdteFrame :: PAddr }

> data KernelState = X64KernelState {
>     x64KSGdt :: Array GDTSlot GDTE,
>     x64KSASIDTable :: Array ASID (Maybe (PPtr ASIDPool)),
>     x64KSGlobalPML4 :: PPtr PML4E}

> newKernelState :: PAddr -> (KernelState, [PAddr])
> newKernelState _data_start = error "No initial state defined for x64"

