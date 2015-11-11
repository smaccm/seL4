%
% Copyright 2014, General Dynamics C4 Systems
%
% This software may be distributed and modified according to the terms of
% the GNU General Public License version 2. Note that NO WARRANTY is provided.
% See "LICENSE_GPLv2.txt" for details.
%
% @TAG(GD_GPL)
%
FIXME who is the 64-bit kernel copyright by?

This module defines the x86 64-bit register set.

> module SEL4.Machine.RegisterSet.X64 where

\begin{impdetails}

> import qualified Data.Word
> import Data.Array
> import Data.Bits

\end{impdetails}

> data Register =
>     RAX | RBX | RCX | RDX | RSI | RDI | RBP |
>     R8 | R9 | R10 | R11 | R12 | R13 | R14 | R15 | DS | ES | FS | GS |
>     FaultInstruction | -- "FaultIP"
>     TLS_BASE | PADDING_REGISTER |
>     ErrorRegister | NextIP | CS | RFLAGS | RSP | SS
>     deriving (Eq, Enum, Bounded, Ord, Ix, Show)

> type Word = Data.Word.Word64

> capRegister = RBX
> msgInfoRegister = RSI
> msgRegisters = [RDI, RBP]
> badgeRegister = capRegister
> frameRegisters = FaultInstruction : RSP : RFLAGS : [RAX .. R15]
> gpRegisters = [TLS_BASE, FS, GS]
> exceptionMessage = [FaultInstruction, RSP, RFLAGS]

FIXME kernel people are not using R8-R15 at the moment, but that can change

> syscallMessage = [RAX .. RBP] ++ [NextIP, RSP, RFLAGS]

FIXME move to structures?

> gdtToSel :: Word -> Word
> gdtToSel n = (n `shiftL` 3) .|. 3

> selCS3 = gdtToSel 5
> selDS3 = gdtToSel 6
> selTLS = gdtToSel 7
> selIPCBUF = gdtToSel 8

> initContext :: [(Register, Word)]
> initContext = [(DS, selDS3), (ES, selDS3), (CS, selCS3), (SS, selDS3)
>               ,(RFLAGS, bit 9 .|. bit 1)] -- User mode

FIXME FPU context?

> sanitiseRegister :: Register -> Word -> Word
> sanitiseRegister RFLAGS v =
>     v .|. bit 1 .&. (complement (bit 3)) .&. (complement (bit 5))  .|. bit 9
>     .&. (bit 12 - 1)
> sanitiseRegister FS v = if v /= selTLS && v /= selIPCBUF then 0 else v
> sanitiseRegister GS v = if v /= selTLS && v /= selIPCBUF then 0 else v
> sanitiseRegister _ v = v

