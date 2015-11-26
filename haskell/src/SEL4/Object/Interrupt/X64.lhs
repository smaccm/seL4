% Copyright 2014, General Dynamics C4 Systems
%
% This software may be distributed and modified according to the terms of
% the GNU General Public License version 2. Note that NO WARRANTY is provided.
% See "LICENSE_GPLv2.txt" for details.
%
% @TAG(GD_GPL)
%

This module defines the machine-specific interrupt handling routines for x64.

> module SEL4.Object.Interrupt.X64 where

\begin{impdetails}

> import SEL4.Machine
> import SEL4.Model
> import SEL4.Object.Structures
> import SEL4.API.Failures
> import SEL4.API.Invocation.X64 as Arch

\end{impdetails}

> decodeIRQControl :: Word -> [Word] -> PPtr CTE -> [Capability] ->
>         KernelF SyscallError Arch.IRQControlInvocation
> decodeIRQControl _ _ _ _ = throw IllegalOperation -- FIXME IMPLEMENT

> performIRQControl :: Arch.IRQControlInvocation -> KernelP ()
> performIRQControl _ = fail "FIXME X64 IMPLEMENT"

