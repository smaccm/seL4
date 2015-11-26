% Copyright 2014, General Dynamics C4 Systems
%
% This software may be distributed and modified according to the terms of
% the GNU General Public License version 2. Note that NO WARRANTY is provided.
% See "LICENSE_GPLv2.txt" for details.
%
% @TAG(GD_GPL)
%

This module defines the machine-specific interrupt handling routines for x64.

> {-# LANGUAGE CPP #-}

> module SEL4.Object.Interrupt.X64 where


\begin{impdetails}

> import SEL4.Machine
> import SEL4.Model
> import SEL4.Object.Structures
> import SEL4.API.Failures
> import SEL4.API.Types
> import SEL4.API.Invocation
> import SEL4.API.Invocation.X64 as ArchInv
> import SEL4.API.InvocationLabels.X64 as ArchLabels
> import {-# SOURCE #-} SEL4.Object.CNode
> import {-# SOURCE #-} SEL4.Kernel.CSpace
> import qualified SEL4.Machine.Hardware.X64 as Arch


\end{impdetails}

FIXME IRQ shouldn't be last in the list of args! talk to kernel people

FIXME this copies a lot of code from decodeIRQControl that is completely generic, but that's what the C code does too... the C code does not look good here, and needs to be fixed after which we can fix this

> decodeIRQControl :: Word -> [Word] -> PPtr CTE -> [Capability] ->
>         KernelF SyscallError ArchInv.IRQControlInvocation
> decodeIRQControl label args srcSlot extraCaps =
>     case (invocationType label, args, extraCaps) of
>         (ArchInvocationLabel ArchLabels.X64IRQIssueIRQHandlerIOAPIC,
>                  index:depth:ioapic:pin:level:polarity:irqW:_, cnode:_) -> do
>
>             rangeCheck irqW (fromEnum minIRQ) (fromEnum maxIRQ)
>             -- FIXME: is this right for x64-specific?
>             let irq = toEnum (fromIntegral irqW) :: IRQ
>
>             destSlot <- lookupTargetSlot cnode (CPtr index)
>                 (fromIntegral depth)
>             ensureEmptySlot destSlot
>
>             -- FIXME check semantics against toEnum, we might want == 0 here
>             let vector = (fromIntegral $ fromEnum irq) + Arch.irqIntOffset
>             return $ ArchInv.IssueIRQHandlerIOAPIC irq destSlot srcSlot ioapic
>                 pin (toEnum $ fromIntegral level) (toEnum $ fromIntegral polarity) vector
>
>         (ArchInvocationLabel ArchLabels.X64IRQIssueIRQHandlerMSI,
>                  index:depth:pciBus:pciDev:pciFunc:handle:irqW:_, cnode:_) -> do
>
>             rangeCheck irqW (fromEnum minIRQ) (fromEnum maxIRQ)
>             -- FIXME: is this right for x64-specific?
>             let irq = toEnum (fromIntegral irqW) :: IRQ
>
>             destSlot <- lookupTargetSlot cnode (CPtr index)
>                 (fromIntegral depth)
>             ensureEmptySlot destSlot
>
>             rangeCheck pciBus 0 Arch.maxPCIBus
>             rangeCheck pciDev 0 Arch.maxPCIDev
>             rangeCheck pciFunc 0 Arch.maxPCIFunc
>
>             return $ ArchInv.IssueIRQHandlerMSI irq destSlot srcSlot pciBus
>                 pciDev pciFunc handle
>
>         _ -> throw IllegalOperation

> performIRQControl :: ArchInv.IRQControlInvocation -> KernelP ()
> performIRQControl _ = fail "FIXME X64 IMPLEMENT"

