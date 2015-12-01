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
> import {-# SOURCE #-} SEL4.Object.Interrupt
> import qualified SEL4.Machine.Hardware.X64 as Arch


\end{impdetails}

FIXME IRQ shouldn't be last in the list of args! talk to kernel people

FIXME this copies a lot of code from decodeIRQControl that is completely generic, but that's what the C code does too... the C code does not look good here, and needs to be fixed after which we can fix this. In fact, the C code could use a rewrite.

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
>             -- from ioapic_map_pin_to_vector
>             rangeCheck ioapic 0 (Arch.numIOAPICs - 1)
>             rangeCheck pin 0 (Arch.ioapicIRQLines - 1)
>             rangeCheck level (0::Word) 1
>             rangeCheck polarity (0::Word) 1
>
>             -- FIXME check semantics against toEnum, we might want == 0 here
>             let vector = (fromIntegral $ fromEnum irq) + Arch.irqIntOffset
>             return $ ArchInv.IssueIRQHandlerIOAPIC irq destSlot srcSlot ioapic
>                 pin level polarity vector
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
> performIRQControl (ArchInv.IssueIRQHandlerIOAPIC (IRQ irq) destSlot srcSlot ioapic
>         pin level polarity vector) = withoutPreemption $ do
>     doMachineOp $ Arch.ioapicMapPinToVector ioapic pin level polarity vector
>     irqState <- doMachineOp $ Arch.irqStateIRQIOAPICNew ioapic pin level polarity (1::Word) (0::Word)
>     doMachineOp $ Arch.updateIRQState irq irqState
>     -- do same thing as generic path in performIRQControl in Interrupt.lhs
>     setIRQState IRQSignal (IRQ irq)
>     cteInsert (IRQHandlerCap (IRQ irq)) destSlot srcSlot
>     return ()
>
> performIRQControl (ArchInv.IssueIRQHandlerMSI (IRQ irq) destSlot srcSlot pciBus
>         pciDev pciFunc handle) = withoutPreemption $ do
>     irqState <- doMachineOp $ Arch.irqStateIRQMSINew pciBus pciDev pciFunc handle
>     doMachineOp $ Arch.updateIRQState irq irqState
>     -- do same thing as generic path in performIRQControl in Interrupt.lhs
>     setIRQState IRQSignal (IRQ irq)
>     cteInsert (IRQHandlerCap (IRQ irq)) destSlot srcSlot
>     return ()

