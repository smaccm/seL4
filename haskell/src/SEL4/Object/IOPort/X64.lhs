% Copyright 2014, General Dynamics C4 Systems
%
% This software may be distributed and modified according to the terms of
% the GNU General Public License version 2. Note that NO WARRANTY is provided.
% See "LICENSE_GPLv2.txt" for details.
%
% @TAG(GD_GPL)
%

This module defines IO port routines, specific to x64.

> module SEL4.Object.IOPort.X64 where

\begin{impdetails}

> {-# BOOT-IMPORTS: SEL4.Machine SEL4.Model SEL4.Object.Structures SEL4.Object.Instances() SEL4.API.Types SEL4.API.Failures SEL4.API.Invocation.X64 SEL4.API.InvocationLabels.X64 #-}
> {-# BOOT-EXPORTS: performX64PortInvocation decodeX64PortInvocation #-}

> import SEL4.Machine
> import SEL4.API.Types
> import SEL4.API.Failures
> import SEL4.Machine.RegisterSet
> import SEL4.Machine.Hardware.X64
> import SEL4.Model
> import SEL4.Object.Structures
> import SEL4.Object.TCB
> import SEL4.API.Invocation.X64 as ArchInv
> import SEL4.API.Invocation
> import SEL4.API.InvocationLabels.X64

\end{impdetails}

> ensurePortOperationAllowed :: ArchCapability -> IOPort -> Int ->
>     KernelF SyscallError ()
> ensurePortOperationAllowed
>     cap@(IOPortCap { capIOPortFirstPort = first_allowed,
>                      capIOPortLastPort = last_allowed })
>     start_port size = do
>     let end_port = start_port + fromIntegral size - 1
>     assert (first_allowed <= last_allowed) "first allowed must be less than last allowed"
>     assert (start_port <= end_port) "start port must be less than end port"
>     when ((start_port < first_allowed) || (end_port > last_allowed)) $
>         throw IllegalOperation
> ensurePortOperationAllowed _ _ _ = fail "Unreachable"

FIXME kernel people need to fix the C here and not pack port and output data into a single register

> decodeX64PortInvocation :: Word -> [Word] -> CPtr -> PPtr CTE ->
>         ArchCapability -> [(Capability, PPtr CTE)] ->
>         KernelF SyscallError ArchInv.Invocation
> decodeX64PortInvocation label args _ _ cap@(IOPortCap {}) _ = do
>     case (invocationType label, args) of
>         (ArchInvocationLabel X64IOPortIn8, port':_) -> do
>             let port = (fromIntegral port') :: IOPort
>             ensurePortOperationAllowed cap port 1
>             return $ InvokeIOPort $ IOPortInvocation port $ IOPortIn8
>         (ArchInvocationLabel X64IOPortIn16, port':_) -> do
>             let port = (fromIntegral port') :: IOPort
>             ensurePortOperationAllowed cap port 2
>             return $ InvokeIOPort $ IOPortInvocation port $ IOPortIn16
>         (ArchInvocationLabel X64IOPortIn32, port':_) -> do
>             let port = (fromIntegral port') :: IOPort
>             ensurePortOperationAllowed cap port 4
>             return $ InvokeIOPort $ IOPortInvocation port $ IOPortIn32
>         (ArchInvocationLabel X64IOPortOut8, port':out:_) -> do
>             let port = (fromIntegral port') :: IOPort
>             ensurePortOperationAllowed cap port 1
>             let output_data = fromIntegral out
>             return $ InvokeIOPort $ IOPortInvocation port $ IOPortOut8 output_data
>         (ArchInvocationLabel X64IOPortOut16, port':out:_)-> do
>             let port = (fromIntegral port') :: IOPort
>             ensurePortOperationAllowed cap port 2
>             let output_data = fromIntegral out
>             return $ InvokeIOPort $ IOPortInvocation port $ IOPortOut16 output_data
>         (ArchInvocationLabel X64IOPortOut32, port':out:_) -> do
>             let port = (fromIntegral port') :: IOPort
>             ensurePortOperationAllowed cap port 4
>             let output_data = fromIntegral out
>             return $ InvokeIOPort $ IOPortInvocation port $ IOPortOut32 output_data
>         (_, _) -> throw TruncatedMessage
> decodeX64PortInvocation _ _ _ _ _ _ = fail "Unreachable"

> performX64PortInvocation :: ArchInv.Invocation -> KernelP [Word]
> performX64PortInvocation (InvokeIOPort (IOPortInvocation port port_data)) = withoutPreemption $ do
>     case port_data of
>         ArchInv.IOPortIn8 -> portIn in8
>         ArchInv.IOPortIn16 -> portIn in16
>         ArchInv.IOPortIn32 -> portIn in32
>         ArchInv.IOPortOut8 w -> portOut out8 w
>         ArchInv.IOPortOut16 w -> portOut out16 w
>         ArchInv.IOPortOut32 w -> portOut out32 w
>     return $ []
>     where
>         portIn f = do
>             ct <- getCurThread
>             res <- doMachineOp f
>             setMRs ct Nothing [res]
>             msgInfo <- return $ MI {
>                 msgLength = 1,
>                 msgExtraCaps = 0,
>                 msgCapsUnwrapped = 0,
>                 msgLabel = 0 }
>             setMessageInfo ct msgInfo
>         portOut f w = do
>             ct <- getCurThread
>             doMachineOp $ f port w
>             setMessageInfo ct $ MI 0 0 0 0

> performX64PortInvocation _ = fail "Unreachable"

