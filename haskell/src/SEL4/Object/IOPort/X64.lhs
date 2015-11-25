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
> import SEL4.API.Invocation.X64
> import SEL4.API.InvocationLabels.X64

\end{impdetails}

> ensurePortOperationAllowed :: ArchCapability -> IOPort -> Int ->
>     KernelF SyscallError ()
> ensurePortOperationAllowed
>     cap@(IOPortCap { capIOPortFirstPort = first_allowed,
>                      capIOPortLastPort = last_allowed })
>     start_port size = do
>     let end_port = start_port + fromIntegral size - 1
>     assert (first_allowed <= last_allowed)
>     assert (start_port <= end_port)
>     when ((start_port < first_allowed) || (end_port > last_allowed))
>         throw IllegalOperation

FIXME kernel people need to fix the C here and not pack port and output data into a single register

> decodeX64PortInvocation :: Word -> [Word] -> CPtr -> PPtr CTE ->
>         ArchCapability -> [(Capability, PPtr CTE)] ->
>         KernelF SyscallError Invocation
> decodeX64PortInvocation label args _ _ cap@(IOPortCap {}) _ = do
>     case (invocationType label, args) of
>         (X64IOPortIn8, port:_) -> do
>             let port' = (fromIntegral port) :: IOPort
>             ensurePortOperationAllowed cap port 1
>             return $ InvokeIOPort $ IOPortInvocation port $ IOPortIn8
>         (X64IOPortIn16, port:_) -> do
>             let port' = (fromIntegral port) :: IOPort
>             ensurePortOperationAllowed cap port 2
>             return $ InvokeIOPort $ IOPortInvocation port $ IOPortIn16
>         (X64IOPortIn32, port:_) -> do
>             let port' = (fromIntegral port) :: IOPort
>             ensurePortOperationAllowed cap port 4
>             return $ InvokeIOPort $ IOPortInvocation port $ IOPortIn32
>         (X64IOPortOut8, port:out:_) -> do
>             let port' = (fromIntegral port) :: IOPort
>             ensurePortOperationAllowed cap port 1
>             let output_data = fromIntegral out
>             return $ InvokeIOPort $ IOPortInvocation port $ IOPortOut8 output_data
>         (X64IOPortOut16, port:out:_)-> do
>             let port' = (fromIntegral port) :: IOPort
>             ensurePortOperationAllowed cap port 2
>             let output_data = fromIntegral out
>             return $ InvokeIOPort $ IOPortInvocation port $ IOPortOut16 output_data
>         (X64IOPortOut32, port:out:_) -> do
>             let port' = (fromIntegral port) :: IOPort
>             ensurePortOperationAllowed cap port 4
>             let output_data = fromIntegral out
>             return $ InvokeIOPort $ IOPortInvocation port $ IOPortOut32 output_data
>         (_, _) -> throw TruncatedMessage

> performX64PortInvocation :: Invocation -> KernelP [Word]
> performX64PortInvocation (InvokeIOPort (IOPortInvocation port port_data)) =
>     case port_data of
>         IOPortIn8 -> portIn in8
>         IOPortIn16 -> portIn in16
>         IOPortIn32 -> portIn in32
>         IOPortOut8 w -> portOut out8 w
>         IOPortOut16 w -> portOut out16 w
>         IOPortOut32 w -> portOut out32 w
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
>             setMessageInfo ct MI 0 0 0 0

