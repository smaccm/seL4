% Copyright 2014, General Dynamics C4 Systems
%
% This software may be distributed and modified according to the terms of
% the GNU General Public License version 2. Note that NO WARRANTY is provided.
% See "LICENSE_GPLv2.txt" for details.
%
% @TAG(GD_GPL)
%

This module contains the architecture-specific thread switch code for X86-64bit.

> module SEL4.Kernel.Thread.X64 where

\begin{impdetails}

> import SEL4.Machine
> import SEL4.Machine.RegisterSet.X64
> import SEL4.Model.StateData
> import SEL4.Model.StateData.X64
> import SEL4.Object.Structures
> import SEL4.Object.TCB
> import SEL4.Kernel.VSpace.X64
> import qualified SEL4.Machine.Hardware.X64 as X64Hardware
> import {-# SOURCE #-} SEL4.Kernel.Init
> import SEL4.Model.PSpace

\end{impdetails}

> switchToThread :: PPtr TCB -> Kernel ()
> switchToThread tcb = do
>     setVMRoot tcb
>     base <- asUser tcb $ getRegister (Register TLS_BASE)
>     bufferPtr <- threadGet tcbIPCBuffer tcb
>     gdt <- gets $ x86KSGdt . ksArchState
>     let gdt' <- gdt//[ (GDT_TLS, base), (GDT_IPCBUF, bufferPtr) ]
>     modify (\s -> s {
>         ksArchState = (ksArchState s) { x64Gdt = gdt' }})

> configureIdleThread :: PPtr TCB -> KernelInit ()
> configureIdleThread tcb = do
>     doKernelOp $ asUser tcb $ do
>         setRegister (Register RFLAGS) 0x1f
>         setRegister (Register NEXTIP) $ fromVPtr idleThreadStart
>         setRegister (Register CS) selCS0
>         setRegister (Register DS) selDS0
>         setRegister (Register ES) selDS0
>         setRegister (Register FS) selDS0
>         setRegister (Register GS) selDS0
>         setRegister (Register SS) selDS0
>         setRegister (Register RSP) $ kernelStackAlloc

> switchToIdleThread :: Kernel ()
> switchToIdleThread = return ()

> activateIdleThread :: PPtr TCB -> Kernel ()
> activateIdleThread _ = return ()

