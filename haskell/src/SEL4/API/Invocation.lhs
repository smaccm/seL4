%
% Copyright 2014, General Dynamics C4 Systems
%
% This software may be distributed and modified according to the terms of
% the GNU General Public License version 2. Note that NO WARRANTY is provided.
% See "LICENSE_GPLv2.txt" for details.
%
% @TAG(GD_GPL)
%

This module defines the interfaces presented to clients by the kernel's objects.

\begin{impdetails}

We use the C preprocessor to select a target architecture.

> {-# LANGUAGE CPP #-}

\end{impdetails}

> module SEL4.API.Invocation where

\begin{impdetails}

> import SEL4.Machine
> import SEL4.API.Types
> import SEL4.Object.Structures

\end{impdetails}

The architecture-specific definitions are imported qualified with the "Arch" prefix.

> import qualified SEL4.API.Invocation.TARGET as Arch
> import qualified SEL4.API.InvocationLabels.TARGET as ArchLabels

\subsection{Invocation Type}

The following type can specify any kernel object invocation. It contains physical pointers to any kernel objects required for the operation, and other arguments decoded from the message registers.

> data Invocation
>         = InvokeUntyped UntypedInvocation
>         | InvokeEndpoint (PPtr Endpoint) Word Bool
>         | InvokeNotification (PPtr Notification) Word 
>         | InvokeReply (PPtr TCB) (PPtr CTE)
>         | InvokeDomain (PPtr TCB) Domain
>         | InvokeTCB TCBInvocation
>         | InvokeCNode CNodeInvocation
>         | InvokeIRQControl IRQControlInvocation
>         | InvokeIRQHandler IRQHandlerInvocation
>         | InvokeArchObject Arch.Invocation
>         deriving Show

\subsubsection{TCB Object Invocations}

The following data type defines the set of possible TCB invocation operations. The operations are discussed and defined in more detail in \autoref{sec:object.tcb}.

> data TCBInvocation
>         = Suspend { suspendThread :: PPtr TCB }
>         | Resume { resumeThread :: PPtr TCB }
>         | ThreadControl {
>             tcThread :: PPtr TCB,
>             tcThreadCapSlot :: PPtr CTE,
>             tcNewFaultEP :: Maybe CPtr,
>             tcNewPriority :: Maybe Priority,
>             tcNewCRoot, tcNewVRoot :: Maybe (Capability, PPtr CTE),
>             tcNewIPCBuffer :: Maybe (VPtr, Maybe (Capability, PPtr CTE)) }
>         | NotificationControl { 
>             notificationTCB :: PPtr TCB,
>             notificationPtr :: Maybe (PPtr Notification) }
>         | WriteRegisters {
>             writeRegsThread :: PPtr TCB,
>             writeRegsResume :: Bool,
>             writeRegsValues :: [Word],
>             writeRegsArch :: Arch.CopyRegisterSets }
>         | ReadRegisters {
>             readRegsThread :: PPtr TCB,
>             readRegsSuspend :: Bool,
>             readRegsLength :: Word,
>             readRegsArch :: Arch.CopyRegisterSets }
>         | CopyRegisters {
>             copyRegsTarget :: PPtr TCB,
>             copyRegsSource :: PPtr TCB,
>             copyRegsSuspendSource, copyRegsResumeTarget :: Bool, 
>             copyRegsTransferFrame, copyRegsTransferInteger :: Bool,
>             copyRegsTransferArch :: Arch.CopyRegisterSets }
>         deriving Show

\subsubsection{CNode Invocations}

The following data type defines the set of possible CNode invocation operations. The operations are discussed and defined in more detail in \autoref{sec:object.cnode}.

> data CNodeInvocation
>         = Insert {
>             insertCap :: Capability,
>             sourceSlot, targetSlot :: PPtr CTE }
>         | Rotate {
>             moveCap1, moveCap2 :: Capability,
>             sourceSlot, pivotSlot, targetSlot :: PPtr CTE }
>         | Revoke { targetSlot :: PPtr CTE }
>         | Move {
>             moveCap :: Capability,
>             sourceSlot, targetSlot :: PPtr CTE }
>         | Recycle { targetSlot :: PPtr CTE }
>         | SaveCaller {
>             targetSlot :: PPtr CTE }
>         | Delete { targetSlot :: PPtr CTE }
>         deriving Show

\subsubsection{Untyped Invocations}

The following data type defines the parameters expected for invocations of Untyped objects.

> data UntypedInvocation
>         = Retype {
>             retypeSource :: PPtr CTE,
>             retypeRegionBase :: PPtr (),
>             retypeFreeRegionBase :: PPtr (),
>             retypeNewType :: ObjectType,
>             retypeNewSizeBits :: Int,
>             retypeSlots :: [PPtr CTE] }
>         deriving Show

\subsubsection{Interrupt Controller Invocations}

The following data type defines the set of possible invocations for interrupt controller capabilities.

> data IRQControlInvocation
>         = InterruptControl { interruptControlArch :: Arch.InterruptControl }
>         | IssueIRQHandler {
>             issueHandlerIRQ :: IRQ,
>             issueHandlerSlot, issueHandlerControllerSlot :: PPtr CTE }
>         deriving Show

\subsubsection{IRQ Handler Invocations}

The following data type defines the set of possible invocations for IRQ capabilities.

> data IRQHandlerInvocation
>         = AckIRQ { irqHandlerIRQ :: IRQ }
>         | ClearIRQHandler { irqHandlerIRQ :: IRQ }
>         | SetMode {
>             modeIRQ :: IRQ,
>             modeTrigger :: Bool,
>             modePolarity :: Bool }
>         | SetIRQHandler {
>             irqHandlerIRQ :: IRQ,
>             setIRQHandlerCap :: Capability,
>             setIRQHandlerSlot :: PPtr CTE }
>         deriving Show

\subsection{Invocation Labels}

The following type enumerates all the kinds of invocations that clients can request of the kernel. The derived Enum instance defines the message label that clients should use when requesting that service. These labels are enumerated globally to ensure that no objects share an invocation label. This is to avoid confusion: service requests to the wrong object will fail immediately rather than perform unexpected actions.

FIXME this is not quite accurate anymore, no one knows what the argument should be however

> data InvocationLabel
>         = InvalidInvocation
>         | UntypedRetype
>         | TCBReadRegisters
>         | TCBWriteRegisters
>         | TCBCopyRegisters
>         | TCBConfigure
>         | TCBSetPriority
>         | TCBSetIPCBuffer
>         | TCBSetSpace
>         | TCBSuspend
>         | TCBResume
>         | TCBBindNotification
>         | TCBUnbindNotification
>         | CNodeRevoke
>         | CNodeDelete
>         | CNodeRecycle
>         | CNodeCopy
>         | CNodeMint
>         | CNodeMove
>         | CNodeMutate
>         | CNodeRotate
>         | CNodeSaveCaller
>         | IRQIssueIRQHandler
>         | IRQInterruptControl
>         | IRQAckIRQ
>         | IRQSetIRQHandler
>         | IRQClearIRQHandler
>         | IRQSetMode
>         | DomainSetSet
>         | ArchInvocationLabel ArchLabels.InvocationLabel
>         deriving (Show, Eq)

> instance Bounded InvocationLabel where
>     minBound = InvalidInvocation
>     maxBound = maxBound ArchLabels.InvocationLabel

> instance Enum InvocationLabel where
>     fromEnum e = case e of
>          InvalidInvocation -> 0
>          UntypedRetype -> 1
>          TCBReadRegisters -> 2
>          TCBWriteRegisters -> 3
>          TCBCopyRegisters -> 4
>          TCBConfigure -> 5
>          TCBSetPriority -> 6
>          TCBSetIPCBuffer -> 7
>          TCBSetSpace -> 8
>          TCBSuspend -> 9
>          TCBResume -> 10
>          TCBBindNotification -> 11
>          TCBUnbindNotification -> 12
>          CNodeRevoke -> 13
>          CNodeDelete -> 14
>          CNodeRecycle -> 15
>          CNodeCopy -> 16
>          CNodeMint -> 17
>          CNodeMove -> 18
>          CNodeMutate -> 19
>          CNodeRotate -> 20
>          CNodeSaveCaller -> 21
>          IRQIssueIRQHandler -> 22
>          IRQInterruptControl -> 23
>          IRQAckIRQ -> 24
>          IRQSetIRQHandler -> 25
>          IRQClearIRQHandler -> 26
>          IRQSetMode -> 27
>          DomainSetSet -> apiMax
>          ArchInvocationLabel a -> apiMax + 1 + fromEnum a
>          where apiMax = 28
>     toEnum n
>         | n == 0 = InvalidInvocation
>         | n == 1 = UntypedRetype
>         | n == 2 = TCBReadRegisters
>         | n == 3 = TCBWriteRegisters
>         | n == 4 = TCBCopyRegisters
>         | n == 5 = TCBConfigure
>         | n == 6 = TCBSetPriority
>         | n == 7 = TCBSetIPCBuffer
>         | n == 8 = TCBSetSpace
>         | n == 9 = TCBSuspend
>         | n == 10 = TCBResume
>         | n == 11 = TCBBindNotification
>         | n == 12 = TCBUnbindNotification
>         | n == 13 = CNodeRevoke
>         | n == 14 = CNodeDelete
>         | n == 15 = CNodeRecycle
>         | n == 16 = CNodeCopy
>         | n == 17 = CNodeMint
>         | n == 18 = CNodeMove
>         | n == 19 = CNodeMutate
>         | n == 20 = CNodeRotate
>         | n == 21 = CNodeSaveCaller
>         | n == 22 = IRQIssueIRQHandler
>         | n == 23 = IRQInterruptControl
>         | n == 24 = IRQAckIRQ
>         | n == 25 = IRQSetIRQHandler
>         | n == 26 = IRQClearIRQHandler
>         | n == 27 = IRQSetMode
>         | n == 28 = DomainSetSet
>         | n > apiMax = ArchInvocationLabel $ toEnum n
>         | otherwise = error "toEnum out of range for InvocationLabel"
>         where apiMax = 28

Decode the invocation type requested by a particular message label.

> invocationType :: Word -> InvocationLabel
> invocationType x
>     | x' <= fromEnum (maxBound :: InvocationLabel) = toEnum x'
>     | otherwise = InvalidInvocation
>     where x' = fromIntegral x

> isPDFlush :: InvocationLabel -> Bool
> isPDFlush x = case x of
>       ArchInvocationLabel a -> ArchLabels.isPDFlush a
>       _ -> False

> isPageFlush :: InvocationLabel -> Bool
> isPageFlush x = case x of
>       ArchInvocationLabel a -> ArchLabels.isPageFlush a
>       _ -> False

