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
FIXME TODO move arch-dependent datatype to appropriate arch files
FIXME TODO define for x64

> data APIInvocationLabel
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
>         | ArchInvocationLabel ArchInvocationLabel
>         deriving (Show, Eq, Bounded)

> data InvocationLabel
>         = APIInvocationLabel APIInvocationLabel
>         | ARMPDClean_Data
>         | ARMPDInvalidate_Data
>         | ARMPDCleanInvalidate_Data
>         | ARMPDUnify_Instruction
>         | ARMPageTableMap
>         | ARMPageTableUnmap
>         | ARMPageMap
>         | ARMPageRemap
>         | ARMPageUnmap
>         | ARMPageClean_Data
>         | ARMPageInvalidate_Data
>         | ARMPageCleanInvalidate_Data
>         | ARMPageUnify_Instruction
>         | ARMPageGetAddress
>         | ARMASIDControlMakePool
>         | ARMASIDPoolAssign
>         deriving (Eq)

> instance Bounded InvocationLabel where
>     minBound = APIInvocationLabel minBound
>     maxBound = ARMASIDPoolAssign

> instance Enum InvocationLabel where
>     fromEnum e = case e of
>         APIInvocationLabel a -> fromEnum a
>         ARMPDClean_Data -> apiMax + 1
>         ARMPDInvalidate_Data -> apiMax + 2
>         ARMPDCleanInvalidate_Data -> apiMax + 3
>         ARMPDUnify_Instruction -> apiMax + 4
>         ARMPageTableMap -> apiMax + 5
>         ARMPageTableUnmap -> apiMax + 6
>         ARMPageMap -> apiMax + 7
>         ARMPageRemap -> apiMax + 8
>         ARMPageUnmap -> apiMax + 9
>         ARMPageClean_Data -> apiMax + 10
>         ARMPageInvalidate_Data -> apiMax + 11
>         ARMPageCleanInvalidate_Data -> apiMax + 12
>         ARMPageUnify_Instruction -> apiMax + 13
>         ARMPageGetAddress -> apiMax + 14
>         ARMASIDControlMakePool -> apiMax + 15
>         ARMASIDPoolAssign -> apiMax + 16
>         where apiMax = fromEnum (maxBound :: APIInvocationLabel)
>     toEnum n
>         | n <= apiMax = APIInvocationLabel $ toEnum n
>         | n == apiMax + 1 = ARMPDClean_Data
>         | n == apiMax + 2 = ARMPDInvalidate_Data
>         | n == apiMax + 3 = ARMPDCleanInvalidate_Data
>         | n == apiMax + 4 = ARMPDUnify_Instruction
>         | n == apiMax + 5 = ARMPageTableMap
>         | n == apiMax + 6 = ARMPageTableUnmap
>         | n == apiMax + 7 = ARMPageMap
>         | n == apiMax + 8 = ARMPageRemap
>         | n == apiMax + 9 = ARMPageUnmap
>         | n == apiMax + 10 = ARMPageClean_Data
>         | n == apiMax + 11 = ARMPageInvalidate_Data
>         | n == apiMax + 12 = ARMPageCleanInvalidate_Data
>         | n == apiMax + 13 = ARMPageUnify_Instruction
>         | n == apiMax + 14 = ARMPageGetAddress
>         | n == apiMax + 15 = ARMASIDControlMakePool
>         | n == apiMax + 16 = ARMASIDPoolAssign
>         | otherwise = error "toEnum out of range for ARM.InvocationLabel"
>         where apiMax = fromEnum (maxBound :: APIInvocationLabel)

Decode the invocation type requested by a particular message label.

> invocationType :: Word -> InvocationLabel
> invocationType x
>     | x' <= fromEnum (maxBound :: InvocationLabel) = toEnum x'
>     | otherwise = InvalidInvocation
>     where x' = fromIntegral x

> isPDFlush :: InvocationLabel -> Bool
> isPDFlush x = case x of
>       ARMPDClean_Data -> True
>       ARMPDInvalidate_Data -> True
>       ARMPDCleanInvalidate_Data -> True
>       ARMPDUnify_Instruction -> True
>       _ -> False

> isPageFlush :: InvocationLabel -> Bool
> isPageFlush x = case x of
>       ARMPageClean_Data -> True
>       ARMPageInvalidate_Data -> True
>       ARMPageCleanInvalidate_Data -> True
>       ARMPageUnify_Instruction -> True
>       _ -> False


