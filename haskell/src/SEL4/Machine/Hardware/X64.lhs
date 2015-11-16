% Copyright 2014, General Dynamics C4 Systems
%
% This software may be distributed and modified according to the terms of
% the GNU General Public License version 2. Note that NO WARRANTY is provided.
% See "LICENSE_GPLv2.txt" for details.
%
% @TAG(GD_GPL)
%

\begin{impdetails}

> {-# LANGUAGE CPP, GeneralizedNewtypeDeriving #-}

\end{impdetails}

This module defines the low-level ARM hardware interface.

> module SEL4.Machine.Hardware.X64 where

\begin{impdetails}

> import SEL4.Machine.RegisterSet

> import Foreign.Ptr
> import Control.Monad.Reader
> import Data.Bits
> import Data.Word(Word8)
> import Data.Ix

\end{impdetails}

The x86-64-specific register set definitions are qualified with the "X64" prefix, and the platform-specific hardware access functions are qualified with the "Platform" prefix. The latter module is outside the scope of the reference manual; for the executable model, it is specific to the external simulator used for user-level code.

> import qualified SEL4.Machine.RegisterSet.X64 as X64
> import qualified SEL4.Machine.Hardware.X64.PLATFORM as Platform

\subsection{Data Types}

The machine monad contains a platform-specific opaque pointer, used by the external simulator interface.

> type MachineMonad = ReaderT MachineData IO

> type MachineData = Ptr Platform.CallbackData

> type IRQ = Platform.IRQ

FIXME there is a 1-to-1 correspondence between hardware and software ASIDs on x64

> newtype HardwareASID = HardwareASID { fromHWASID :: Word8 }
>     deriving (Num, Enum, Bounded, Ord, Ix, Eq, Show)

> toPAddr = Platform.PAddr

\subsubsection{Virtual Memory}

x86-64 hardware-defined pages come in three sizes: 4k, 2M, 1G.

> data VMPageSize
>     = X64SmallPage
>     | X64LargePage
>     | X64HugePage
>     deriving (Show, Eq, Ord, Enum, Bounded)

x86 virtual memory faults are handled by one of two trap handlers: one for data faults, and one for instruction faults.

> data VMFaultType
>     = X64DataFault
>     | X64InstructionFault
>     deriving Show

\subsubsection{Physical Memory}

The MMU does not allow access to physical addresses while translation is enabled; the kernel must access its objects via virtual addresses. Depending on the platform, these virtual addresses may either be the same as the physical addresses, or offset by a constant.

> type PAddr = Platform.PAddr

> ptrFromPAddr :: PAddr -> PPtr a
> ptrFromPAddr = Platform.ptrFromPAddr

> addrFromPPtr :: PPtr a -> PAddr
> addrFromPPtr = Platform.addrFromPPtr

> fromPAddr :: PAddr -> Word
> fromPAddr = Platform.fromPAddr

\subsection{Hardware Access}

The following functions define the x86 64bit specific interface between the kernel and the hardware. Most of them depend on the simulator in use, and are therefore defined in the platform module.

> pageBits :: Int
> pageBits = 12

> pageBitsForSize :: VMPageSize -> Int
> pageBitsForSize X64SmallPage = 12
> pageBitsForSize X64LargePage = 21
> pageBitsForSize X64HugePage = 30

> getMemoryRegions :: MachineMonad [(PAddr, PAddr)]
> getMemoryRegions = do
>     cpbtr <- ask
>     liftIO $ Platform.getMemoryRegions cpbtr

> getDeviceRegions :: MachineMonad [(PAddr, PAddr)]
> getDeviceRegions = do
>     cbptr <- ask
>     liftIO $ Platform.getDeviceRegions cbptr

> getKernelDevices :: MachineMonad [(PAddr, PPtr Word)]
> getKernelDevices = do
>     cbptr <- ask
>     liftIO $ Platform.getKernelDevices cbptr

> loadWord :: PPtr Word -> MachineMonad Word
> loadWord ptr = do
>     cbptr <- ask
>     liftIO $ Platform.loadWordCallback cbptr $ addrFromPPtr ptr

> storeWord :: PPtr Word -> Word -> MachineMonad ()
> storeWord ptr val = do
>     cbptr <- ask
>     liftIO $ Platform.storeWordCallback cbptr (addrFromPPtr ptr) val

> storeWordVM :: PPtr Word -> Word -> MachineMonad ()
> storeWordVM ptr val = storeWord ptr val

FIXME: Not on x64

> pageColourBits :: Int
> pageColourBits = Platform.pageColourBits

> getActiveIRQ :: MachineMonad (Maybe IRQ)
> getActiveIRQ = do
>     cbptr <- ask
>     liftIO $ Platform.getActiveIRQ cbptr

> ackInterrupt :: IRQ -> MachineMonad ()
> ackInterrupt irq = do
>     cbptr <- ask
>     liftIO $ Platform.ackInterrupt cbptr irq

> maskInterrupt :: Bool -> IRQ -> MachineMonad ()
> maskInterrupt maskI irq = do
>     cbptr <- ask
>     liftIO $ Platform.maskInterrupt cbptr maskI irq

FIXME: IOAPIC: set\_mode\_config and map\_pin\_to\_vector equivalents?

> setInterruptMode :: IRQ -> Bool -> Bool -> MachineMonad ()
> setInterruptMode _ _ _ = return ()

> configureTimer :: MachineMonad IRQ
> configureTimer = do
>     cbptr <- ask
>     liftIO $ Platform.configureTimer cbptr

> resetTimer :: MachineMonad ()
> resetTimer = do
>     cbptr <- ask
>     liftIO $ Platform.resetTimer cbptr

> debugPrint :: String -> MachineMonad ()
> debugPrint str = liftIO $ putStrLn str

> getRestartPC = getRegister (Register X64.FaultInstruction)
> setNextPC = setRegister (Register X64.NextIP)

\subsection{Memory Management}

There are several operations used by the memory management code to access relevant hardware registers.

\subsubsection{Cleaning Memory}

This function is called before a region of user-memory is recycled.
It zeros every word to ensure that user tasks cannot access any private data
that might previously have been stored in the region.

X64: FIXME then flushes the kernel's mapping from the virtually-indexed caches?

> clearMemory :: PPtr Word -> Int -> MachineMonad ()
> clearMemory ptr byteLength = do
>     let wordSize = fromIntegral $ finiteBitSize (undefined::Word) `div` 8
>     let ptr' = PPtr $ fromPPtr ptr
>     let ptrs = [ptr', ptr' + wordSize .. ptr' + fromIntegral byteLength - 1]
>     mapM_ (\p -> storeWord p 0) ptrs

This function is called before a region of memory is made user-accessible.
Though in Haskell, it is implemented as "clearMemory",
we draw the logical distinction to gain more freedom for its interpretation
in the Isabelle formalization.

> initMemory :: PPtr Word -> Int -> MachineMonad ()
> initMemory = clearMemory

This function is called to free a region of user-memory after use.
In Haskell, this operation does not do anything.
We just use it as a stub for the Isabelle formalization.

> freeMemory :: PPtr Word -> Int -> MachineMonad ()
> freeMemory _ _ = return ()

Same as "clearMemory", but uses storeWordVM to write to memory.
To be used when creating mapping objects (page tables and -dirs)
Flushing the kernel's mapping from the virtually-indexed
caches must be done separately.

> clearMemoryVM :: PPtr Word -> Int -> MachineMonad ()
> clearMemoryVM ptr bits = do
>     let wordSize = fromIntegral $ finiteBitSize (undefined::Word) `div` 8
>     let ptr' = PPtr $ fromPPtr ptr
>     let ptrs = [ptr', ptr' + wordSize .. ptr' + 1 `shiftL` bits - 1]
>     mapM_ (\p -> storeWordVM p 0) ptrs

\subsubsection{Address Space Setup}

FIXME: what does this do on x64?

> setCurrentPD :: PAddr -> MachineMonad ()
> setCurrentPD pd = do
>     dsb
>     writeTTBR0 pd
>     isb

> setHardwareASID :: HardwareASID -> MachineMonad ()
> setHardwareASID (HardwareASID hw_asid) = do
>     cbptr <- ask
>     liftIO $ Platform.setHardwareASID cbptr hw_asid

\subsubsection{Memory Barriers}

FIXME: does this have to be called dsb?

> mfence :: MachineMonad ()
> mfence = do
>     cbptr <- ask
>     liftIO $ Platform.mfenceCallback cbptr

\subsubsection{Cache Cleaning and TLB Flushes}

> invalidateTLB :: MachineMonad ()
> invalidateTLB = do
>     cbptr <- ask
>     liftIO $ Platform.invalidateTLBCallback cbptr

> invalidateTLB_ASID :: HardwareASID -> MachineMonad ()
> invalidateTLB_ASID (HardwareASID hw_asid) = do
>     cbptr <- ask
>     liftIO $ Platform.invalidateTLB_ASIDCallback cbptr hw_asid

This function is used to clear the load exclusive monitor. This dummy
implementation assumes the monitor is not modelled in our simulator.

> clearExMonitor :: MachineMonad ()
> clearExMonitor = return ()

\subsubsection{Fault Status Registers}

FIXME: x64 has anything like this?

\subsubsection{Page Table Structure}

> data PML4E
>     = InvalidPML4E
>     | PDPointerTablePML4E {
>         pml4Table :: PAddr,
>         pml4Accessed :: Bool,
>         pml4CacheDisabled :: Bool,
>         pml4WriteThrough :: Bool,
>         pml4SuperUser :: Bool,
>         pml4ReadWrite :: Bool,
>         pml4ExecuteDisable :: Bool }
>     deriving (Show, Eq)

> data PDPTE
>     = InvalidPDPTE
>     | PageDirectoryPDPTE {
>         pdptTable :: PAddr,
>         pdptAccessed :: Bool,
>         pdptCacheDisabled :: Bool,
>         pdptWriteThrough :: Bool,
>         pdptSuperUser :: Bool,
>         pdptReadWrite :: Bool,
>         pdptExecuteDisable :: Bool }
>     | HugePagePDPTE {
>         pdptFrame :: PAddr,
>         pdptGlobal :: Bool,
>         pdptDirty :: Bool,
>         pdptAccessed :: Bool,
>         pdptCacheDisabled :: Bool,
>         pdptWriteThrough :: Bool,
>         pdptSuperUser :: Bool,
>         pdptReadWrite :: Bool,
>         pdptExecuteDisable :: Bool }
>     deriving (Show, Eq)

         
The ARM architecture defines a two-level hardware-walked page table. The kernel must write entries to this table in the defined format to construct address spaces for user-level tasks.

The following types are Haskell representations of an entry in an ARMv6 page table. The "PDE" (page directory entry) type is an entry in the first level, and the "PTE" (page table entry) type is an entry in the second level. Note that "SuperSectionPDE" is an extension provided by some ARMv6 cores.

> data PDE
>     = InvalidPDE
>     | PageTablePDE {
>         pdeTable :: PAddr,
>         pdeAccessed :: Bool,
>         pdeCacheDisabled :: Bool,
>         pdeWriteThrough :: Bool,
>         pdeSuperUser :: Bool,
>         pdeReadWrite :: Bool,
>         pdeExecuteDisable :: Bool }
>     | LargePagePDE {
>         pdeFrame :: PAddr,
>         pdeGlobal :: Bool,
>         pdeDirty :: Bool,
>         pdeAccessed :: Bool,
>         pdeCacheDisabled :: Bool,
>         pdeWriteThrough :: Bool,
>         pdeSuperUser :: Bool,
>         pdeReadWrite :: Bool,
>         pdeExecuteDisable :: Bool }
>     deriving (Show, Eq)

> -- FIXME x64
> wordFromPDE :: PDE -> Word
> wordFromPDE InvalidPDE = 0
> wordFromPDE (PageTablePDE table parity domain) = 1 .|.
>     (fromIntegral table .&. 0xfffffc00) .|.
>     (if parity then bit 9 else 0) .|.
>     ((domain .&. 0xf) `shiftL` 5)
> wordFromPDE (SectionPDE frame parity domain cacheable global xn rights) = 2 .|.
>     (fromIntegral frame .&. 0xfff00000) .|.
>     (if parity then bit 9 else 0) .|.
>     (if cacheable then bit 2 .|. bit 3 else 0) .|.
>     (if xn then bit 4 else 0) .|.
>     ((domain .&. 0xf) `shiftL` 5) .|.
>     (if global then 0 else bit 17) .|.
>     (fromIntegral $ fromEnum rights `shiftL` 10)
> wordFromPDE (SuperSectionPDE frame parity cacheable global xn rights) = 2 .|.
>     bit 18 .|.
>     (fromIntegral frame .&. 0xff000000) .|.
>     (if parity then bit 9 else 0) .|.
>     (if cacheable then bit 2 .|. bit 3 else 0) .|.
>     (if xn then bit 4 else 0) .|.
>     (if global then 0 else bit 17) .|.
>     (fromIntegral $ fromEnum rights `shiftL` 10)

> data PTE
>     = InvalidPTE
>     | SmallPagePTE {
>         pteFrame :: PAddr,
>         pteGlobal :: Bool,
>         pteDirty :: Bool,
>         pteAccessed :: Bool,
>         pteCacheDisabled :: Bool,
>         pteWriteThrough :: Bool,
>         pteSuperUser :: Bool,
>         pteReadWrite :: Bool,
>         pteExecuteDisable :: Bool }
>     deriving (Show, Eq)

> -- FIXME x64
> wordFromPTE :: PTE -> Word
> wordFromPTE InvalidPTE = 0
> wordFromPTE (LargePagePTE frame cacheable global xn rights) = 1 .|.
>     (fromIntegral frame .&. 0xffff0000) .|.
>     (if cacheable then bit 2 .|. bit 3 else 0) .|.
>     (if global then 0 else bit 11) .|.
>     (if xn then bit 15 else 0) .|.
>     (fromIntegral $ fromEnum rights `shiftL` 4)
> wordFromPTE (SmallPagePTE frame cacheable global xn rights) = 2 .|.
>     (fromIntegral frame .&. 0xfffff000) .|.
>     (if xn then 1 else 0) .|.
>     (if cacheable then bit 2 .|. bit 3 else 0) .|.
>     (if global then 0 else bit 11) .|.
>     (fromIntegral $ fromEnum rights `shiftL` 4)

> -- FIXME x64: what do these change to, how do we amalgamate
> data VMRights
>     = VMNoAccess
>     | VMKernelOnly
>     | VMReadOnly
>     | VMReadWrite
>     deriving (Show, Eq, Enum)

> data VMAttributes = VMAttributes {
>     armPageCacheable, armParityEnabled, armExecuteNever :: Bool }

All tables in x86 64bit do 9 bits of translation, with eight bytes per entry.
Every table is one small page in size.

> ptTranslationBits :: Int
> ptTranslationBits = 9

> ptBits :: Int
> ptBits = ptTranslationBits + 3

> cacheLineBits = Platform.cacheLineBits
> cacheLine = Platform.cacheLine


