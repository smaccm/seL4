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
> import Data.Word(Word8, Word16, Word32, Word64)

\end{impdetails}

The x86-64-specific register set definitions are qualified with the "X64" prefix, and the platform-specific hardware access functions are qualified with the "Platform" prefix. The latter module is outside the scope of the reference manual; for the executable model, it is specific to the external simulator used for user-level code.

> import qualified SEL4.Machine.RegisterSet.X64 as X64
> import qualified SEL4.Machine.Hardware.X64.PLATFORM as Platform

\subsection{Data Types}

The machine monad contains a platform-specific opaque pointer, used by the external simulator interface.

> type MachineMonad = ReaderT MachineData IO

> type MachineData = Ptr Platform.CallbackData

> type IRQ = Platform.IRQ

> type CR3 = Platform.CR3

> type IOPort = Word16

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

FIXME pc99 has 2 separate offsets for different kernel windows.
addrFromKPPtr is called in non-boot code exactly once in setVMRoot, so rather
than add an additional pointer type we just give a different translation function

> type PAddr = Platform.PAddr

> ptrFromPAddr :: PAddr -> PPtr a
> ptrFromPAddr = Platform.ptrFromPAddr

> addrFromPPtr :: PPtr a -> PAddr
> addrFromPPtr = Platform.addrFromPPtr

> addrFromKPPtr :: PPtr a -> PAddr
> addrFromKPPtr = Platform.addrFromKPPtr

> fromPAddr :: PAddr -> Word
> fromPAddr = Platform.fromPAddr

\subsection{Hardware Access}

The following functions define the x86 64bit specific interface between the kernel and the hardware. Most of them depend on the simulator in use, and are therefore defined in the platform module.

> pageBits :: Int
> pageBits = 12

All tables in x86 64bit do 9 bits of translation, with eight bytes per entry.
Every table is one small page in size.

> ptTranslationBits :: Int
> ptTranslationBits = 9

> pageBitsForSize :: VMPageSize -> Int
> pageBitsForSize X64SmallPage = pageBits
> pageBitsForSize X64LargePage = pageBits + ptTranslationBits
> pageBitsForSize X64HugePage = pageBits + ptTranslationBits + ptTranslationBits

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

> storeWord :: PPtr Word -> Word -> MachineMonad ()
> storeWord ptr val = do
>     cbptr <- ask
>     liftIO $ Platform.storeWordCallback cbptr (addrFromPPtr ptr) val

> storeWordVM :: PPtr Word -> Word -> MachineMonad ()
> storeWordVM ptr val = storeWord ptr val

> loadWord :: PPtr Word -> MachineMonad Word
> loadWord ptr = do
>     cbptr <- ask
>     liftIO $ Platform.loadWordCallback cbptr $ addrFromPPtr ptr

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

> ptBits :: Int
> ptBits = ptTranslationBits + 3

> pdBits :: Int
> pdBits = ptTranslationBits + 3

> pdptBits :: Int
> pdptBits = ptTranslationBits + 3

> pml4Bits :: Int
> pml4Bits = ptTranslationBits + 3

> ioptBits :: Int
> ioptBits = ptTranslationBits + 3

FIXME: Not on x64

> pageColourBits :: Int
> pageColourBits = Platform.pageColourBits

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
Flushing the kernel's mapping from TLBindexed
caches must be done separately.

> clearMemoryVM :: PPtr Word -> Int -> MachineMonad ()
> clearMemoryVM ptr bits = do
>     let wordSize = fromIntegral $ finiteBitSize (undefined::Word) `div` 8
>     let ptr' = PPtr $ fromPPtr ptr
>     let ptrs = [ptr', ptr' + wordSize .. ptr' + 1 `shiftL` bits - 1]
>     mapM_ (\p -> storeWordVM p 0) ptrs

\subsubsection{Address Space Setup}

> setCurrentCR3 :: CR3 -> MachineMonad ()
> setCurrentCR3 cr3 = Platform.writeCR3 cr3

> getCurrentCR3 :: MachineMonad CR3
> getCurrentCR3 = do
>     cbptr <- ask
>     liftIO $ Platform.readCR3 cbptr

> archSetCurrentVSpaceRoot :: PAddr -> Word -> MachineMonad ()
> archSetCurrentVSpaceRoot pd asid =
>   setCurrentCR3 $ Platform.X64CR3 pd asid

> resetCR3 = error "Unimplemented"

\subsubsection{Memory Barriers}


> mfence :: MachineMonad ()
> mfence = Platform.mfence

\subsubsection{Cache Cleaning and TLB Flushes}

> invalidateTLB :: MachineMonad ()
> invalidateTLB = Platform.invalidateTLB

> invalidateTLBEntry :: VPtr -> MachineMonad ()
> invalidateTLBEntry vptr = Platform.invalidateTLB vptr

> invalidatePageStructureCache :: MachineMonad ()
> invalidatePageStructureCache = invalidateTLBEntry 0

\subsubsection{Fault Status Registers}

FIXME: x64 has anything like this?

\subsubsection{Page Table Structure}

> data PML4E
>     = InvalidPML4E
>     | PDPointerTablePML4E {
>         pml4eTable :: PAddr,
>         pml4eAccessed :: Bool,
>         pml4eCacheDisabled :: Bool,
>         pml4eWriteThrough :: Bool,
>         pml4eExecuteDisable :: Bool,
>         pml4eRights :: VMRights }
>     deriving (Show, Eq)

> wordFromPML4E :: PML4E -> Word
> wordFromPML4E InvalidPML4E = 0
> wordFromPML4E (PDPointerTablePML4E table accessed cd wt xd rights) = 1 .|.
>     (if xd then bit 63 else 0) .|.
>     (fromIntegral table .&. 0x7fffffffff000) .|.
>     (if accessed then bit 5 else 0) .|.
>     (if cd then bit 4 else 0) .|.
>     (if wt then bit 3 else 0) .|.
>     (fromIntegral $ vmRightsToBits rights `shiftL` 1)

> data PDPTE
>     = InvalidPDPTE
>     | PageDirectoryPDPTE {
>         pdpteTable :: PAddr,
>         pdpteAccessed :: Bool,
>         pdpteCacheDisabled :: Bool,
>         pdpteWriteThrough :: Bool,
>         pdpteExecuteDisable :: Bool,
>         pdpteRights :: VMRights }
>     | HugePagePDPTE {
>         pdpteFrame :: PAddr,
>         pdpteGlobal :: Bool,
>         pdptePAT :: Bool,
>         pdpteDirty :: Bool,
>         pdpteAccessed :: Bool,
>         pdpteCacheDisabled :: Bool,
>         pdpteWriteThrough :: Bool,
>         pdpteExecuteDisable :: Bool,
>         pdpteRights :: VMRights }
>     deriving (Show, Eq)

> wordFromPDPTE :: PDPTE -> Word
> wordFromPDPTE InvalidPDPTE = 0
> wordFromPDPTE (PageDirectoryPDPTE table accessed cd wt xd rights) = 1 .|.
>     (if xd then bit 63 else 0) .|.
>     (fromIntegral table .&. 0x7fffffffff000) .|.
>     (if accessed then bit 5 else 0) .|.
>     (if cd then bit 4 else 0) .|.
>     (if wt then bit 3 else 0) .|.
>     (fromIntegral $ vmRightsToBits rights `shiftL` 1)
> wordFromPDPTE (HugePagePDPTE frame global pat dirty accessed cd wt xd rights) = 1 .|. bit 7 .|.
>     (if xd then bit 63 else 0) .|.
>     (fromIntegral frame .&. 0x7ffffc0000000) .|.
>     (if global then bit 8 else 0) .|.
>     (if pat then bit 12 else 0) .|.
>     (if dirty then bit 6 else 0) .|.
>     (if accessed then bit 5 else 0) .|.
>     (if cd then bit 4 else 0) .|.
>     (if wt then bit 3 else 0) .|.
>     (fromIntegral $ vmRightsToBits rights `shiftL` 1)


         
The ARM architecture defines a two-level hardware-walked page table. The kernel must write entries to this table in the defined format to construct address spaces for user-level tasks.

The following types are Haskell representations of an entry in an ARMv6 page table. The "PDE" (page directory entry) type is an entry in the first level, and the "PTE" (page table entry) type is an entry in the second level. Note that "SuperSectionPDE" is an extension provided by some ARMv6 cores.

> -- FIXME x64: rights on tables, not pages?
> data PDE
>     = InvalidPDE
>     | PageTablePDE {
>         pdeTable :: PAddr,
>         pdeAccessed :: Bool,
>         pdeCacheDisabled :: Bool,
>         pdeWriteThrough :: Bool,
>         pdeExecuteDisable :: Bool,
>         pdeRights :: VMRights }
>     | LargePagePDE {
>         pdeFrame :: PAddr,
>         pdeGlobal :: Bool,
>         pdePAT :: Bool,
>         pdeDirty :: Bool,
>         pdeAccessed :: Bool,
>         pdeCacheDisabled :: Bool,
>         pdeWriteThrough :: Bool,
>         pdeExecuteDisable :: Bool,
>         pdeRights :: VMRights }
>     deriving (Show, Eq)

> -- FIXME x64
> wordFromPDE :: PDE -> Word
> wordFromPDE InvalidPDE = 0
> wordFromPDE (PageTablePDE table accessed cd wt xd rights) = 1 .|.
>     (if xd then bit 63 else 0) .|.
>     (fromIntegral table .&. 0x7fffffffff000) .|.
>     (if accessed then bit 5 else 0) .|.
>     (if cd then bit 4 else 0) .|.
>     (if wt then bit 3 else 0) .|.
>     (fromIntegral $ vmRightsToBits rights `shiftL` 1)
> wordFromPDE (LargePagePDE frame global pat dirty accessed cd wt xd rights) = 1 .|. bit 7 .|.
>     (if xd then bit 63 else 0) .|.
>     (fromIntegral frame .&. 0x7ffffffe00000) .|.
>     (if global then bit 8 else 0) .|.
>     (if pat then bit 12 else 0) .|.
>     (if dirty then bit 6 else 0) .|.
>     (if accessed then bit 5 else 0) .|.
>     (if cd then bit 4 else 0) .|.
>     (if wt then bit 3 else 0) .|.
>     (fromIntegral $ vmRightsToBits rights `shiftL` 1)

> data PTE
>     = InvalidPTE
>     | SmallPagePTE {
>         pteFrame :: PAddr,
>         pteGlobal :: Bool,
>         ptePAT :: Bool,
>         pteDirty :: Bool,
>         pteAccessed :: Bool,
>         pteCacheDisabled :: Bool,
>         pteWriteThrough :: Bool,
>         pteExecuteDisable :: Bool,
>         pteRights :: VMRights }
>     deriving (Show, Eq)

FIXME x64: These need to be defined

> newtype IOCTE = IOCTE { iocte :: Word}
>     deriving Show
> data IOPTE = InvalidIOPTE
>     deriving (Show, Eq)

> -- FIXME x64: word size?
> wordFromPTE :: PTE -> Word
> wordFromPTE InvalidPTE = 0
> wordFromPTE (SmallPagePTE frame global pat dirty accessed cd wt xd rights) = 1 .|.
>     (if xd then bit 63 else 0) .|.
>     (fromIntegral frame .&. 0x7fffffffffe000) .|.
>     (if global then bit 8 else 0) .|.
>     (if pat then bit 7 else 0) .|.
>     (if dirty then bit 6 else 0) .|.
>     (if accessed then bit 5 else 0) .|.
>     (if cd then bit 4 else 0) .|.
>     (if wt then bit 3 else 0) .|.
>     (fromIntegral $ vmRightsToBits rights `shiftL` 1)

Pointer Accessor Functions FIXME x64 TYPES

> getPTIndex :: VPtr -> Word
> getPTIndex vptr = 
>     let shiftBits = pageBits 
>     in fromVPtr $ vptr `shiftR` shiftBits .&. mask ptTranslationBits

> getPDIndex :: VPtr -> Word
> getPDIndex vptr = 
>     let shiftBits = pageBits + ptTranslationBits 
>     in fromVPtr $ vptr `shiftR` shiftBits .&. mask ptTranslationBits

> getPDPTIndex :: VPtr -> Word
> getPDPTIndex vptr = 
>     let shiftBits = pageBits + ptTranslationBits + ptTranslationBits
>     in fromVPtr $ vptr `shiftR` shiftBits .&. mask ptTranslationBits


> -- FIXME x64: Check this
> getPML4Index :: VPtr -> Word
> getPML4Index vptr = 
>     let shiftBits = pageBits + ptTranslationBits + ptTranslationBits + ptTranslationBits 
>     in fromVPtr $ vptr `shiftR` shiftBits .&. mask ptTranslationBits

Page entries - could be either PTEs, PDEs or PDPTEs.

> -- FIXME x64: fix this up?
> data VMPageEntry
>     = VMPTE PTE
>     | VMPDE PDE
>     | VMPDPTE PDPTE
>     deriving (Show, Eq)

> data VMPageEntryPtr
>     = VMPTEPtr (PPtr PTE)
>     | VMPDEPtr (PPtr PDE)
>     | VMPDPTEPtr (PPtr PDPTE)
>     deriving (Show, Eq)

> data VMMapType
>     = VMNoMap
>     | VMVSpaceMap
>     | VMIOSpaceMap
>     deriving (Show, Eq, Enum)

> data VMRights
>     = VMKernelOnly
>     | VMReadOnly
>     | VMReadWrite
>     deriving (Show, Eq)

> vmRightsToBits :: VMRights -> Word
> vmRightsToBits VMKernelOnly = 0x1
> vmRightsToBits VMReadOnly = 0x10
> vmRightsToBits VMReadWrite = 0x11

> data VMAttributes = VMAttributes {
>     x64WriteThrough, x64PAT, x64CacheDisabled :: Bool }

> cacheLineBits = Platform.cacheLineBits
> cacheLine = Platform.cacheLine

> pptrBase :: VPtr
> pptrBase = Platform.pptrBase

> -- This firstValidIODomain and numIODomainBits calculated as part of the boot code.
> -- Right now, for simplicity, we assume it is constant
> firstValidIODomain :: Word16
> firstValidIODomain = Platform.firstValidIODomain

> numIODomainIDBits :: Int
> numIODomainIDBits = Platform.numIODomainIDBits

> hwASIDInvalidate :: Word64 -> MachineMonad ()
> hwASIDInvalidate asid = do
>     cbptr <- ask
>     liftIO $ Platform.hwASIDInvalidate cbptr asid


> getFaultAddress :: MachineMonad VPtr
> getFaultAddress = do
>     cbptr <- ask
>     liftIO $ Platform.getFaultAddress cbptr


> -- FIXME x64: IOPTEs

FIXME IO port input/output

> in8 :: MachineMonad Word
> in8 = error "Unimplemented"
> in16 :: MachineMonad Word
> in16 = error "Unimplemented"
> in32 :: MachineMonad Word
> in32 = error "Unimplemented"
> out8 :: IOPort -> Word8 -> MachineMonad ()
> out8 = error "Unimplemented"
> out16 :: IOPort -> Word16 -> MachineMonad ()
> out16 = error "Unimplemented"
> out32 :: IOPort -> Word32 -> MachineMonad ()
> out32 = error "Unimplemented"

