% Copyright 2014, General Dynamics C4 Systems
%
% This software may be distributed and modified according to the terms of
% the GNU General Public License version 2. Note that NO WARRANTY is provided.
% See "LICENSE_GPLv2.txt" for details.
%
% @TAG(GD_GPL)
%

This module defines the handling of the x64 hardware-defined page tables.

> module SEL4.Kernel.VSpace.X64 where

\begin{impdetails}

> import SEL4.API.Types
> import SEL4.API.Failures
> import SEL4.Machine.RegisterSet
> import SEL4.Machine.RegisterSet.X64 (Register(..))
> import SEL4.Machine.Hardware.X64
> import SEL4.Model
> import SEL4.Object.Structures
> import SEL4.Model.StateData.X64
> import SEL4.API.Invocation
> import SEL4.API.InvocationLabels.X64
> import {-# SOURCE #-} SEL4.Object.CNode
> import {-# SOURCE #-} SEL4.Object.TCB
> import {-# SOURCE #-} SEL4.Kernel.Init
> import {-# SOURCE #-} SEL4.Kernel.CSpace

> import Data.Bits
> import Data.Maybe
> import Data.Array
> import Data.Word (Word32)

\end{impdetails}

The x64-specific invocations are imported with the "ArchInv" prefix. This is necessary to avoid namespace conflicts with the generic invocations.

> import SEL4.API.Invocation.X64 as ArchInv

\subsection{Constants}

All virtual addresses above "pptrUserTop" cannot be mapped by user-level tasks. With the exception of one page, at "globalsBase", they cannot be read; the globals page is mapped read-only.

> kernelBase :: VPtr
> kernelBase = VPtr 0xffffffff80000000

> globalsBase :: VPtr
> globalsBase = VPtr 0xffffc000

The idle thread's code is at an arbitrary location in kernel memory. For convenience in the Haskell model, we place it in the globals frame, but there is no need for it to be in user-accessible memory.

> idleThreadStart :: VPtr
> idleThreadStart = globalsBase + VPtr 0x100

\subsubsection{Creating a New Address Space}

When a new page directory is created, the kernel copies all of the global mappings from the kernel page directory into the new page directory.

> copyGlobalMappings :: PPtr PML4E -> Kernel ()
> copyGlobalMappings newPM = do
>     globalPM <- gets (x64KSGlobalPML4 . ksArchState)
>     let base = getPML4Index pptrBase
>     let pml4eBits = objBits (undefined :: PML4E) -- = 3, size of word
>     let pmSize = 1 `shiftL` ptTranslationBits -- 512 entries in table
>     forM_ [base .. pmSize - 1] $ \index -> do
>         let offset = PPtr index `shiftL` pml4eBits 
>         pml4e <- getObject $ globalPM + offset
>         storePML4E (newPM + offset) pml4e

> createMappingEntries :: PAddr -> VPtr ->
>     VMPageSize -> VMRights -> VMAttributes -> PPtr PML4E ->
>     KernelF SyscallError (VMPageEntry, VMPageEntryPtr)
> createMappingEntries base vptr X64SmallPage vmRights attrib vspace = do
>     p <- lookupErrorOnFailure False $ lookupPTSlot vspace vptr
>     return $ (VMPTE $ SmallPagePTE {
>         pteFrame = base,
>         pteGlobal = False,
>         ptePAT = x64PAT attrib,
>         pteDirty = False,
>         pteAccessed = False,
>         pteCacheDisabled = x64CacheDisabled attrib,
>         pteWriteThrough = x64WriteThrough attrib,
>         pteExecuteDisable = False,
>         pteRights = vmRights }, VMPTEPtr p)
>
> createMappingEntries base vptr X64LargePage vmRights attrib vspace = do
>     p <- lookupErrorOnFailure False $ lookupPDSlot vspace vptr
>     return $ (VMPDE $ LargePagePDE {
>         pdeFrame = base,
>         pdeGlobal = False,
>         pdePAT = x64PAT attrib,
>         pdeDirty = False,
>         pdeAccessed = False,
>         pdeCacheDisabled = x64CacheDisabled attrib,
>         pdeWriteThrough = x64WriteThrough attrib,
>         pdeExecuteDisable = False,
>         pdeRights = vmRights }, VMPDEPtr p)
>
> createMappingEntries base vptr X64HugePage vmRights attrib vspace = do
>     p <- lookupErrorOnFailure False $ lookupPDPTSlot vspace vptr
>     return $ (VMPDPTE $ HugePagePDPTE {
>         pdpteFrame = base,
>         pdpteGlobal = False,
>         pdptePAT = False,
>         pdpteDirty = False,
>         pdpteAccessed = False,
>         pdpteCacheDisabled = x64CacheDisabled attrib,
>         pdpteWriteThrough = x64WriteThrough attrib,
>         pdpteExecuteDisable = False,
>         pdpteRights = vmRights }, VMPDPTEPtr p)

The following function is called before creating or modifying mappings in a page table or page directory, and is responsible for ensuring that the mapping is safe.

> ensureSafeMapping :: (VMPageEntry, VMPageEntryPtr) ->
>     KernelF SyscallError ()
> ensureSafeMapping (VMPTE InvalidPTE, _) = return ()
> ensureSafeMapping (VMPDE InvalidPDE, _) = return ()
> ensureSafeMapping (VMPDPTE InvalidPDPTE, _) = return ()
>
> ensureSafeMapping (VMPTE (SmallPagePTE {}), VMPTEPtr slot) = do
>         pte <- withoutFailure $ getObject slot
>         case pte of
>             InvalidPTE -> return ()
>             _ -> throw DeleteFirst
>
> ensureSafeMapping (VMPDE (LargePagePDE {}), VMPDEPtr slot) = do
>         pde <- withoutFailure $ getObject slot
>         case pde of
>             InvalidPDE -> return ()
>             _ -> throw DeleteFirst
>
> ensureSafeMapping (VMPDPTE (HugePagePDPTE {}), VMPDPTEPtr slot) = do
>         pdpte <- withoutFailure $ getObject slot
>         case pdpte of
>             InvalidPDPTE -> return ()
>             _ -> throw DeleteFirst
>
> ensureSafeMapping _ = fail "This should never happen"

\subsection{Lookups and Faults}

\subsubsection{IPC Buffer Accesses}

When the kernel tries to access a thread's IPC buffer, this function is called to determine whether the buffer exists and to find its physical address.

> -- UNCHANGED FOR X64
> lookupIPCBuffer :: Bool -> PPtr TCB -> Kernel (Maybe (PPtr Word))
> lookupIPCBuffer isReceiver thread = do
>     bufferPtr <- threadGet tcbIPCBuffer thread
>     bufferFrameSlot <- getThreadBufferSlot thread
>     bufferCap <- getSlotCap bufferFrameSlot
>     case bufferCap of
>         ArchObjectCap (frame@PageCap {}) -> do
>             let rights = capVPRights frame
>             let pBits = pageBitsForSize $ capVPSize frame
>             if (rights == VMReadWrite || not isReceiver && rights == VMReadOnly)
>               then do
>                  let ptr = capVPBasePtr frame +
>                            PPtr (fromVPtr bufferPtr .&. mask pBits)
>                  assert (ptr /= 0)
>                             "IPC buffer pointer must be non-null"
>                  return $ Just ptr
>               else return Nothing
>         _ -> return Nothing

\subsubsection{ASID Lookups}

Locating the page directory for a given ASID is necessary when updating or deleting a mapping given its ASID and virtual address.

> findVSpaceForASID :: ASID -> KernelF LookupFailure (PPtr PML4E)
> findVSpaceForASID asid = do
>     assert (asid > 0) "ASID 0 is used for objects that are not mapped"
>     assert (asid <= snd asidRange) "ASID out of range"
>     asidTable <- withoutFailure $ gets (x64KSASIDTable . ksArchState)
>     let poolPtr = asidTable!(asidHighBitsOf asid)
>     ASIDPool pool <- case poolPtr of
>         Just ptr -> withoutFailure $ getObject ptr
>         Nothing -> throw InvalidRoot
>     let pm = pool!(asid .&. mask asidLowBits)
>     case pm of
>         Just ptr -> do
>             assert (ptr /= 0) "findVSpaceForASID: found null PD"
>             return ptr
>         Nothing -> throw InvalidRoot


These checks are too expensive to run in haskell. The first funcion checks that the pointer is to a page directory, which would require testing that each entry of the table is present. The second checks that the page directory appears in x64KSASIDMap only on the ASIDs specified, which would require walking all possible ASIDs to test. In the formalisation of this specification, these functions are given alternative definitions that make the appropriate checks.

> checkPDAt :: PPtr PDE -> Kernel ()
> checkPDAt _ = return ()


> checkPTAt :: PPtr PDE -> Kernel ()
> checkPTAt _ = return ()

> checkPML4ASIDMapMembership :: PPtr PML4E -> [ASID] -> Kernel ()
> checkPML4ASIDMapMembership _ _ = return ()

> checkPML4UniqueToASID :: PPtr PML4E -> ASID -> Kernel ()
> checkPML4UniqueToASID pd asid = checkPML4ASIDMapMembership pd [asid]

> checkPML4NotInASIDMap :: PPtr PML4E -> Kernel ()
> checkPML4NotInASIDMap pd = checkPML4ASIDMapMembership pd []

\subsubsection{Locating Page Table and Page Directory Slots}

The "lookupPTSlot" function locates the page table slot that maps a given virtual address, and returns a pointer to the slot. It will throw a lookup failure if the required page directory slot does not point to a page table.

> lookupPTSlot :: PPtr PML4E -> VPtr -> KernelF LookupFailure (PPtr PTE)
> lookupPTSlot pm vptr = do
>     pdSlot <- lookupPDSlot pm vptr
>     pde <- withoutFailure $ getObject pdSlot
>     case pde of
>         PageTablePDE {} -> do
>             let pt = ptrFromPAddr $ pdeTable pde
>             let ptIndex = getPTIndex vptr 
>             let ptSlot = pt + (PPtr $ ptIndex `shiftL` 3) -- ptr arithmetic, 8 byte words
>             return ptSlot
>         _ -> throw $ MissingCapability (pageBits + ptBits)

> lookupPDSlot :: PPtr PML4E -> VPtr -> KernelF LookupFailure (PPtr PDE)
> lookupPDSlot pm vptr = do
>     pdptSlot <- lookupPDPTSlot pm vptr
>     pdpte <- withoutFailure $ getObject pdptSlot
>     case pdpte of
>         PageDirectoryPDPTE {} -> do
>             let pd = ptrFromPAddr $ pdpteTable pdpte
>             let pdIndex = getPDIndex vptr
>             let pdSlot = pd + (PPtr $ pdIndex `shiftL` 3) -- FIXME x64: word_size_bits 
>             return pdSlot
>         _ -> throw $ MissingCapability (pageBits + ptBits)

> lookupPDPTSlot :: PPtr PML4E -> VPtr -> KernelF LookupFailure (PPtr PDPTE)
> lookupPDPTSlot pm vptr = do
>     let pml4Slot = lookupPML4Slot pm vptr
>     pml4e <- withoutFailure $ getObject pml4Slot
>     case pml4e of
>         PDPointerTablePML4E {} -> do
>             let pdpt = ptrFromPAddr $ pml4eTable pml4e
>             let pdptIndex = getPML4Index vptr 
>             let pdptSlot = pdpt + (PPtr $ pdptIndex `shiftL` 3) -- FIXME x64: word_size_bits 
>             return pdptSlot
>         _ -> throw $ MissingCapability (pageBits + ptBits)

Similarly, "lookupPDSlot" locates a slot in the top-level page directory. However, it does not access the kernel state and never throws a fault, so it is not in the kernel monad.

> lookupPML4Slot :: PPtr PML4E -> VPtr -> PPtr PML4E
> lookupPML4Slot pm vptr =
>     let pmIndex = getPML4Index vptr
>     in pm + (PPtr $ pmIndex `shiftL` 3)

\subsubsection{Handling Faults}

If the kernel receives a VM fault from the CPU, it must determine the address and cause of the fault and then throw it to the user-level fault handler. The C datastructure to sture the cause of the fault has only 12 bits space, hence the mask. Only the lower bits are significant anyway.

> handleVMFault :: PPtr TCB -> VMFaultType -> KernelF Fault ()
> handleVMFault thread f = do
>     addr <- withoutFailure $ doMachineOp getFaultAddress
>     fault <- withoutFailure $ asUser thread $ getRegister (Register ErrorRegister)
>     case f of
>         X64DataFault -> throw $ VMFault addr [0, fault .&. mask 5] -- FSR is 5 bits in x64
>         X64InstructionFault -> throw $ VMFault addr [1, fault .&. mask 5]

\subsection{Unmapping and Deletion}

When a capability backing a virtual memory mapping is deleted, or when an explicit request is made to remove a mapping, the kernel must locate the corresponding entries in the page table or ASID table and remove them. It is also necessary to flush the removed mappings from the hardware caches.

\subsubsection{Deleting an ASID Pool}

> deleteASIDPool :: ASID -> PPtr ASIDPool -> Kernel ()
> deleteASIDPool base ptr = do
>     assert (base .&. mask asidLowBits == 0)
>         "ASID pool's base must be aligned"
>     asidTable <- gets (x64KSASIDTable . ksArchState)
>     when (asidTable!(asidHighBitsOf base) == Just ptr) $ do
>         let asidTable' = asidTable//[(asidHighBitsOf base, Nothing)]
>         modify (\s -> s {
>             ksArchState = (ksArchState s) { x64KSASIDTable = asidTable' }})
>         tcb <- getCurThread
>         setVMRoot tcb

\subsubsection{Deleting an Address Space}

> asidInvalidate :: ASID -> Kernel ()
> asidInvalidate (ASID asid) = doMachineOp $ hwASIDInvalidate asid

> deleteASID :: ASID -> PPtr PML4E -> Kernel ()
> deleteASID asid pm = do
>     asidTable <- gets (x64KSASIDTable . ksArchState)
>     asidInvalidate asid
>     case asidTable!(asidHighBitsOf asid) of
>         Nothing -> return ()
>         Just poolPtr -> do
>             ASIDPool pool <- getObject poolPtr
>             when (pool!(asid .&. mask asidLowBits) == Just pm) $ do
>                 let pool' = pool//[(asid .&. mask asidLowBits, Nothing)]
>                 setObject poolPtr $ ASIDPool pool'
>                 tcb <- getCurThread
>                 setVMRoot tcb

\subsubsection{Deleting a PDPT}

> unmapPDPT :: ASID -> VPtr -> PPtr PDPTE -> Kernel ()
> unmapPDPT asid vaddr pdpt = ignoreFailure $ do
>     vspace <- findVSpaceForASID asid
>     let pmSlot = lookupPML4Slot vspace vaddr
>     pml4e <- withoutFailure $ getObject pmSlot
>     case pml4e of
>         PDPointerTablePML4E { pml4eTable = pt' } ->
>             if pt' == addrFromPPtr pdpt then return () else throw InvalidRoot
>         _ -> throw InvalidRoot
>     withoutFailure $ do 
>         flushPDPT vspace vaddr pdpt
>         storePML4E pmSlot InvalidPML4E

\subsubsection{Deleting a Page Directory}

> unmapPageDirectory :: ASID -> VPtr -> PPtr PDE -> Kernel ()
> unmapPageDirectory asid vaddr pd = ignoreFailure $ do
>     vspace <- findVSpaceForASID asid
>     pdptSlot <- lookupPDPTSlot vspace vaddr
>     pdpte <- withoutFailure $ getObject pdptSlot
>     case pdpte of
>         PageDirectoryPDPTE { pdpteTable = pd' } ->
>             if pd' == addrFromPPtr pd then return () else throw InvalidRoot
>         _ -> throw InvalidRoot
>     withoutFailure $ do
>         doMachineOp invalidatePageStructureCache -- FIXME x64: hardware implement
>         storePDPTE pdptSlot InvalidPDPTE

\subsubsection{Deleting a Page Table}

> unmapPageTable :: ASID -> VPtr -> PPtr PTE -> Kernel ()
> unmapPageTable asid vaddr pt = ignoreFailure $ do
>     vspace <- findVSpaceForASID asid
>     pdSlot <- lookupPDSlot vspace vaddr
>     pde <- withoutFailure $ getObject pdSlot
>     case pde of
>         PageTablePDE { pdeTable = pt' } ->
>             if pt' == addrFromPPtr pt then return () else throw InvalidRoot
>         _ -> throw InvalidRoot -- FIXME x64: dummy throw
>     withoutFailure $ do 
>         flushTable vspace vaddr pt
>         storePDE pdSlot InvalidPDE
>         doMachineOp invalidatePageStructureCache -- FIXME x64: hardware implement


\subsubsection{Unmapping a Frame}

> unmapPage :: VMPageSize -> ASID -> VPtr -> PPtr Word -> Kernel ()
> unmapPage size asid vptr ptr = ignoreFailure $ do
>     vspace <- findVSpaceForASID asid
>     case size of
>         X64SmallPage -> do
>             p <- lookupPTSlot vspace vptr
>             pte <- withoutFailure $ getObject p
>             checkMappingPPtr ptr (VMPTE pte)
>             withoutFailure $ storePTE p InvalidPTE
>         X64LargePage -> do
>             p <- lookupPDSlot vspace vptr
>             pde <- withoutFailure $ getObject p
>             checkMappingPPtr ptr (VMPDE pde)
>             withoutFailure $ storePDE p InvalidPDE
>         X64HugePage -> do
>             p <- lookupPDPTSlot vspace vptr
>             pdpte <- withoutFailure $ getObject p
>             checkMappingPPtr ptr (VMPDPTE pdpte)
>             withoutFailure $ storePDPTE p InvalidPDPTE
>     withoutFailure $ do
>         tcb <- getCurThread
>         threadRootSlot <- getThreadVSpaceRoot tcb
>         threadRoot <- getSlotCap threadRootSlot
>         case threadRoot of
>             ArchObjectCap (PML4Cap { capPML4BasePtr = ptr', capPML4MappedASID = Just _ }) 
>                                -> when (ptr' == vspace) $ doMachineOp $ invalidateTLBEntry vptr
>             _ -> return ()

This helper function checks that the mapping installed at a given PT or PD slot points at the given physical address. If that is not the case, the mapping being unmapped has already been displaced, and the unmap need not be performed.

> checkMappingPPtr :: PPtr Word -> VMPageEntry -> KernelF LookupFailure ()
> checkMappingPPtr pptr (VMPTE pte) =
>     case pte of
>         SmallPagePTE { pteFrame = base } ->
>             unless (base == addrFromPPtr pptr) $ throw InvalidRoot
>         _ -> throw InvalidRoot
> checkMappingPPtr pptr (VMPDE pde) =
>     case pde of
>         LargePagePDE { pdeFrame = base } ->
>             unless (base == addrFromPPtr pptr) $ throw InvalidRoot
>         _ -> throw InvalidRoot
> checkMappingPPtr pptr (VMPDPTE pdpte) =
>     case pdpte of
>         HugePagePDPTE { pdpteFrame = base } ->
>             unless (base == addrFromPPtr pptr) $ throw InvalidRoot
>         _ -> throw InvalidRoot

\subsection{Address Space Switching}

> setCurrentVSpaceRoot :: PAddr -> ASID -> MachineMonad ()
> setCurrentVSpaceRoot addr (ASID asid) = archSetCurrentVSpaceRoot addr (Word asid) 

> setVMRoot :: PPtr TCB -> Kernel ()
> setVMRoot tcb = do
>     threadRootSlot <- getThreadVSpaceRoot tcb
>     threadRoot <- getSlotCap threadRootSlot
>     catchFailure
>         (case threadRoot of
>             ArchObjectCap (PML4Cap {
>                     capPML4MappedASID = Just asid,
>                     capPML4BasePtr = pd }) -> do
>                 pd' <- findVSpaceForASID asid
>                 when (pd /= pd') $ throw InvalidRoot
>                 withoutFailure $ doMachineOp $ setCurrentVSpaceRoot (addrFromPPtr pd) asid
>             _ -> throw InvalidRoot)
>         (\_ -> do
>             globalPML4 <- gets (x64KSGlobalPML4 . ksArchState)
>             doMachineOp $ setCurrentVSpaceRoot (addrFromKPPtr globalPML4) 0)

\subsection{Helper Functions}


> isValidVTableRoot :: Capability -> Bool
> isValidVTableRoot
>     (ArchObjectCap (PML4Cap { capPML4MappedASID = Just _ })) = True
> isValidVTableRoot _ = False

The location of an IPC buffer is computed using the relevant bits of a VPtr as an offset within a frame.
The IPC buffer frame must be a frame capability, and the buffer must be aligned.

Note that implementations with separate high and low memory regions may also wish to limit valid IPC buffer frames to low memory, so the kernel can access them without extra mappings. This function may also be used to enforce cache colouring restrictions.

> checkValidIPCBuffer :: VPtr -> Capability -> KernelF SyscallError ()
> checkValidIPCBuffer vptr (ArchObjectCap (PageCap {})) = do
>     when (vptr .&. mask 9 /= 0) $ throw AlignmentError
>     return ()
> checkValidIPCBuffer _ _ = throw IllegalOperation

> maskVMRights :: VMRights -> CapRights -> VMRights
> maskVMRights r m = case (r, capAllowRead m, capAllowWrite m) of
>     (VMReadOnly, True, _) -> VMReadOnly
>     (VMReadWrite, True, False) -> VMReadOnly
>     (VMReadWrite, True, True) -> VMReadWrite
>     _ -> VMKernelOnly

\subsection{Flushing}

%FIXME x64: needs review

> flushPDPT :: PPtr PML4E -> VPtr -> PPtr PDPTE -> Kernel ()
> flushPDPT _ vptr pdpte = doMachineOp $ resetCR3

%FIXME x64: needs review

> flushPageDirectory :: PPtr PML4E -> VPtr -> PPtr PDE -> Kernel ()
> flushPageDirectory _ vptr pde = doMachineOp $ resetCR3

%FIXME x64: needs review

> flushTable :: PPtr PML4E -> VPtr -> PPtr PTE -> Kernel ()
> flushTable vspace vptr pt = do
>     assert (vptr .&. mask (ptTranslationBits + pageBits) == 0)
>         "vptr must be 1MB aligned"
>     tcb <- getCurThread
>     threadRootSlot <- getThreadVSpaceRoot tcb
>     threadRoot <- getSlotCap threadRootSlot
>     case threadRoot of
>         ArchObjectCap (PML4Cap {
>               capPML4MappedASID = Just _,
>               capPML4BasePtr = vspace'}) ->
>             when (vspace == vspace') $ do 
>                 let pteBits = objBits (undefined :: PTE)
>                 let ptSize = 1 `shiftL` ptTranslationBits
>                 forM_ [0 .. ptSize - 1] $ \index -> do
>                     let offset = PPtr index `shiftL` pteBits
>                     pte <- getObject $ pt + offset
>                     case pte of
>                         InvalidPTE -> return ()
>                         _ -> let index' = index `shiftL` pageBits
>                              in doMachineOp $ invalidateTLBEntry $ 
>                                          VPtr $ (fromVPtr vptr) + index'
>         _ -> return ()

\subsection{Decoding x64 Invocations}

> attribsFromWord :: Word -> VMAttributes
> attribsFromWord w = VMAttributes {
>     x64WriteThrough = w `testBit` 0,
>     x64PAT = w `testBit` 2,
>     x64CacheDisabled = w `testBit` 1 }

> pageBase :: VPtr -> VMPageSize -> VPtr
> pageBase vaddr size = vaddr .&. (complement $ mask (pageBitsForSize size))


> decodeX64FrameInvocation :: Word -> [Word] -> PPtr CTE -> 
>                    ArchCapability -> [(Capability, PPtr CTE)] ->
>                    KernelF SyscallError ArchInv.Invocation
> decodeX64FrameInvocation label args cte (cap@PageCap {}) extraCaps = 
>     case (invocationType label, args, extraCaps) of
>         (ArchInvocationLabel X64PageMap, vaddr:rightsMask:attr:_, (vspaceCap,_):_) -> do
>             when (isJust $ capVPMappedAddress cap) $ 
>                 throw $ InvalidCapability 0
>             (vspace,asid) <- case vspaceCap of
>                 ArchObjectCap (PML4Cap {
>                         capPML4MappedASID = Just asid,
>                         capPML4BasePtr = vspace })
>                     -> return (vspace, asid)
>                 _ -> throw $ InvalidCapability 1 
>             vspaceCheck <- lookupErrorOnFailure False $ findVSpaceForASID asid
>             when (vspaceCheck /= vspace) $ throw $ InvalidCapability 1
>             let vtop = vaddr + (bit (pageBitsForSize $ capVPSize cap) - 1)
>             when (VPtr vtop > kernelBase) $
>                 throw $ InvalidArgument 0
>             let vmRights = maskVMRights (capVPRights cap) $
>                     rightsFromWord rightsMask 
>             checkVPAlignment (capVPSize cap) (VPtr vaddr)
>             entries <- createMappingEntries (addrFromPPtr $ capVPBasePtr cap)
>                 (VPtr vaddr) (capVPSize cap) vmRights
>                 (attribsFromWord attr) vspace
>             ensureSafeMapping entries
>             return $ InvokePage $ PageMap {
>                 pageMapASID = asid,
>                 pageMapCap = ArchObjectCap $ cap { capVPMappedAddress = Just (asid, VPtr vaddr) },
>                 pageMapCTSlot = cte,
>                 pageMapEntries = entries }
>         (ArchInvocationLabel X64PageMap, _, _) -> throw TruncatedMessage        
>         (ArchInvocationLabel X64PageRemap, rightsMask:attr:_, (vspaceCap,_):_) -> do
>             when (capVPMapType cap == VMIOSpaceMap) $ throw IllegalOperation
>             (vspace,asid) <- case vspaceCap of
>                 ArchObjectCap (PML4Cap {
>                         capPML4MappedASID = Just asid,
>                         capPML4BasePtr = vspace })
>                     -> return (vspace,asid)
>                 _ -> throw $ InvalidCapability 1
>             vspaceCheck <- lookupErrorOnFailure False $ findVSpaceForASID asid
>             when (vspaceCheck /= vspace) $ throw $ InvalidCapability 1
>             vaddr <- case capVPMappedAddress cap of
>                 Just (_, v) -> return v
>                 _ -> throw $ InvalidCapability 0
>             -- asidCheck not required because ASIDs and HWASIDs are the same on x86
>             let vmRights = maskVMRights (capVPRights cap) $
>                     rightsFromWord rightsMask
>             checkVPAlignment (capVPSize cap) vaddr
>             entries <- createMappingEntries (addrFromPPtr $ capVPBasePtr cap)
>                 vaddr (capVPSize cap) vmRights (attribsFromWord attr) vspace
>             -- x64 allows arbitrary remapping, so no need to call ensureSafeMapping
>             return $ InvokePage $ PageRemap {
>                 pageRemapEntries = entries }
>         (ArchInvocationLabel X64PageRemap, _, _) -> throw TruncatedMessage
>         (ArchInvocationLabel X64PageUnmap, _, _) -> case capVPMapType cap of
>             VMIOSpaceMap -> decodeX64IOUnmapInvocation label args cte cap extraCaps
>             _ -> return $ InvokePage $ PageUnmap {
>                 pageUnmapCap = cap,
>                 pageUnmapCapSlot = cte }  
>         (ArchInvocationLabel X64PageMapIO, _, _) -> decodeX64IOMapInvocation label args cte cap extraCaps
>         (ArchInvocationLabel X64PageGetAddress, _, _) -> return $ InvokePage $ PageGetAddr (capVPBasePtr cap)
>         _ -> throw IllegalOperation
> decodeX64FrameInvocation _ _ _ _ _ = fail "Unreachable"


> decodeX64IOMapInvocation :: Word -> [Word] -> PPtr CTE -> 
>                    ArchCapability -> [(Capability, PPtr CTE)] ->
>                    KernelF SyscallError ArchInv.Invocation
> decodeX64IOMapInvocation label args cte cap extraCaps = error "Not implemented"

> decodeX64IOUnmapInvocation :: Word -> [Word] -> PPtr CTE -> 
>                    ArchCapability -> [(Capability, PPtr CTE)] ->
>                    KernelF SyscallError ArchInv.Invocation
> decodeX64IOUnmapInvocation label args cte cap extraCaps = error "Not implemented"


> decodeX64PDPointerTableInvocation :: Word -> [Word] -> PPtr CTE ->
>         ArchCapability -> [(Capability, PPtr CTE)] ->
>         KernelF SyscallError ArchInv.Invocation

> decodeX64PDPointerTableInvocation label args cte cap@(PDPointerTableCap {}) extraCaps = do
>     case (invocationType label, args, extraCaps) of
>         (ArchInvocationLabel X64PDPTMap, vaddr':attr:_, (vspaceCap,_):_) -> do
>             when (isJust $ capPDPTMappedAddress cap) $
>                 throw $ InvalidCapability 0
>             (vspace,asid) <- case vspaceCap of
>                 ArchObjectCap (PML4Cap {
>                          capPML4MappedASID = Just asid,
>                          capPML4BasePtr = vspace })
>                     -> return (vspace,asid)
>                 _ -> throw $ InvalidCapability 1
>             let shiftBits = pageBits + ptTranslationBits + ptTranslationBits + ptTranslationBits
>             let vaddr = vaddr' .&. complement (mask shiftBits)
>             when (VPtr vaddr >= kernelBase ) $
>                 throw $ InvalidArgument 0
>             vspaceCheck <- lookupErrorOnFailure False $ findVSpaceForASID asid
>             when (vspaceCheck /= vspace) $ throw $ InvalidCapability 1
>             let pml4Slot = lookupPML4Slot vspace (VPtr vaddr)
>             oldpml4e <- withoutFailure $ getObject pml4Slot
>             unless (oldpml4e == InvalidPML4E) $ throw DeleteFirst
>             let pml4e = PDPointerTablePML4E {
>                     pml4eTable = addrFromPPtr $ capPTBasePtr cap,
>                     pml4eAccessed = False,
>                     pml4eCacheDisabled = x64CacheDisabled $ attribsFromWord attr,
>                     pml4eWriteThrough = x64WriteThrough $ attribsFromWord attr,
>                     pml4eExecuteDisable = False,
>                     pml4eRights = VMReadWrite }
>             return $ InvokePDPT $ PDPTMap {
>                 pdptMapCap = ArchObjectCap $ cap { capPDPTMappedAddress = Just (asid, (VPtr vaddr)) },
>                 pdptMapCTSlot = cte,
>                 pdptMapPML4E = pml4e,
>                 pdptMapPML4Slot = pml4Slot }
>         (ArchInvocationLabel X64PDPTMap, _, _) -> throw TruncatedMessage
>         (ArchInvocationLabel X64PDPTUnmap, _, _) -> do
>             cteVal <- withoutFailure $ getCTE cte
>             final <- withoutFailure $ isFinalCapability cteVal
>             unless final $ throw RevokeFirst
>             return $ InvokePDPT $ PDPTUnmap {
>                 pdptUnmapCap = cap,
>                 pdptUnmapCapSlot = cte }
>         _ -> throw IllegalOperation
> decodeX64PDPointerTableInvocation _ _ _ _ _ = fail "Unreachable"


> decodeX64PageDirectoryInvocation :: Word -> [Word] -> PPtr CTE ->
>         ArchCapability -> [(Capability, PPtr CTE)] ->
>         KernelF SyscallError ArchInv.Invocation

> decodeX64PageDirectoryInvocation label args cte cap@(PageDirectoryCap {}) extraCaps  = 
>     case (invocationType label, args, extraCaps) of
>         (ArchInvocationLabel X64PageDirectoryMap, vaddr':attr:_, (pml4Cap,_):_) -> do
>             when (isJust $ capPDMappedAddress cap) $
>                 throw $ InvalidCapability 0
>             (pml,asid) <- case pml4Cap of
>                 ArchObjectCap (PML4Cap {
>                          capPML4MappedASID = Just asid,
>                          capPML4BasePtr = pml })
>                     -> return (pml,asid)
>                 _ -> throw $ InvalidCapability 1
>             let shiftBits = pageBits + ptTranslationBits + ptTranslationBits
>             let vaddr = vaddr' .&. complement (mask shiftBits)
>             when (VPtr vaddr >= kernelBase ) $
>                 throw $ InvalidArgument 0
>             pmlCheck <- lookupErrorOnFailure False $ findVSpaceForASID asid
>             when (pmlCheck /= pml) $ throw $ InvalidCapability 1
>             pdptSlot <- lookupErrorOnFailure False $ lookupPDPTSlot pml (VPtr vaddr)
>             oldpde <- withoutFailure $ getObject pdptSlot
>             unless (oldpde == InvalidPDPTE) $ throw DeleteFirst
>             let pdpte = PageDirectoryPDPTE {
>                     pdpteTable = addrFromPPtr $ capPTBasePtr cap,
>                     pdpteAccessed = False,
>                     pdpteCacheDisabled = x64CacheDisabled $ attribsFromWord attr,
>                     pdpteWriteThrough = x64WriteThrough $ attribsFromWord attr,
>                     pdpteExecuteDisable = False,
>                     pdpteRights = VMReadWrite }
>             return $ InvokePageDirectory $ PageDirectoryMap {
>                 pdMapCap = ArchObjectCap $ cap { capPDMappedAddress = Just (asid, (VPtr vaddr)) },
>                 pdMapCTSlot = cte,
>                 pdMapPDPTE = pdpte,
>                 pdMapPDPTSlot = pdptSlot }
>         (ArchInvocationLabel X64PageDirectoryMap, _, _) -> throw TruncatedMessage
>         (ArchInvocationLabel X64PageDirectoryUnmap, _, _) -> do
>             cteVal <- withoutFailure $ getCTE cte
>             final <- withoutFailure $ isFinalCapability cteVal
>             unless final $ throw RevokeFirst
>             return $ InvokePageDirectory $ PageDirectoryUnmap {
>                 pdUnmapCap = cap,
>                 pdUnmapCapSlot = cte }
>         _ -> throw IllegalOperation
> decodeX64PageDirectoryInvocation _ _ _ _ _ = fail "Unreachable"


> decodeX64PageTableInvocation :: Word -> [Word] -> PPtr CTE ->
>         ArchCapability -> [(Capability, PPtr CTE)] ->
>         KernelF SyscallError ArchInv.Invocation

> decodeX64PageTableInvocation label args cte cap@(PageTableCap {}) extraCaps = 
>    case (invocationType label, args, extraCaps) of
>         (ArchInvocationLabel X64PageTableMap, vaddr':attr:_, (pml4Cap,_):_) -> do
>             when (isJust $ capPTMappedAddress cap) $
>                 throw $ InvalidCapability 0
>             (pml,asid) <- case pml4Cap of
>                 ArchObjectCap (PML4Cap {
>                          capPML4MappedASID = Just asid,
>                          capPML4BasePtr = pml })
>                     -> return (pml,asid)
>                 _ -> throw $ InvalidCapability 1
>             let shiftBits = pageBits + ptTranslationBits
>             let vaddr = vaddr' .&. complement (mask shiftBits)
>             when (VPtr vaddr >= kernelBase ) $
>                 throw $ InvalidArgument 0
>             pmlCheck <- lookupErrorOnFailure False $ findVSpaceForASID asid
>             when (pmlCheck /= pml) $ throw $ InvalidCapability 1
>             pdSlot <- lookupErrorOnFailure False $ lookupPDSlot pml (VPtr vaddr)
>             oldpde <- withoutFailure $ getObject pdSlot
>             unless (oldpde == InvalidPDE) $ throw DeleteFirst
>             let pde = PageTablePDE {
>                     pdeTable = addrFromPPtr $ capPTBasePtr cap,
>                     pdeAccessed = False,
>                     pdeCacheDisabled = x64CacheDisabled $ attribsFromWord attr,
>                     pdeWriteThrough = x64WriteThrough $ attribsFromWord attr,
>                     pdeExecuteDisable = False,
>                     pdeRights = VMReadWrite}
>             return $ InvokePageTable $ PageTableMap {
>                 ptMapCap = ArchObjectCap $ cap { capPTMappedAddress = Just (asid, (VPtr vaddr)) },
>                 ptMapCTSlot = cte,
>                 ptMapPDE = pde,
>                 ptMapPDSlot = pdSlot }
>         (ArchInvocationLabel X64PageTableMap, _, _) -> throw TruncatedMessage
>         (ArchInvocationLabel X64PageTableUnmap, _, _) -> do
>             cteVal <- withoutFailure $ getCTE cte
>             final <- withoutFailure $ isFinalCapability cteVal
>             unless final $ throw RevokeFirst
>             return $ InvokePageTable $ PageTableUnmap {
>                 ptUnmapCap = cap,
>                 ptUnmapCapSlot = cte }
>         _ -> throw IllegalOperation
> decodeX64PageTableInvocation _ _ _ _ _ = fail "Unreachable"


> decodeX64ASIDControlInvocation :: Word -> [Word] ->
>         ArchCapability -> [(Capability, PPtr CTE)] ->
>         KernelF SyscallError ArchInv.Invocation

> decodeX64ASIDControlInvocation label args ASIDControlCap extraCaps = 
>     case (invocationType label, args, extraCaps) of
>         (ArchInvocationLabel X64ASIDControlMakePool, index:depth:_,
>                         (untyped,parentSlot):(root,_):_) -> do
>             asidTable <- withoutFailure $ gets (x64KSASIDTable . ksArchState)
>             let free = filter (\(x,y) -> x <= (1 `shiftL` asidHighBits) - 1 && isNothing y) $ assocs asidTable
>             when (null free) $ throw DeleteFirst
>             let base = (fst $ head free) `shiftL` asidLowBits
>             let pool = makeObject :: ASIDPool
>             frame <- case untyped of
>                 UntypedCap {} | capBlockSize untyped == objBits pool -> do
>                     ensureNoChildren parentSlot
>                     return $ capPtr untyped
>                 _ -> throw $ InvalidCapability 1
>             destSlot <- lookupTargetSlot root (CPtr index) (fromIntegral depth)
>             ensureEmptySlot destSlot
>             return $ InvokeASIDControl $ MakePool {
>                 makePoolFrame = frame,
>                 makePoolSlot = destSlot,
>                 makePoolParent = parentSlot,
>                 makePoolBase = base }
>         (ArchInvocationLabel X64ASIDControlMakePool, _, _) -> throw TruncatedMessage
>         _ -> throw IllegalOperation
> decodeX64ASIDControlInvocation _ _ _ _ = fail "Unreachable"


> decodeX64ASIDPoolInvocation :: Word ->
>         ArchCapability -> [(Capability, PPtr CTE)] ->
>         KernelF SyscallError ArchInv.Invocation

> decodeX64ASIDPoolInvocation label cap@(ASIDPoolCap {}) extraCaps =
>     case (invocationType label, extraCaps) of
>         (ArchInvocationLabel X64ASIDPoolAssign, (vspaceCap,vspaceCapSlot):_) ->
>             case vspaceCap of
>                 ArchObjectCap (PML4Cap { capPML4MappedASID = Nothing })
>                   -> do
>                     asidTable <- withoutFailure $ gets (x64KSASIDTable . ksArchState)
>                     let base = capASIDBase cap
>                     let poolPtr = asidTable!(asidHighBitsOf base)
>                     when (isNothing poolPtr) $ throw $ FailedLookup False InvalidRoot
>                     let Just p = poolPtr
>                     when (p /= capASIDPool cap) $ throw $ InvalidCapability 0
>                     ASIDPool pool <- withoutFailure $ getObject $ p
>                     let free = filter (\(x,y) -> x <=  (1 `shiftL` asidLowBits) - 1
>                                                  && x + base /= 0 && isNothing y) $ assocs pool
>                     when (null free) $ throw DeleteFirst
>                     let asid = fst $ head free
>                     return $ InvokeASIDPool $ Assign {
>                         assignASID = asid + base,
>                         assignASIDPool = capASIDPool cap,
>                         assignASIDCTSlot = vspaceCapSlot }
>                 _ -> throw $ InvalidCapability 1
>         (ArchInvocationLabel X64ASIDPoolAssign, _) -> throw TruncatedMessage
>         _ -> throw IllegalOperation
> decodeX64ASIDPoolInvocation _ _ _ = fail "Unreachable"



> decodeX64MMUInvocation :: Word -> [Word] -> CPtr -> PPtr CTE ->
>         ArchCapability -> [(Capability, PPtr CTE)] ->
>         KernelF SyscallError ArchInv.Invocation

> decodeX64MMUInvocation label args _ cte cap@(PageCap {}) extraCaps = 
>  decodeX64FrameInvocation label args cte cap extraCaps
> decodeX64MMUInvocation label args _ cte cap@(PDPointerTableCap {}) extraCaps = 
>  decodeX64PDPointerTableInvocation label args cte cap extraCaps
> decodeX64MMUInvocation label args _ cte cap@(PageDirectoryCap {}) extraCaps = 
>  decodeX64PageDirectoryInvocation label args cte cap extraCaps
> decodeX64MMUInvocation label args _ cte cap@(PageTableCap {}) extraCaps = 
>  decodeX64PageTableInvocation label args cte cap extraCaps
> decodeX64MMUInvocation label args _ _ cap@(ASIDControlCap {}) extraCaps = 
>  decodeX64ASIDControlInvocation label args cap extraCaps
> decodeX64MMUInvocation label _ _ _ cap@(ASIDPoolCap {}) extraCaps = 
>  decodeX64ASIDPoolInvocation label cap extraCaps
> decodeX64MMUInvocation label _ _ _ cap@(IOPageTableCap {}) _ = error "Not implemented"
> decodeX64MMUInvocation label _ _ _ cap@(PML4Cap {}) _ = error "Not implemented"
> decodeX64MMUInvocation _ _ _ _ _ _ = fail "Unreachable"


Checking virtual address for page size dependent alignment:

> checkVPAlignment :: VMPageSize -> VPtr -> KernelF SyscallError ()
>
> checkVPAlignment sz w =
>     unless (w .&. mask (pageBitsForSize sz) == 0) $
>            throw AlignmentError

> checkValidMappingSize :: VMPageSize -> Kernel ()
> checkValidMappingSize _ = return ()

\subsection{Invocation Implementations}

> performX64MMUInvocation :: ArchInv.Invocation -> KernelP [Word]
> performX64MMUInvocation i = withoutPreemption $ do
>     case i of
>         InvokePDPT oper -> performPDPTInvocation oper
>         InvokePageDirectory oper -> performPageDirectoryInvocation oper
>         InvokePageTable oper -> performPageTableInvocation oper
>         InvokeIOPageTable oper -> performIOPageTableInvocation oper
>         InvokePage oper -> performPageInvocation oper
>         InvokeASIDControl oper -> performASIDControlInvocation oper
>         InvokeASIDPool oper -> performASIDPoolInvocation oper
>         _ -> fail "Unreachable"
>     return $ []

> performPDPTInvocation :: PDPTInvocation -> Kernel ()
> performPDPTInvocation (PDPTMap cap ctSlot pml4e pml4Slot) = do
>     updateCap ctSlot cap
>     storePML4E pml4Slot pml4e
>     doMachineOp invalidatePageStructureCache
>
> performPDPTInvocation (PDPTUnmap cap ctSlot) = do
>     case capPDPTMappedAddress cap of
>         Just (asid, vaddr) -> do
>             unmapPDPT asid vaddr (capPDPTBasePtr cap)
>             let ptr = capPDPTBasePtr cap
>             let pdpteBits = objBits InvalidPDPTE
>             let slots = [ptr, ptr + bit pdpteBits .. ptr + bit pdptBits - 1]
>             mapM_ (flip storePDPTE InvalidPDPTE) slots
>         _ -> return ()
>     ArchObjectCap cap <- getSlotCap ctSlot
>     updateCap ctSlot (ArchObjectCap $ cap { capPDPTMappedAddress = Nothing })

> performPageDirectoryInvocation :: PageDirectoryInvocation -> Kernel ()
> performPageDirectoryInvocation (PageDirectoryMap cap ctSlot pdpte pdptSlot) = do
>     updateCap ctSlot cap
>     storePDPTE pdptSlot pdpte
>     doMachineOp invalidatePageStructureCache
>
> performPageDirectoryInvocation (PageDirectoryUnmap cap ctSlot) = do
>     case capPDMappedAddress cap of
>         Just (asid, vaddr) -> do
>             unmapPageDirectory asid vaddr (capPDBasePtr cap)
>             let ptr = capPDBasePtr cap
>             let pdeBits = objBits InvalidPDE
>             let slots = [ptr, ptr + bit pdeBits .. ptr + bit pdBits - 1]
>             mapM_ (flip storePDE InvalidPDE) slots
>         _ -> return ()
>     ArchObjectCap cap <- getSlotCap ctSlot
>     updateCap ctSlot (ArchObjectCap $ cap { capPDMappedAddress = Nothing })


> performPageTableInvocation :: PageTableInvocation -> Kernel ()
> performPageTableInvocation (PageTableMap cap ctSlot pde pdSlot) = do
>     updateCap ctSlot cap
>     storePDE pdSlot pde
>     doMachineOp invalidatePageStructureCache
>
> performPageTableInvocation (PageTableUnmap cap slot) = do
>     case capPTMappedAddress cap of
>         Just (asid, vaddr) -> do
>             unmapPageTable asid vaddr (capPTBasePtr cap)
>             let ptr = capPTBasePtr cap
>             let pteBits = objBits InvalidPTE
>             let slots = [ptr, ptr + bit pteBits .. ptr + bit ptBits - 1]
>             mapM_ (flip storePTE InvalidPTE) slots
>         _ -> return ()
>     ArchObjectCap cap <- getSlotCap slot
>     updateCap slot (ArchObjectCap $ cap { capPTMappedAddress = Nothing })

> performIOPageTableInvocation :: IOPageTableInvocation -> Kernel ()
> performIOPageTableInvocation _ = error "Unimplemented"

> pteCheckIfMapped :: PPtr PTE -> Kernel Bool
> pteCheckIfMapped slot = do
>     pt <- getObject slot
>     return $ pt /= InvalidPTE

> pdeCheckIfMapped :: PPtr PDE -> Kernel Bool
> pdeCheckIfMapped slot = do
>     pd <- getObject slot
>     return $ pd /= InvalidPDE

> performPageInvocation :: PageInvocation -> Kernel ()
> performPageInvocation (PageMap asid cap ctSlot entries) = do
>     updateCap ctSlot cap
>     case entries of
>         (VMPTE pte, VMPTEPtr slot) -> storePTE slot pte
>         (VMPDE pde, VMPDEPtr slot) -> storePDE slot pde
>         (VMPDPTE pdpte, VMPDPTEPtr slot) -> storePDPTE slot pdpte
>         _ -> fail "impossible"
> 
> performPageInvocation (PageRemap entries) = case entries of
>     (VMPTE pte, VMPTEPtr slot) -> storePTE slot pte
>     (VMPDE pde, VMPDEPtr slot) -> storePDE slot pde
>     (VMPDPTE pdpte, VMPDPTEPtr slot) -> storePDPTE slot pdpte
>     _ -> fail "impossible"
> 
> performPageInvocation (PageUnmap cap ctSlot) = do
>     case capVPMappedAddress cap of
>         Just (asid, vaddr) -> unmapPage (capVPSize cap) asid vaddr
>                                     (capVPBasePtr cap)
>         _ -> return ()
>     ArchObjectCap cap <- getSlotCap ctSlot
>     updateCap ctSlot (ArchObjectCap $ 
>                           cap { capVPMappedAddress = Nothing })
> 
> performPageInvocation (PageIOMap asid cap ctSlot entries) = error "Unimplemented"
>
> performPageInvocation (PageGetAddr ptr) = do
>     let paddr = fromPAddr $ addrFromPPtr ptr
>     ct <- getCurThread
>     msgTransferred <- setMRs ct Nothing [paddr]
>     msgInfo <- return $ MI {
>             msgLength = msgTransferred,
>             msgExtraCaps = 0,
>             msgCapsUnwrapped = 0,
>             msgLabel = 0 }
>     setMessageInfo ct msgInfo


> performASIDControlInvocation :: ASIDControlInvocation -> Kernel ()
> performASIDControlInvocation (MakePool frame slot parent base) = do
>     deleteObjects frame pageBits
>     pcap <- getSlotCap parent
>     updateCap parent (pcap {capFreeIndex = maxFreeIndex (capBlockSize pcap) })
>     placeNewObject frame (makeObject :: ASIDPool) 0
>     let poolPtr = PPtr $ fromPPtr frame
>     cteInsert (ArchObjectCap $ ASIDPoolCap poolPtr base) parent slot
>     assert (base .&. mask asidLowBits == 0)
>         "ASID pool's base must be aligned"
>     asidTable <- gets (x64KSASIDTable . ksArchState)
>     let asidTable' = asidTable//[(asidHighBitsOf base, Just poolPtr)]
>     modify (\s -> s {
>         ksArchState = (ksArchState s) { x64KSASIDTable = asidTable' }})


> performASIDPoolInvocation :: ASIDPoolInvocation -> Kernel ()
> performASIDPoolInvocation (Assign asid poolPtr ctSlot) = do
>     oldcap <- getSlotCap ctSlot
>     ASIDPool pool <- getObject poolPtr
>     let ArchObjectCap cap = oldcap
>     updateCap ctSlot (ArchObjectCap $ cap { capPML4MappedASID = Just asid })
>     let pool' = pool//[(asid .&. mask asidLowBits, Just $ capPML4BasePtr cap)]
>     setObject poolPtr $ ASIDPool pool'

\subsection{Simulator Support}

The kernel model's x64 targets use an external simulation of the physical address space for user-level virtual memory, I/O devices and MMU data structures, separate from the "PSpace" which is used for kernel objects. However, "PDE" objects are accessed by the kernel, so they must be stored in both the external physical memory model and the internal "PSpace". To make verification simpler we do the same for "PTE" objects.

> storePML4E :: PPtr PML4E -> PML4E -> Kernel ()
> storePML4E slot pml4e = do
>     setObject slot pml4e
>     doMachineOp $ storeWordVM (PPtr $ fromPPtr slot) $ wordFromPML4E pml4e

> storePDPTE :: PPtr PDPTE -> PDPTE -> Kernel ()
> storePDPTE slot pdpte = do
>     setObject slot pdpte
>     doMachineOp $ storeWordVM (PPtr $ fromPPtr slot) $ wordFromPDPTE pdpte


> storePDE :: PPtr PDE -> PDE -> Kernel ()
> storePDE slot pde = do
>     setObject slot pde
>     doMachineOp $ storeWordVM (PPtr $ fromPPtr slot) $ wordFromPDE pde


> storePTE :: PPtr PTE -> PTE -> Kernel ()
> storePTE slot pte = do
>     setObject slot pte
>     doMachineOp $ storeWordVM (PPtr $ fromPPtr slot) $ wordFromPTE pte

> storeIOPTE :: PPtr IOPTE -> IOPTE -> Kernel ()
> storeIOPTE slot pte = error "Unimplemented"

> deleteIOPageTable :: ArchCapability -> Kernel ()
> deleteIOPageTable (IOPageTableCap {}) = error "Unimplemented"
> deleteIOPageTable _ = error "Not an IOPageTable capability"

> unmapIOPage :: ArchCapability -> Kernel ()
> unmapIOPage (PageCap { capVPMapType = VMIOSpaceMap }) = error "Unimplemented"
> unmapIOPage _ = error "Not an IOPage capability"

> mapKernelWindow :: Kernel ()
> mapKernelWindow = error "Unimplemented . init code"

> activateGlobalVSpace :: Kernel ()
> activateGlobalVSpace = error "Unimplemented . init code"

> createIPCBufferFrame :: Capability -> VPtr -> KernelInit Capability
> createIPCBufferFrame = error "Unimplemented . init code"

> createBIFrame :: Capability -> VPtr -> Word32 -> Word32 -> KernelInit Capability
> createBIFrame = error "Unimplemented . init code"

> createFramesOfRegion :: Capability -> Region -> Bool -> VPtr -> KernelInit () 
> createFramesOfRegion = error "Unimplemented . init code"

> createITPDPTs :: Capability -> VPtr -> VPtr -> KernelInit Capability
> createITPDPTs = error "Unimplemented . init code" 

> writeITPDPTs :: Capability -> Capability -> KernelInit ()
> writeITPDPTs = error "Unimplemented . init code"

> createITASIDPool :: Capability -> KernelInit Capability
> createITASIDPool = error "Unimplemented . init code"

> writeITASIDPool :: Capability -> Capability -> Kernel ()
> writeITASIDPool = error "Unimplemented . init code"

> createDeviceFrames :: Capability -> KernelInit ()
> createDeviceFrames = error "Unimplemented . init code"
