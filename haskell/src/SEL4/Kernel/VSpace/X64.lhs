% FIXME: Clagged from ARM
% Copyright 2014, General Dynamics C4 Systems
%
% This software may be distributed and modified according to the terms of
% the GNU General Public License version 2. Note that NO WARRANTY is provided.
% See "LICENSE_GPLv2.txt" for details.
%
% @TAG(GD_GPL)
%

This module defines the handling of the ARM hardware-defined page tables.

> module SEL4.Kernel.VSpace.X64 where

\begin{impdetails}

> import SEL4.API.Types
> import SEL4.API.Failures
> import SEL4.Machine.RegisterSet
> import SEL4.Machine.Hardware.X64
> import SEL4.Model
> import SEL4.Object.Structures
> import SEL4.Model.StateData.X64
> import SEL4.Object.Instances
> import SEL4.API.Invocation
> import SEL4.API.InvocationLabels.X64
> import SEL4.Kernel.BootInfo
> import {-# SOURCE #-} SEL4.Object.CNode
> import {-# SOURCE #-} SEL4.Object.TCB
> import {-# SOURCE #-} SEL4.Kernel.Init
> import {-# SOURCE #-} SEL4.Kernel.CSpace

> import Data.Bits
> import Data.Maybe
> import Data.List
> import Data.Array
> import Data.Word (Word32)

\end{impdetails}

The ARM-specific invocations are imported with the "ArchInv" prefix. This is necessary to avoid namespace conflicts with the generic invocations.

> import SEL4.API.Invocation.X64 as ArchInv

\subsection{Constants}

All virtual addresses above "kernelBase" cannot be mapped by user-level tasks. With the exception of one page, at "globalsBase", they cannot be read; the globals page is mapped read-only.

> -- FIXME x64: do these exist?
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
>         pteExecuteDisable = False,
>         pteFrame = base,
>         pteGlobal = False,
>         ptePAT = x64PAT attrib,
>         pteDirty = False,
>         pteAccessed = False,
>         pteCacheDisabled = x64CacheDisabled attrib,
>         pteWriteThrough = x64WriteThrough attrib,
>         pteRights = vmRights }, VMPTEPtr p) -- this probably won't type check.
>
> createMappingEntries base vptr X64LargePage vmRights attrib vspace = do
>     p <- lookupErrorOnFailure False $ lookupPDSlot vspace vptr
>     return $ (VMPDE $ LargePagePDE {
>         pdeExecuteDisable = False,
>         pdeFrame = base,
>         pdeGlobal = False,
>         pdePAT = x64PAT attrib,
>         pdeDirty = False,
>         pdeAccessed = False,
>         pdeCacheDisabled = x64CacheDisabled attrib,
>         pdeWriteThrough = x64WriteThrough attrib,
>         pdeRights = vmRights }, VMPDEPtr p) -- this probably won't type check.
>
> createMappingEntries base vptr X64HugePage vmRights attrib vspace = do
>     p <- lookupErrorOnFailure False $ lookupPDPTSlot vspace vptr
>     return $ (VMPDPTE $ HugePagePDPTE {
>         pdpteExecuteDisable = False,
>         pdpteFrame = base,
>         pdpteGlobal = False,
>         pdptePAT = False,
>         pdpteDirty = False,
>         pdpteAccessed = False,
>         pdpteCacheDisabled = x64CacheDisabled attrib,
>         pdpteWriteThrough = x64WriteThrough attrib,
>         pdpteRights = vmRights }, VMPDPTEPtr p) -- this probably won't type check.

The following function is called before creating or modifying mappings in a page table or page directory, and is responsible for ensuring that the mapping is safe --- that is, that inserting it will behave predictably and will not damage the hardware. The ARMv6 specifications require that there are never two mappings of different sizes at any virtual address in the active address space, so this function will throw a fault if the requested operation would change the size of the mapping of any existing valid entry.

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
>     addr <- withoutFailure $ doMachineOp getFaultAddress -- FIXME x64: implement getFaultAddress = read_cr2
>     fault <- withoutFailure $ asUser thread $ getRegister undefined -- FIXME: what is ErrorRegister supposed to mean here?!
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

> deleteASID :: ASID -> PPtr PML4E -> Kernel ()
> deleteASID asid pm = do
>     asidTable <- gets (x64KSASIDTable . ksArchState)
>     hwASIDInvalidate asid --FIXME x64: add to hardware functions
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
>         case threadRoot of -- FIXME x64: don't really know if this is how this should be written
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
The IPC buffer frame must be an ARM frame capability, and the buffer must be aligned.

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

\subsection{ARM Hardware ASID allocation}

X64UPDATE

> flushPDPT :: PPtr PML4E -> VPtr -> PPtr PDPTE -> Kernel ()
> flushPDPT _ vptr pdpte = doMachineOp $ resetCR3

X64UPDATE

> flushPageDirectory :: PPtr PML4E -> VPtr -> PPtr PDE -> Kernel ()
> flushPageDirectory _ vptr pde = doMachineOp $ resetCR3

X64UPDATE

> -- FIXME x64: someone should look at this pile of fail
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

\subsection{Decoding ARM Invocations}

> attribsFromWord :: Word -> VMAttributes
> attribsFromWord w = VMAttributes {
>     x64CacheDisabled = w `testBit` 1,
>     x64PAT = w `testBit` 2,
>     x64WriteThrough = w `testBit` 0 }

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
>             let vtop = vaddr + bit (pageBitsForSize $ capVPSize cap)
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


> decodeX64MMUInvocation :: Word -> [Word] -> CPtr -> PPtr CTE ->
>         ArchCapability -> [(Capability, PPtr CTE)] ->
>         KernelF SyscallError ArchInv.Invocation

> decodeX64MMUInvocation label args _ _ cap@(PDPointerTableCap {}) _ = error "Not implemented"
> decodeX64MMUInvocation label args _ _ cap@(PageDirectoryCap {}) _ = error "Not implemented"
> decodeX64MMUInvocation label args _ cte cap@(PageTableCap {}) _ = error "Not implemented"
> decodeX64MMUInvocation label args _ cte cap@(PageCap {}) extraCaps = decodeX64FrameInvocation label args cte cap extraCaps
> decodeX64MMUInvocation label args _ _ ASIDControlCap extraCaps = error "Not implemented"
> decodeX64MMUInvocation label _ _ _ cap@(ASIDPoolCap {}) _ = error "Not implemented"
> decodeX64MMUInvocation label _ _ _ cap@(IOPageTableCap {}) _ = error "Not implemented"
> decodeX64MMUInvocation label _ _ _ cap@(PML4Cap {}) _ = error "Not implemented"
> decodeX64MMUInvocation _ _ _ _ _ _ = fail "Unreachable"


> decodeX64IOInvocation :: Word -> [Word] -> CPtr -> PPtr CTE ->
>         ArchCapability -> [(Capability, PPtr CTE)] ->
>         KernelF SyscallError ArchInv.Invocation

> decodeX64IOInvocation _ _ _ _ cap@(IOPortCap {}) _ = error "Unimplemented"
> decodeX64IOInvocation _ _ _ _ cap@(IOSpaceCap {}) _ = error "Unimplemented"
> decodeX64IOInvocation _ _ _ _ _ _ = fail "Unreachable"


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
> performPDPTInvocation _ = error "Not implemented"

> performPageDirectoryInvocation :: PageDirectoryInvocation -> Kernel ()
> performPageDirectoryInvocation _ = error "Not implemented"

> performPageTableInvocation :: PageTableInvocation -> Kernel ()
> performPageTableInvocation _ = error "Not implemented"

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
> performASIDControlInvocation _ =
>     error "Not implemented"

> performASIDPoolInvocation :: ASIDPoolInvocation -> Kernel ()
> performASIDPoolInvocation (Assign asid poolPtr ctSlot) =
>     error "Not implemented"

> performX64IOInvocation :: IOPortInvocation -> KernelP [Word]
> performX64IOInvocation _ = error "Unimplemented"

\subsection{Simulator Support}

The kernel model's ARM targets use an external simulation of the physical address space for user-level virtual memory, I/O devices and MMU data structures, separate from the "PSpace" which is used for kernel objects. However, "PDE" objects are accessed by the kernel, so they must be stored in both the external physical memory model and the internal "PSpace". To make verification simpler we do the same for "PTE" objects.

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
> mapKernelWindow = error "Unimplemented -- init code"

> activateGlobalVSpace :: Kernel ()
> activateGlobalVSpace = error "Unimplemented -- init code"

> createIPCBufferFrame :: Capability -> VPtr -> KernelInit Capability
> createIPCBufferFrame = error "Unimplemented -- init code"

> createBIFrame :: Capability -> VPtr -> Word32 -> Word32 -> KernelInit Capability
> createBIFrame = error "Unimplemented -- init code"

> createFramesOfRegion :: Capability -> Region -> Bool -> VPtr -> KernelInit () 
> createFramesOfRegion = error "Unimplemented -- init code"

> createITPDPTs :: Capability -> VPtr -> VPtr -> KernelInit Capability
> createITPDPTs = error "Unimplemented -- init code" 

> writeITPDPTs :: Capability -> Capability -> KernelInit ()
> writeITPDPTs = error "Unimplemented -- init code"

> createITASIDPool :: Capability -> KernelInit Capability
> createITASIDPool = error "Unimplemented -- init code"

> writeITASIDPool :: Capability -> Capability -> Kernel ()
> writeITASIDPool = error "Unimplemented -- init code"

> createDeviceFrames :: Capability -> KernelInit ()
> createDeviceFrames = error "Unimplemented -- init code"
