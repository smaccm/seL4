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
> import SEL4.Object.Instances()
> import SEL4.API.Invocation
> import SEL4.Kernel.BootInfo
> import {-# SOURCE #-} SEL4.Object.CNode
> import {-# SOURCE #-} SEL4.Object.TCB
> import {-# SOURCE #-} SEL4.Kernel.Init
> import {-# SOURCE #-} SEL4.Kernel.CSpace

> import Data.Bits
> import Data.Maybe
> import Data.List
> import Data.Array
> import Data.Word

\end{impdetails}

The ARM-specific invocations are imported with the "ArchInv" prefix. This is necessary to avoid namespace conflicts with the generic invocations.

> import SEL4.API.Invocation.X64 as ArchInv
> import SEL4.API.InvocationLabels.X64 as ArchLabels

\subsection{Constants}

All virtual addresses above "kernelBase" cannot be mapped by user-level tasks. With the exception of one page, at "globalsBase", they cannot be read; the globals page is mapped read-only.

> -- FIXME x64: do these exist?
> kernelBase :: VPtr
> kernelBase = VPtr 0xf0000000

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
>     forM_ [fromVPtr base .. pmSize - 1] $ \index -> do
>         let offset = PPtr index `shiftL` pml4eBits 
>         pml4e <- getObject $ globalPM + offset
>         storePML4E (newPM + offset) pml4e

\subsection{Creating and Updating Mappings}

When a frame is being mapped, or an existing mapping updated, the following function is used to locate the page table or page directory slots that will be updated and to construct the entry that will be written into them.

> createMappingEntries :: PAddr -> VPtr ->
>     VMPageSize -> VMRights -> VMAttributes -> PPtr PDE ->
>     KernelF SyscallError (Either (PTE, [PPtr PTE]) (PDE, [PPtr PDE]))
> createMappingEntries base vptr ARMSmallPage vmRights attrib pd = do
>     p <- lookupErrorOnFailure False $ lookupPTSlot pd vptr
>     return $ Left (SmallPagePTE {
>         pteFrame = base,
>         pteCacheable = armPageCacheable attrib,
>         pteGlobal = False,
>         pteExecuteNever = armExecuteNever attrib,
>         pteRights = vmRights }, [p])
>
> createMappingEntries base vptr ARMLargePage vmRights attrib pd = do
>     p <- lookupErrorOnFailure False $ lookupPTSlot pd vptr
>     return $ Left (LargePagePTE {
>         pteFrame = base,
>         pteCacheable = armPageCacheable attrib,
>         pteGlobal = False,
>         pteExecuteNever = armExecuteNever attrib,
>         pteRights = vmRights }, [p, p + 4 .. p + 60])
>
> createMappingEntries base vptr ARMSection vmRights attrib pd = do
>     let p = lookupPDSlot pd vptr
>     return $ Right (SectionPDE {
>         pdeFrame = base,
>         pdeParity = armParityEnabled attrib,
>         pdeDomain = 0,
>         pdeCacheable = armPageCacheable attrib,
>         pdeGlobal = False,
>         pdeExecuteNever = armExecuteNever attrib,
>         pdeRights = vmRights }, [p])
>
> createMappingEntries base vptr ARMSuperSection vmRights attrib pd = do
>     let p = lookupPDSlot pd vptr
>     return $ Right (SuperSectionPDE {
>         pdeFrame = base,
>         pdeParity = armParityEnabled attrib,
>         pdeCacheable = armPageCacheable attrib,
>         pdeGlobal = False,
>         pdeExecuteNever = armExecuteNever attrib,
>         pdeRights = vmRights }, [p, p + 4 .. p + 60])

The following function is called before creating or modifying mappings in a page table or page directory, and is responsible for ensuring that the mapping is safe --- that is, that inserting it will behave predictably and will not damage the hardware. The ARMv6 specifications require that there are never two mappings of different sizes at any virtual address in the active address space, so this function will throw a fault if the requested operation would change the size of the mapping of any existing valid entry.

> ensureSafeMapping :: Either (PTE, [PPtr PTE]) (PDE, [PPtr PDE]) ->
>     KernelF SyscallError ()

> ensureSafeMapping (Left (InvalidPTE, _)) = return ()
>
> ensureSafeMapping (Left (SmallPagePTE {}, ptSlots)) =
>     forM_ ptSlots $ \slot -> do
>         pte <- withoutFailure $ getObject slot
>         case pte of
>             InvalidPTE -> return ()
>             SmallPagePTE {} -> return ()
>             _ -> throw DeleteFirst
>
> ensureSafeMapping (Left (LargePagePTE {}, ptSlots)) =
>     forM_ ptSlots $ \slot -> do
>         pte <- withoutFailure $ getObject slot
>         case pte of
>             InvalidPTE -> return ()
>             LargePagePTE {} -> return ()
>             _ -> throw DeleteFirst

> ensureSafeMapping (Right (InvalidPDE, _)) = return ()
>
> ensureSafeMapping (Right (PageTablePDE {}, _)) =
>     fail "This function is not called when mapping page tables"
>
> ensureSafeMapping (Right (SectionPDE {}, pdSlots)) =
>     forM_ pdSlots $ \slot -> do
>         pde <- withoutFailure $ getObject slot
>         case pde of
>             InvalidPDE -> return ()
>             SectionPDE {} -> return ()
>             _ -> throw DeleteFirst
>
> ensureSafeMapping (Right (SuperSectionPDE {}, pdSlots)) =
>     forM_ pdSlots $ \slot -> do
>         pde <- withoutFailure $ getObject slot
>         case pde of
>             InvalidPDE -> return ()
>             SuperSectionPDE {} -> return ()
>             _ -> throw DeleteFirst

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
>     asidTable <- withoutFailure $ gets (x86KSASIDTable . ksArchState)
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

This version of findPDForASID will fail rather than raise an exception if the ASID does not look up a page directory.

> findVSpaceForASIDAssert :: ASID -> Kernel (PPtr PML4E)
> findVSpaceForASIDAssert asid = do
>     pm <- findVSpaceForASID asid `catchFailure`
>         const (fail "findVSpaceForASIDAssert: pd not found")
>     assert (pm .&. mask pdBits == 0)
>         "findVSpaceForASIDAssert: page directory pointer alignment check"
>     asidMap <- gets (x86KSASIDMap . ksArchState)
>     flip assert "findVSpaceForASIDAssert: page directory map mismatch"
>         $ case asidMap ! asid of
>             Nothing -> True
>             Just (_, pm') -> pm == pm'
>     return pm


These checks are too expensive to run in haskell. The first funcion checks that the pointer is to a page directory, which would require testing that each entry of the table is present. The second checks that the page directory appears in x86KSASIDMap only on the ASIDs specified, which would require walking all possible ASIDs to test. In the formalisation of this specification, these functions are given alternative definitions that make the appropriate checks.

> checkPDAt :: PPtr PDE -> Kernel ()
> checkPDAt _ = return ()


> checkPTAt :: PPtr PDE -> Kernel ()
> checkPTAt _ = return ()

> checkPML4ASIDMapMembership :: PPtr PML4E -> [ASID] -> Kernel ()
> checkPDASIDMapMembership _ _ = return ()

> checkPML4UniqueToASID :: PPtr PML4E -> ASID -> Kernel ()
> checkPML4UniqueToASID pd asid = checkPML4ASIDMapMembership pd [asid]

> checkPML4NotInASIDMap :: PPtr PML4E -> Kernel ()
> checkPML4NotInASIDMap pd = checkPML4ASIDMapMembership pd []

\subsubsection{Locating Page Table and Page Directory Slots}

The "lookupPTSlot" function locates the page table slot that maps a given virtual address, and returns a pointer to the slot. It will throw a lookup failure if the required page directory slot does not point to a page table.

> lookupPTSlot :: PPtr PML4E -> VPtr -> KernelF LookupFailure (PPtr PTE)
> lookupPTSlot pm vptr = do
>     let pdSlot = lookupPDSlot pm vptr
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
>     let pdptSlot = lookupPDPTSlot pm vptr
>     pdpte <- withoutFailure $ getObject pdptSlot
>     case pdpte of
>         PageDirectoryPDPTE {} -> do
>             let pd = ptrFromPAddr $ pdpteTable pdpte
>             let pdIndex = getPDIndex vptr 
>             let pdSlot = pd + (PPtr $ ptIndex `shiftL` 3) -- FIXME x64: word_size_bits 
>             return pdSlot
>         _ -> throw $ MissingCapability (pageBits + ptBits)

> lookupPDPTSlot :: PPtr PML4E -> VPtr -> KernelF LookupFailure (PPtr PDE)
> lookupPDPTSlot pm vptr = do
>     let pml4Slot = lookupPML4Slot pm vptr
>     pml4e <- withoutFailure $ getObject pml4Slot
>     case pml4e of
>         PDPointerTablePML4E {} -> do
>             let pdpt = ptrFromPAddr $ pml4Table pml4e
>             let pdptIndex = getPML4Index vptr 
>             let pdptSlot = pdpt + (PPtr $ ptIndex `shiftL` 3) -- FIXME x64: word_size_bits 
>             return pdptSlot

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
>     fault <- withoutFailure $ asUser thread $ getRegister ErrorRegister
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
>     asidTable <- gets (x86KSASIDTable . ksArchState)
>     when (asidTable!(asidHighBitsOf base) == Just ptr) $ do
>         let asidTable' = asidTable//[(asidHighBitsOf base, Nothing)]
>         modify (\s -> s {
>             ksArchState = (ksArchState s) { x86KSASIDTable = asidTable' }})
>         tcb <- getCurThread
>         setVMRoot tcb

\subsubsection{Deleting an Address Space}

> deleteASID :: ASID -> PPtr PML4E -> Kernel ()
> deleteASID asid pm = do
>     asidTable <- gets (x86KSASIDTable . ksArchState)
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
>     pml4e <- withoutFailure $ getObject pdptSlot
>     case pml4e of
>         PageDirectoryPTPML4E { pml4Table = pt' } -> return $
>             if pt' = addrFromPPtr pdpt then return () else throw InvalidRoot
>         _ -> throw InvalidRoot
>     flushPDPT vspace pdpt
>     withoutFailure $ storePML4E pml4e InvalidPML4E

\subsubsection{Deleting a Page Directory}

> unmapPageDirectory :: ASID -> VPtr -> PPtr PDE -> Kernel ()
> unmapPageDirectory asid vaddr pd = ignoreFailure $ do
>     vspace <- findVSpaceForASID asid
>     let pdptSlot = lookupPDPTSlot vspace vaddr
>     pdpte <- withoutFailure $ getObject pdptSlot
>     case pdpte of
>         PageDirectoryPDPTE { pdptTable = pd' } -> return $
>             if pt' = addrFromPPtr pd then return () else throw InvalidRoot
>         _ -> throw InvalidRoot
>     invalidatePageStructureCache -- FIXME x64: hardware implement
>     withoutFailure $ storePDPTE pdpte InvalidPDPTE

\subsubsection{Deleting a Page Table}

> unmapPageTable :: ASID -> VPtr -> PPtr PTE -> Kernel ()
> unmapPageTable asid vaddr pt = ignoreFailure $ do
>     vspace <- findVSpaceForASID asid
>     let pdSlot = lookupPDSlot vspace vaddr
>     pde <- withoutFailure $ getObject pdSlot
>     case pde of
>         PageTablePDE { pdeTable = pt' } -> return $
>             if pt' = addrFromPPtr pt then return () else throw InvalidRoot
>         _ -> throw InvalidRoot -- FIXME x64: dummy throw
>     flushTable vspace vaddr pt -- FIXME x64: implement
>     withoutFailure $ storePDE pdSlot InvalidPDE
>     invalidatePageStructureCache -- FIXME x64: hardware implement


\subsubsection{Unmapping a Frame}

> unmapPage :: VMPageSize -> ASID -> VPtr -> PPtr Word -> Kernel ()
> unmapPage size asid vptr ptr = ignoreFailure $ do
>     vspace <- findVSpaceForASID asid
>     case size of
>         X64SmallPage -> do
>             p <- lookupPTSlot vspace vptr
>             pte <- getObject p
>             checkMappingPPtr ptr (VMPTE pte)
>             withoutFailure $ storePTE p InvalidPTE
>         X64LargePage -> do
>             p <- lookupPDSlot vspace vptr
>             pde <- getObject p
>             checkMappingPPtr ptr (VMPDE pde)
>             withoutFailure $ storePDE p InvalidPDE
>         X64HugePage -> do
>             let p = lookupPDPTSlot vspace vptr
>             pdpte <- getObject p
>             checkMappingPPtr ptr (VMPDPTE pdpte)
>             withoutFailure $ storePDPTE p InvalidPDPTE
>     tcb <- getCurThread
>     threadRootSlot <- getThreadVSpaceRoot tcb
>     threadRoot <- getSlotCap threadRootSlot
>     case threadRoot of -- FIXME x64: don't really know if this is how this should be written
>         ArchObjectCap (PML4Cap { capPML4BasePtr = ptr', capPML4MappedAsid = Just _ }) 
>                            -> when (ptr' == vspace) $ doMachineOp $ invalidateTLBEntry vptr
>         _ -> return ()

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
>                 doMachineOp $ setCurrentVSpaceRoot (addrFromPPtr pd, asid)
>             _ -> throw InvalidRoot)
>         (\_ -> do
>             case threadRoot of
>                 ArchObjectCap (PageDirectoryCap {
>                     capPDMappedASID = Just _,
>                     capPDBasePtr = pd }) -> checkPML4NotInASIDMap pd
>                 _ -> return ()
>             globalPML4 <- gets (x86KSGlobalPML4 . ksArchState)
>             doMachineOp $ setCurrentVSpaceRoot (addrFromKPPtr globalPML4, 0) )

\subsection{Helper Functions}

The VSpace root must be an ARM page directory with an ASID allocated.

Note that this does not check that the ASID is valid, so invalid-root faults are still possible after setting this capability as the root. This is because the ASID may become invalid at any time. % XXX fix this

> isValidVTableRoot :: Capability -> Bool
> isValidVTableRoot
>     (ArchObjectCap (PageDirectoryCap { capPDMappedASID = Just _ })) = True
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

ARM memory mappings may be marked cacheable or non-cacheable. Also, parity checking can be enabled or disabled at a page table level.

> attribsFromWord :: Word -> VMAttributes
> attribsFromWord w = VMAttributes {
>     armPageCacheable = w `testBit` 0,
>     armParityEnabled = w `testBit` 1,
>     armExecuteNever = w `testBit` 2 }

\subsection{ARM Hardware ASID allocation}

X64UPDATE

> flushPDPT :: PPtr PML4E -> VPtr -> PPtr PDPTE -> Kernel ()
> flushPDPT vspace vptr pdpte = doMachineOp $ resetCR3

X64UPDATE

> flushPageDirectory :: PPtr PML4E -> VPtr -> PPtr PDE -> Kernel ()
> flushPageDirectory vsoace vptr pde = doMachineOp $ resetCR3

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
>             when (vspace = vspace') $ do 
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

> labelToFlushType :: Word -> FlushType
> labelToFlushType label = case invocationType label of
>       ARMPDClean_Data -> Clean
>       ARMPageClean_Data -> Clean
>       ARMPDInvalidate_Data -> Invalidate
>       ARMPageInvalidate_Data -> Invalidate
>       ARMPDCleanInvalidate_Data -> CleanInvalidate
>       ARMPageCleanInvalidate_Data -> CleanInvalidate
>       ARMPDUnify_Instruction -> Unify
>       ARMPageUnify_Instruction -> Unify
>       _ -> error "Should never be called without a flush invocation"

> pageBase :: VPtr -> VMPageSize -> VPtr
> pageBase vaddr size = vaddr .&. (complement $ mask (pageBitsForSize size))


> decodeARMMMUInvocation :: Word -> [Word] -> CPtr -> PPtr CTE ->
>         ArchCapability -> [(Capability, PPtr CTE)] ->
>         KernelF SyscallError ArchInv.Invocation

There are five ARM-specific capability types. They correspond to the two levels of the hardware-defined page table, the two levels of the global ASID table, and the frames used to back virtual memory pages.

Capabilities for page directories --- the top level of the hardware-defined page table --- have only a single invocation, which allows the user to clean and/or invalidate caches.

> decodeARMMMUInvocation label args _ _ cap@(PageDirectoryCap {}) _ =
>     case (isPDFlush (invocationType label), args) of
>         (True, start:end:_) -> do
>             when (end <= start) $ 
>                 throw $ InvalidArgument 1
>             when (VPtr start >= kernelBase || VPtr end > kernelBase) $
>                 throw IllegalOperation 
>             (pd,asid) <- case cap of
>                 PageDirectoryCap {
>                          capPDMappedASID = Just asid,
>                          capPDBasePtr = pd}
>                     -> return (pd,asid)
>                 _ -> throw $ InvalidCapability 0 
>             pdCheck <- lookupErrorOnFailure False $ findPDForASID asid
>             when (pdCheck /= pd) $ throw $ InvalidCapability 0
>             frameInfo <-
>                  withoutFailure $ resolveVAddr (capPDBasePtr cap) (VPtr start)
>             case frameInfo of
>                 -- Fail if there is nothing mapped here
>                 Nothing -> return $ InvokePageDirectory PageDirectoryNothing
>                 Just frameInfo -> do
>                     withoutFailure $ checkValidMappingSize (fst frameInfo)
>                     let baseStart = pageBase (VPtr start) (fst frameInfo)
>                     let baseEnd = pageBase (VPtr end - 1) (fst frameInfo)
>                     when (baseStart /= baseEnd) $
>                         throw $ RangeError start $ fromVPtr $ baseStart + 
>                                   mask (pageBitsForSize (fst frameInfo))
>                     let offset = start .&. mask (pageBitsForSize (fst frameInfo))
>                     let pStart = snd frameInfo + toPAddr offset
>                     return $ InvokePageDirectory $ PageDirectoryFlush {
>                          pdFlushType = labelToFlushType label,
>                          pdFlushStart = VPtr start,
>                          pdFlushEnd = VPtr end - 1,
>                          pdFlushPStart = pStart,
>                          pdFlushPD = pd,
>                          pdFlushASID = asid }
>         (True, _) -> throw TruncatedMessage
>         _ -> throw IllegalOperation

Capabilities for page tables --- that is, the second level of the hardware-defined page table structure --- have one method. It is used to attach the table to a top-level page directory, at a specific virtual address. It is a single-use method; if it succeeds, the table cannot be mapped again at a different address or in a different page directory, even if the original page directory is deleted. The mapping may only be removed by deleting the page table capability.

Note that these capabilities cannot be copied until they have been mapped, so any given page table object can only appear in one page directory. This is to ensure that the page unmapping operation always succeeds.

> decodeARMMMUInvocation label args _ cte cap@(PageTableCap {}) extraCaps =
>     case (invocationType label, args, extraCaps) of
>         (ARMPageTableMap, vaddr:attr:_, (pdCap,_):_) -> do
>             when (isJust $ capPTMappedAddress cap) $
>                 throw $ InvalidCapability 0
>             (pd,asid) <- case pdCap of
>                 ArchObjectCap (PageDirectoryCap {
>                          capPDMappedASID = Just asid,
>                          capPDBasePtr = pd })
>                     -> return (pd,asid)
>                 _ -> throw $ InvalidCapability 1
>             when (VPtr vaddr >= kernelBase) $
>                 throw $ InvalidArgument 0
>             pdCheck <- lookupErrorOnFailure False $ findPDForASID asid
>             when (pdCheck /= pd) $ throw $ InvalidCapability 1
>             let pdIndex = vaddr `shiftR` 20
>             let vaddr' = pdIndex `shiftL` 20
>             let pdSlot = pd + (PPtr $ pdIndex `shiftL` 2)
>             oldpde <- withoutFailure $ getObject pdSlot
>             unless (oldpde == InvalidPDE) $ throw DeleteFirst
>             let pde = PageTablePDE {
>                     pdeTable = addrFromPPtr $ capPTBasePtr cap,
>                     pdeParity = armParityEnabled $ attribsFromWord attr,
>                     pdeDomain = 0 }
>             return $ InvokePageTable $ PageTableMap {
>                 ptMapCap = ArchObjectCap $
>                     cap { capPTMappedAddress = Just (asid, VPtr vaddr') },
>                 ptMapCTSlot = cte,
>                 ptMapPDE = pde,
>                 ptMapPDSlot = pdSlot }
>         (ARMPageTableMap, _, _) -> throw TruncatedMessage
>         (ARMPageTableUnmap, _, _) -> do
>             cteVal <- withoutFailure $ getCTE cte
>             final <- withoutFailure $ isFinalCapability cteVal
>             unless final $ throw RevokeFirst
>             return $ InvokePageTable $ PageTableUnmap {
>                 ptUnmapCap = cap,
>                 ptUnmapCapSlot = cte }
>         _ -> throw IllegalOperation

Virtual page capabilities may each represent a single mapping into a page table. Unlike page table capabilities, they may be unmapped without deletion, and may be freely copied to allow multiple mappings of the same page. Along with the \emph{Map} and \emph{Unmap} operations, there is a \emph{Remap} operation, which is used to change the access permissions on an existing mapping.

> decodeARMMMUInvocation label args _ cte cap@(PageCap {}) extraCaps =
>     case (invocationType label, args, extraCaps) of
>         (ARMPageMap, vaddr:rightsMask:attr:_, (pdCap,_):_) -> do
>             when (isJust $ capVPMappedAddress cap) $
>                 throw $ InvalidCapability 0
>             (pd,asid) <- case pdCap of
>                 ArchObjectCap (PageDirectoryCap {
>                         capPDMappedASID = Just asid,
>                         capPDBasePtr = pd })
>                     -> return (pd,asid)
>                 _ -> throw $ InvalidCapability 1
>             pdCheck <- lookupErrorOnFailure False $ findPDForASID asid
>             when (pdCheck /= pd) $ throw $ InvalidCapability 1
>             let vtop = vaddr + bit (pageBitsForSize $ capVPSize cap) - 1
>             when (VPtr vtop >= kernelBase) $
>                 throw $ InvalidArgument 0
>             let vmRights = maskVMRights (capVPRights cap) $
>                     rightsFromWord rightsMask
>             checkVPAlignment (capVPSize cap) (VPtr vaddr)
>             entries <- createMappingEntries (addrFromPPtr $ capVPBasePtr cap)
>                 (VPtr vaddr) (capVPSize cap) vmRights
>                 (attribsFromWord attr) pd
>             ensureSafeMapping entries
>             return $ InvokePage $ PageMap {
>                 pageMapASID = asid,
>                 pageMapCap = ArchObjectCap $
>                     cap { capVPMappedAddress = Just (asid, VPtr vaddr) },
>                 pageMapCTSlot = cte,
>                 pageMapEntries = entries }
>         (ARMPageMap, _, _) -> throw TruncatedMessage
>         (ARMPageRemap, rightsMask:attr:_, (pdCap, _):_) -> do
>             (pd,asid) <- case pdCap of
>                 ArchObjectCap (PageDirectoryCap {
>                         capPDMappedASID = Just asid,
>                         capPDBasePtr = pd })
>                     -> return (pd,asid)
>                 _ -> throw $ InvalidCapability 1
>             (asidCheck, vaddr) <- case capVPMappedAddress cap of
>                 Just a -> return a
>                 _ -> throw $ InvalidCapability 0
>             pdCheck <- lookupErrorOnFailure False $ findPDForASID asidCheck
>             when (pdCheck /= pd || asidCheck /= asid) $ throw $ InvalidCapability 1
>             let vmRights = maskVMRights (capVPRights cap) $
>                     rightsFromWord rightsMask
>             checkVPAlignment (capVPSize cap) vaddr
>             entries <- createMappingEntries (addrFromPPtr $ capVPBasePtr cap)
>                 vaddr (capVPSize cap) vmRights (attribsFromWord attr) pd
>             ensureSafeMapping entries
>             return $ InvokePage $ PageRemap {
>                 pageRemapASID = asidCheck,
>                 pageRemapEntries = entries }
>         (ARMPageRemap, _, _) -> throw TruncatedMessage
>         (ARMPageUnmap, _, _) -> return $ InvokePage $ PageUnmap {
>                 pageUnmapCap = cap,
>                 pageUnmapCapSlot = cte }
>         (ARMPageClean_Data, _, _) -> decodeARMPageFlush label args cap
>         (ARMPageInvalidate_Data, _, _) -> decodeARMPageFlush label args cap
>         (ARMPageCleanInvalidate_Data, _, _) -> decodeARMPageFlush label args cap
>         (ARMPageUnify_Instruction, _, _) -> decodeARMPageFlush label args cap
>         (ARMPageGetAddress, _, _) -> return $ InvokePage $ PageGetAddr (capVPBasePtr cap)
>         _ -> throw IllegalOperation


The ASID control capability refers to the top level of a global two-level table used for allocating address space identifiers. It has only one method, "MakePool", which creates an ASID allocation pool given a single frame of untyped memory. Since this method allocates part of a global range of ASIDs, it may return a "DeleteFirst" error if the entire range has been allocated to existing ASID pools.

> decodeARMMMUInvocation label args _ _ ASIDControlCap extraCaps =
>     case (invocationType label, args, extraCaps) of
>         (ARMASIDControlMakePool, index:depth:_,
>                 (untyped,parentSlot):(root,_):_) -> do
>             asidTable <- withoutFailure $ gets (armKSASIDTable . ksArchState)
>             let free = filter (\(x,y) -> x <= (1 `shiftL` asidHighBits) - 1 && isNothing y) $ assocs asidTable
>             when (null free) $ throw DeleteFirst
>             let base = (fst $ head free) `shiftL` asidLowBits
>             let pool = makeObject :: ASIDPool
>             frame <- case untyped of
>                 UntypedCap {} | capBlockSize untyped == objBits pool -> do
>                     ensureNoChildren parentSlot
>                     return $ capPtr untyped
>                 _ -> throw $ InvalidCapability 1
>             destSlot <- lookupTargetSlot
>                 root (CPtr index) (fromIntegral depth)
>             ensureEmptySlot destSlot
>             return $ InvokeASIDControl $ MakePool {
>                 makePoolFrame = frame,
>                 makePoolSlot = destSlot,
>                 makePoolParent = parentSlot,
>                 makePoolBase = base }
>         (ARMASIDControlMakePool, _, _) -> throw TruncatedMessage
>         _ -> throw IllegalOperation

ASID pool capabilities are used to allocate unique address space identifiers for virtual address spaces. They support the "Assign" method, which allocates an ASID for a given page directory capability. The directory must not already have an ASID. Page directories cannot be used until they have been allocated an ASID using this method.

> decodeARMMMUInvocation label _ _ _ cap@(ASIDPoolCap {}) extraCaps =
>     case (invocationType label, extraCaps) of
>         (ARMASIDPoolAssign, (pdCap,pdCapSlot):_) ->
>             case pdCap of
>                 ArchObjectCap (PageDirectoryCap { capPDMappedASID = Nothing })
>                   -> do
>                     asidTable <- withoutFailure $ gets (armKSASIDTable . ksArchState)
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
>                         assignASIDCTSlot = pdCapSlot }
>                 _ -> throw $ InvalidCapability 1
>         (ARMASIDPoolAssign, _) -> throw TruncatedMessage
>         _ -> throw IllegalOperation

> decodeARMPageFlush :: Word -> [Word] -> ArchCapability ->
>                       KernelF SyscallError ArchInv.Invocation
> decodeARMPageFlush label args cap = case (args, capVPMappedAddress cap) of
>     (start:end:_, Just (asid, vaddr)) -> do
>         pd <- lookupErrorOnFailure False $ findPDForASID asid
>         when (end <= start) $ 
>             throw $ InvalidArgument 1
>         let pageSize = 1 `shiftL` pageBitsForSize (capVPSize cap)
>         let pageBase = addrFromPPtr $ capVPBasePtr cap
>         when (start >= pageSize || end > pageSize) $
>             throw $ InvalidArgument 0
>         let pstart = pageBase + toPAddr start
>         let start' = start + fromVPtr vaddr
>         let end' = end + fromVPtr vaddr
>         return $ InvokePage $ PageFlush {
>               pageFlushType = labelToFlushType label,
>               pageFlushStart = VPtr $ start',
>               pageFlushEnd = VPtr $ end' - 1,
>               pageFlushPStart = pstart,
>               pageFlushPD = pd,
>               pageFlushASID = asid }
>     (_:_:_, Nothing) -> throw IllegalOperation     
>     _ -> throw TruncatedMessage


Checking virtual address for page size dependent alignment:

> checkVPAlignment :: VMPageSize -> VPtr -> KernelF SyscallError ()
>
> checkVPAlignment sz w =
>     unless (w .&. mask (pageBitsForSize sz) == 0) $
>            throw AlignmentError

When we fetch a mapping in which to perform a page flush, we assert that its
size is valid. This check is ignored in Haskell, but the Isabelle model has a
notion of the largest permitted object size, and checks it appropriately.

> checkValidMappingSize :: VMPageSize -> Kernel ()
> checkValidMappingSize _ = return ()

\subsection{Invocation Implementations}

> performARMMMUInvocation :: ArchInv.Invocation -> KernelP [Word]
> performARMMMUInvocation i = withoutPreemption $ do
>     case i of
>         InvokePageDirectory oper -> performPageDirectoryInvocation oper
>         InvokePageTable oper -> performPageTableInvocation oper
>         InvokePage oper -> performPageInvocation oper
>         InvokeASIDControl oper -> performASIDControlInvocation oper
>         InvokeASIDPool oper -> performASIDPoolInvocation oper
>     return $ []

> performPageDirectoryInvocation :: PageDirectoryInvocation -> Kernel ()
> performPageDirectoryInvocation (PageDirectoryFlush typ start end pstart pd asid) =

Don't flush an empty range.

>     when (start < end) $ do
>         root_switched <- setVMRootForFlush pd asid
>         doMachineOp $ doFlush typ start end pstart
>         when root_switched $ do
>             tcb <- getCurThread
>             setVMRoot tcb

> performPageDirectoryInvocation PageDirectoryNothing = return ()

> performPageTableInvocation :: PageTableInvocation -> Kernel ()

> performPageTableInvocation (PageTableMap cap ctSlot pde pdSlot) = do
>     updateCap ctSlot cap
>     storePDE pdSlot pde
>     doMachineOp $ cleanByVA_PoU (VPtr $ fromPPtr $ pdSlot) (addrFromPPtr pdSlot)

> performPageTableInvocation (PageTableUnmap cap ctSlot) = do
>     case capPTMappedAddress cap of
>         Just (asid, vaddr) -> do
>             unmapPageTable asid vaddr (capPTBasePtr cap)
>             let ptr = capPTBasePtr cap
>             let pteBits = objBits InvalidPTE
>             let slots = [ptr, ptr + bit pteBits .. ptr + bit ptBits - 1]
>             mapM_ (flip storePTE InvalidPTE) slots
>             doMachineOp $
>                 cleanCacheRange_PoU (VPtr $ fromPPtr $ ptr)
>                                     (VPtr $ fromPPtr $ (ptr + (1 `shiftL` ptBits) - 1))
>                                     (addrFromPPtr ptr)
>         Nothing -> return ()
>     ArchObjectCap cap <- getSlotCap ctSlot
>     updateCap ctSlot (ArchObjectCap $
>                            cap { capPTMappedAddress = Nothing })

When checking if there was already something mapped before a PageMap or PageRemap,
we need only check the first slot because ensureSafeMapping tells us that
the PT/PD is consistent.

> pteCheckIfMapped :: PPtr PTE -> Kernel Bool
> pteCheckIfMapped slot = do
>     pt <- getObject slot
>     return $ pt /= InvalidPTE

> pdeCheckIfMapped :: PPtr PDE -> Kernel Bool
> pdeCheckIfMapped slot = do
>     pd <- getObject slot
>     return $ pd /= InvalidPDE

> performPageInvocation :: PageInvocation -> Kernel ()
>
> performPageInvocation (PageMap asid cap ctSlot entries) = do
>     updateCap ctSlot cap
>     case entries of
>         Left (pte, slots) -> do
>             tlbFlush <- pteCheckIfMapped (head slots)
>             mapM (flip storePTE pte) slots
>             doMachineOp $
>                 cleanCacheRange_PoU (VPtr $ fromPPtr $ head slots)
>                                     (VPtr $ (fromPPtr (last slots)) + (bit (objBits (undefined::PTE)) - 1))
>                                     (addrFromPPtr (head slots))
>             when tlbFlush $ invalidateTLBByASID asid
>         Right (pde, slots) -> do
>             tlbFlush <- pdeCheckIfMapped (head slots)
>             mapM (flip storePDE pde) slots
>             doMachineOp $
>                 cleanCacheRange_PoU (VPtr $ fromPPtr $ head slots)
>                                     (VPtr $ (fromPPtr (last slots)) + (bit (objBits (undefined::PDE)) - 1))
>                                     (addrFromPPtr (head slots))
>             when tlbFlush $ invalidateTLBByASID asid
>
> performPageInvocation (PageRemap asid (Left (pte, slots))) = do
>     tlbFlush <- pteCheckIfMapped (head slots)
>     mapM (flip storePTE pte) slots
>     doMachineOp $
>         cleanCacheRange_PoU (VPtr $ fromPPtr $ head slots)
>                             (VPtr $ (fromPPtr (last slots)) + (bit (objBits (undefined::PTE)) - 1))
>                             (addrFromPPtr (head slots))
>     when tlbFlush $ invalidateTLBByASID asid
>
> performPageInvocation (PageRemap asid (Right (pde, slots))) = do
>     tlbFlush <- pdeCheckIfMapped (head slots)
>     mapM (flip storePDE pde) slots
>     doMachineOp $
>         cleanCacheRange_PoU (VPtr $ fromPPtr $ head slots)
>                             (VPtr $ (fromPPtr (last slots)) + (bit (objBits (undefined::PDE)) - 1))
>                             (addrFromPPtr (head slots))
>     when tlbFlush $ invalidateTLBByASID asid
>
> performPageInvocation (PageUnmap cap ctSlot) = do
>     case capVPMappedAddress cap of
>         Just (asid, vaddr) -> unmapPage (capVPSize cap) asid vaddr
>                                     (capVPBasePtr cap)
>         Nothing -> return ()
>     ArchObjectCap cap <- getSlotCap ctSlot
>     updateCap ctSlot (ArchObjectCap $
>                            cap { capVPMappedAddress = Nothing })
>
> performPageInvocation (PageFlush typ start end pstart pd asid) = 
>     when (start < end) $ do
>         root_switched <- setVMRootForFlush pd asid
>         doMachineOp $ doFlush typ start end pstart
>         when root_switched $ do
>             tcb <- getCurThread
>             setVMRoot tcb
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
>     asidTable <- gets (armKSASIDTable . ksArchState)
>     let asidTable' = asidTable//[(asidHighBitsOf base, Just poolPtr)]
>     modify (\s -> s {
>         ksArchState = (ksArchState s) { armKSASIDTable = asidTable' }})

> performASIDPoolInvocation :: ASIDPoolInvocation -> Kernel ()
> performASIDPoolInvocation (Assign asid poolPtr ctSlot) = do
>     oldcap <- getSlotCap ctSlot
>     ASIDPool pool <- getObject poolPtr
>     let ArchObjectCap cap = oldcap
>     updateCap ctSlot (ArchObjectCap $ cap { capPDMappedASID = Just asid })
>     let pool' = pool//[(asid .&. mask asidLowBits, Just $ capPDBasePtr cap)]
>     setObject poolPtr $ ASIDPool pool'

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


