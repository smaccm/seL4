% FIXME: Clagged from ARM
% Copyright 2014, General Dynamics C4 Systems
%
% This software may be distributed and modified according to the terms of
% the GNU General Public License version 2. Note that NO WARRANTY is provided.
% See "LICENSE_GPLv2.txt" for details.
%
% @TAG(GD_GPL)
%

This module contains operations on machine-specific object types for the ARM.

> module SEL4.Object.ObjectType.X64 where

\begin{impdetails}

> import SEL4.Machine.RegisterSet
> import SEL4.Machine.Hardware.X64
> import SEL4.Model
> import SEL4.Model.StateData.X64
> import SEL4.API.Types
> import SEL4.API.Failures
> import SEL4.API.Invocation.X64 as ArchInv
> import SEL4.Object.Structures
> import SEL4.Kernel.VSpace.X64

> import Data.Bits
> import Data.Array

\end{impdetails}

The ARM-specific types and structures are qualified with the "Arch.Types" and "Arch.Structures" prefixes, respectively. This is to avoid namespace conflicts with the platform-independent modules.

> import qualified SEL4.API.Types.ARM as Arch.Types

\subsection{Copying and Mutating Capabilities}

> deriveCap :: PPtr CTE -> ArchCapability -> KernelF SyscallError ArchCapability

It is not possible to copy a page table or page directory capability unless it has been mapped.

> deriveCap _ (c@PageTableCap { capPTMappedAddress = Just _ }) = return c
> deriveCap _ (PageTableCap { capPTMappedAddress = Nothing })
>     = throw IllegalOperation
> deriveCap _ (c@PageDirectoryCap { capPDMappedAddress = Just _ }) = return c
> deriveCap _ (PageDirectoryCap { capPDMappedASID = Nothing })
>     = throw IllegalOperation
> deriveCap _ (c@PDPointerTableCap { capPDPTMappedAddress = Just _ }) = return c
> deriveCap _ (PDPointerTableCap { capPDPTMappedAddress = Nothing)
>     = throw IllegalOperation
> deriveCap _ (c@PML4Cap { capPML4MappedASID = Just _ }) = return c
> deriveCap _ (PML4Cap { capPML4MappedASID = Nothing)
>     = throw IllegalOperation

Page capabilities are copied without their mapping information, to allow them to be mapped in multiple locations.

> deriveCap _ (c@PageCap {}) = return $ c { capVPMappedAddress = Nothing, capVPMapType = VMNoMap }

ASID capabilities can be copied without modification, as can IOPort and IOSpace caps.

> deriveCap _ c@ASIDControlCap = return c
> deriveCap _ (c@ASIDPoolCap {}) = return c
> deriveCap _ (c@IOPortCap {}) = return c
> deriveCap _ (c@IOSpaceCap {}) = return c

IOPTs

> deriveCap _ (c@IOPageTableCap { capIOPTMappedAddress = Just _ }) = return c
> deriveCap _ (IOPageTableCap { capIOPTMappedAddress = Nothing }) 
>     = throw IllegalOperation

X64 has two writable user data caps

> -- FIXME x64: io_space_capdata_get_domainID
> ioSpaceGetDomainID :: Word -> Word16
> ioSpaceGetDomainID dat = return dat

> -- FIXME x64: io_space_capdata_get_PCIDevice
> ioSpaceGetPCIDevice :: Word -> Word16
> ioSpaceGetPCIDevice dat = return dat

> -- FIXME x64: io_port_capdata_get_firstPort
> ioPortGetFirstPort :: Word -> Word16
> ioPortGetFirstPort dat = return dat 

> -- FIXME x64: io_port_capdata_get_lastPort
> ioPortGetLastPort :: Word -> Word16
> ioPortGetLastPort dat = return dat
   
> updateCapData :: Bool -> Word -> ArchCapability -> Kernel Capability
> updateCapData preserve newData (c@IOSpaceCap {}) = do
>     let pciDevice = ioSpaceGetPCIDevice newData
>     let domID = ioSpaceGetDomainID newData
>     fstValidDom <- gets (x64KSFirstValidIODomain . ksArchState)
>     domIDBits <- gets (x64KSnumIODomainIDBits . ksArchState)
>     return $ if (not preserve && capIOPCIDevice c = 0 && domID >= fstValidDom
>                     && domID /= 0 && domID <= mask domIDBits) 
>                then (ArchCapability (IOSpaceCap domID pciDevice))
>                else NullCap
> updateCapData preserve newData (c@IOPortCap {}) = do
>     let firstPort = ioPortGetFirstPort newData
>     let lastPort = ioPortGetLastPort newData
>     assert (capIOPortFirstPort c <= capIOPortLastPort c)
>     return $ if (firstPort <= lastPort && firstPort >= capIOPortFirstPort c
>                      && lastPort <= capIOPortLastPort c)
>               then (ArchCapability (IOPortCap firstPort lastPort))
>               else NullCap
> updateCapData _ _ c = return c

Page capabilities have read and write permission bits, which are used to restrict virtual memory accesses to their contents. Note that the ability to map objects into a page table or page directory is granted by possession of a capability to it; there is no specific permission bit restricting this ability.

> maskCapRights :: CapRights -> ArchCapability -> Capability
> maskCapRights r c@(PageCap {}) = ArchObjectCap $ c {
>     capVPRights = maskVMRights (capVPRights c) r }
> maskCapRights _ c = ArchObjectCap c

\subsection{Deleting Capabilities}

> finaliseCap :: ArchCapability -> Bool -> Kernel Capability

Deletion of a final capability to an ASID pool requires that the pool is removed from the global ASID table.

> finaliseCap (ASIDPoolCap { capASIDBase = b, capASIDPool = ptr }) True = do
>     deleteASIDPool b ptr
>     return NullCap

Delete a PML4

> finaliseCap (PML4Cap {
>         capPML4MappedASID = Just a,
>         capPML4BasePtr = ptr }) True = do
>     deleteASID a ptr
>     return NullCap

Delete a PDPT

> finaliseCap (PDPointerTableCap {
>         capPDPTMappedAddress = Just (a, v),
>         capPDPTBasePtr = ptr }) True = do
>     unmapPDPT a v ptr
>     return NullCap


Deletion of a final capability to a page directory with an assigned ASID requires the ASID assignment to be removed, and the ASID flushed from the caches.

> finaliseCap (PageDirectoryCap {
>         capPDMappedAddress = Just (a, v),
>         capPDBasePtr = ptr }) True = do
>     unmapPageDirectory a v ptr
>     return NullCap

Deletion of a final capability to a page table that has been mapped requires that the mapping be removed from the page directory, and the corresponding addresses flushed from the caches.

> finaliseCap (PageTableCap {
>         capPTMappedAddress = Just (a, v),
>         capPTBasePtr = ptr }) True = do
>     unmapPageTable a v ptr
>     return NullCap

> finaliseCap (IOSpaceCap {}) True = return NullCap -- FIXME x64: not yet implemented in C

> finaliseCap (c@IOPageTableCap { capIOPTMappedAddress = Just _ }) True = do
>     deleteIOPageTable c
>     return NullCap

Deletion of any mapped frame capability requires the page table slot to be located and cleared, and the unmapped address to be flushed from the caches.

> finaliseCap (c@PageCap { capVPMappedAddress = Just (a, v),
>                        capVPSize = s, capVPBasePtr = ptr,
>                        capVPMapType = typ }) _
>     = do
>         case typ of
>             VMIOSpaceMap -> unmapIOPage c
>             _ -> unmapPage s a v ptr
>         return NullCap

All other capabilities need no finalisation action.

> finaliseCap _ _ = return NullCap

\subsection{Recycling Capabilities}

> resetMemMapping :: ArchCapability -> ArchCapability
> resetMemMapping (PageCap p rts _ sz _) = PageCap p rts sz Nothing
> resetMemMapping (PageTableCap ptr _) = PageTableCap ptr Nothing
> resetMemMapping (PageDirectoryCap ptr _) = PageDirectoryCap ptr Nothing
> resetMemMapping (PDPointerTableCap ptr _ ) = PDPointerTableCap ptr Nothing
> resetMemMapping (PML4Cap ptr _) = PML4Cap ptr Nothing
> resetMemMapping cap = cap

> recycleCap :: Bool -> ArchCapability -> Kernel ArchCapability
> recycleCap is_final (cap@PageCap {}) = do
>       doMachineOp $ clearMemory (capVPBasePtr cap)
>           (1 `shiftL` (pageBitsForSize $ capVPSize cap))
>       finaliseCap cap is_final
>       return $ resetMemMapping cap
>
> recycleCap is_final (cap@PageTableCap { capPTBasePtr = ptr }) = do
>     let pteBits = objBits InvalidPTE
>     let slots = [ptr, ptr + bit pteBits .. ptr + bit ptBits - 1]
>     mapM_ (flip storePTE InvalidPTE) slots
>     case capPTMappedAddress cap of
>         Nothing -> return ()
>         Just (a, v) -> unmapPageTable a v ptr
>     finaliseCap cap is_final
>     return (if is_final then resetMemMapping cap else cap)

> recycleCap is_final (cap@PageDirectoryCap { capPDBasePtr = ptr }) = do
>     let pdeBits = objBits InvalidPDE
>     let slots = [ptr, ptr + bit pdeBits .. ptr + bit pdBits - 1]
>     mapM_ (flip storePTE InvalidPDE) slots
>     case capPDMappedAddress cap of
>         Nothing -> return ()
>         Just (a, v) -> unmapPageDirectory a v ptr
>     finaliseCap cap is_final
>     return (if is_final then resetMemMapping cap else cap)

> recycleCap is_final (cap@PDPointerTableCap { capPDPTBasePtr = ptr }) = do
>     let pdpteBits = objBits InvalidPDPTE
>     let slots = [ptr, ptr + bit pdpteBits .. ptr + bit pdptBits - 1]
>     mapM_ (flip storePTE InvalidPDPTE) slots
>     case capPDMappedAddress cap of
>         Nothing -> return ()
>         Just (a, v) -> unmapPDPT a v ptr
>     finaliseCap cap is_final
>     return (if is_final then resetMemMapping cap else cap)

> recycleCap is_final (cap@PML4Cap { capPML4BasePtr = ptr }) = do
>     let pmBits = objBits InvalidPML4E
>     let slots = [ptr, ptr + bit pmBits .. ptr + bit pml4bits - 1]
>     mapM_ (flip storePDE InvalidPML4E) slots
>     finaliseCap cap is_final
>     return (if is_final then resetMemMapping cap else cap)  

> recycleCap _ (c@IOPortCap {}) = return c
> recycleCap is_final (cap@IOSpaceCap) = do
>     finaliseCap cap is_final
>     return cap

> -- FIXME x64: check that this is final implementation
> recycleCap is_final (cap@IOPageTableCap { capIOPTBasePtr = ptr }) = do
>     let iopteBits = objBits InvalidIOPTE
>     let slots = [ptr, ptr + bit iopteBits .. ptr + bit ioptBits - 1]
>     mapM_ (flip storeIOPTE InvalidIOPTE) slots
>     finaliseCap cap is_final
>     return cap

> recycleCap _ ASIDControlCap = return ASIDControlCap
> recycleCap _ (cap@ASIDPoolCap { capASIDBase = base, capASIDPool = ptr }) = do
>     asidTable <- gets (x64KSASIDTable . ksArchState)
>     when (asidTable!(asidHighBitsOf base) == Just ptr) $ do
>         deleteASIDPool base ptr
>         setObject ptr (makeObject :: ASIDPool)
>         asidTable <- gets (x64KSASIDTable . ksArchState)
>         let asidTable' = asidTable//[(asidHighBitsOf base, Just ptr)]
>         modify (\s -> s {
>             ksArchState = (ksArchState s) { x64KSASIDTable = asidTable' }})
>     return cap
 
 
> hasRecycleRights :: ArchCapability -> Bool

> hasRecycleRights (PageCap { capVPRights = rights }) = rights == VMReadWrite
> hasRecycleRights _ = True


\subsection{Identifying Capabilities}

> sameRegionAs :: ArchCapability -> ArchCapability -> Bool
> sameRegionAs (a@PageCap {}) (b@PageCap {}) =
>     (botA <= botB) && (topA >= topB) && (botB <= topB)
>     where
>         botA = capVPBasePtr a
>         botB = capVPBasePtr b
>         topA = botA + bit (pageBitsForSize $ capVPSize a) - 1
>         topB = botB + bit (pageBitsForSize $ capVPSize b) - 1
> sameRegionAs (a@PageTableCap {}) (b@PageTableCap {}) =
>     capPTBasePtr a == capPTBasePtr b
> sameRegionAs (a@PageDirectoryCap {}) (b@PageDirectoryCap {}) =
>     capPDBasePtr a == capPDBasePtr b
> sameRegionAs (a@PDPointerTableCap {}) (b@PDPointerTableCap {}) =
>     capPDPTBasePtr a == capPDPTBasePtr b
> sameRegionAs (a@PML4Cap {}) (b@PML4Cap {}) =
>     capPML4BasePtr a == capPML4BasePtr b
> sameRegionAs ASIDControlCap ASIDControlCap = True
> sameRegionAs (a@ASIDPoolCap {}) (b@ASIDPoolCap {}) =
>     capASIDPool a == capASIDPool b
> sameRegionAs (a@IOPortCap {}) (b@IOPortCap {}) = True
> sameRegionAs (a@IOSpaceCap {}) (b@IOSpaceCap {}) =
>     capIOPCIDevice a == capIOPCIDevice b
> sameRegionAs (a@IOPageTableCap {}) (b@IOPageTableCap {}) =
>     capIOPTBasePtr a == capIOPTBasePtr b
> sameRegionAs _ _ = False

> isPhysicalCap :: ArchCapability -> Bool
> isPhysicalCap ASIDControlCap = False
> isPhysicalCap _ = True

> sameObjectAs :: ArchCapability -> ArchCapability -> Bool
> sameObjectAs (a@PageCap { capVPBasePtr = ptrA }) (b@PageCap {}) =
>     (ptrA == capVPBasePtr b) && (capVPSize a == capVPSize b)
>         && (ptrA <= ptrA + bit (pageBitsForSize $ capVPSize a) - 1) -- FIXME x64: is overflow check required for x64?
> sameObjectAs a b = sameRegionAs a b

\subsection{Creating New Capabilities}

Creates a page-sized object that consists of plain words observable to the user.

> createPageObject ptr numPages = do
>     addrs <- placeNewObject ptr UserData numPages
>     doMachineOp $ initMemory (PPtr $ fromPPtr ptr) (1 `shiftL` (pageBits + numPages) )
>     return addrs

Create an architecture-specific object.

> -- FIXME x64: WRITE THIS
> createObject :: ObjectType -> PPtr () -> Int -> Kernel ArchCapability
> createObject t regionBase _ =
>     let funupd = (\f x v y -> if y == x then v else f y) in
>     let pointerCast = PPtr . fromPPtr
>     in case t of
>         Arch.Types.APIObjectType _ ->
>             fail "Arch.createObject got an API type"
>         Arch.Types.SmallPageObject -> do
>             createPageObject regionBase 0
>             modify (\ks -> ks { gsUserPages =
>               funupd (gsUserPages ks)
>                      (fromPPtr regionBase) (Just ARMSmallPage)})
>             return $! PageCap (pointerCast regionBase)
>                   VMReadWrite ARMSmallPage Nothing
>         Arch.Types.LargePageObject -> do
>             createPageObject regionBase 4
>             modify (\ks -> ks { gsUserPages =
>               funupd (gsUserPages ks)
>                      (fromPPtr regionBase) (Just ARMLargePage)})
>             return $! PageCap (pointerCast regionBase)
>                   VMReadWrite ARMLargePage Nothing
>         Arch.Types.SectionObject -> do
>             createPageObject regionBase 8
>             modify (\ks -> ks { gsUserPages =
>               funupd (gsUserPages ks)
>                      (fromPPtr regionBase) (Just ARMSection)})
>             return $! PageCap (pointerCast regionBase)
>                   VMReadWrite ARMSection Nothing
>         Arch.Types.SuperSectionObject -> do
>             createPageObject regionBase 12 
>             modify (\ks -> ks { gsUserPages =
>               funupd (gsUserPages ks)
>                      (fromPPtr regionBase) (Just ARMSuperSection)})
>             return $! PageCap (pointerCast regionBase)
>                   VMReadWrite ARMSuperSection Nothing
>         Arch.Types.PageTableObject -> do
>             let ptSize = ptBits - objBits (makeObject :: PTE)
>             let regionSize = (1 `shiftL` ptBits)
>             placeNewObject regionBase (makeObject :: PTE) ptSize
>             doMachineOp $
>                 cleanCacheRange_PoU (VPtr $ fromPPtr regionBase)
>                       (VPtr $ fromPPtr regionBase + regionSize - 1)
>                       (addrFromPPtr regionBase)
>             return $! PageTableCap (pointerCast regionBase) Nothing
>         Arch.Types.PageDirectoryObject -> do
>             let pdSize = pdBits - objBits (makeObject :: PDE)
>             let regionSize = (1 `shiftL` pdBits)
>             placeNewObject regionBase (makeObject :: PDE) pdSize
>             copyGlobalMappings (pointerCast regionBase)
>             doMachineOp $
>                 cleanCacheRange_PoU (VPtr $ fromPPtr regionBase)
>                       (VPtr $ fromPPtr regionBase + regionSize - 1)
>                       (addrFromPPtr regionBase)
>             return $! PageDirectoryCap (pointerCast regionBase) Nothing

\subsection{Capability Invocation}

> decodeInvocation :: Word -> [Word] -> CPtr -> PPtr CTE ->
>         ArchCapability -> [(Capability, PPtr CTE)] ->
>         KernelF SyscallError ArchInv.Invocation
> decodeInvocation = decodeARMMMUInvocation

> performInvocation :: ArchInv.Invocation -> KernelP [Word]
> performInvocation = performARMMMUInvocation

\subsection{Helper Functions}

> capUntypedPtr :: ArchCapability -> PPtr ()
> capUntypedPtr (PageCap { capVPBasePtr = PPtr p }) = PPtr p
> capUntypedPtr (PageTableCap { capPTBasePtr = PPtr p }) = PPtr p
> capUntypedPtr (PageDirectoryCap { capPDBasePtr = PPtr p }) = PPtr p
> capUntypedPtr (PDPointerTableCap { capPDPTBasePtr = PPtr p}) = PPtr p
> capUntypedPtr (PML4Cap { capPML4BasePtr = PPtr p}) = PPtr p
> capUntypedPtr ASIDControlCap = error "ASID control has no pointer"
> capUntypedPtr (ASIDPoolCap { capASIDPool = PPtr p }) = PPtr p
> capUntypedPtr (IOPortCap {}) = error "IOPortCap has no pointer"
> capUntypedPtr (IOSpaceCap {}) = error "IOSpaceCap has no pointer"
> capUntypedPtr (IOPageTableCap { capIOPTBasePtr = PPtr p }) = PPtr p


FIXME x64: ASIDControlCap is size 0?

> capUntypedSize :: ArchCapability -> Word
> capUntypedSize (PageCap {capVPSize = sz}) = 1 `shiftL` pageBitsForSize sz
> capUntypedSize (PageTableCap {}) = 1 `shiftL` 12
> capUntypedSize (PageDirectoryCap {}) = 1 `shiftL` 12
> capUntypedSize (PDPointerTableCap {}) = 1 `shiftL` 12
> capUntypedSize (PML4Cap {}) = 1 `shiftL` 12
> capUntypedSize (ASIDControlCap {}) = 0
> capUntypedSize (ASIDPoolCap {}) = 1 `shiftL` (asidLowBits + 3)
> capUntypedSize (IOPortCap {}) = 0
> capUntypedSize (IOSpaceCap {}) = 0
> capUntypedSize (IOPageTableCap {}) = 1 `shiftL` 12


