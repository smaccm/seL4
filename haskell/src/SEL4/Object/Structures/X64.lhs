% FIXME: Clagged from ARM
% Copyright 2014, General Dynamics C4 Systems
%
% This software may be distributed and modified according to the terms of
% the GNU General Public License version 2. Note that NO WARRANTY is provided.
% See "LICENSE_GPLv2.txt" for details.
%
% @TAG(GD_GPL)
%

This module contains the physical memory model's representations of the ARM-specific data structures, as well as a type representing a capability to an ARM-specific object.

\begin{impdetails}

This module makes use of the GHC extension allowing declaration of types with no constructors, so GHC language extensions are enabled.

> {-# LANGUAGE EmptyDataDecls, GeneralizedNewtypeDeriving #-}

\end{impdetails}

> module SEL4.Object.Structures.X64 where

\begin{impdetails}

> import SEL4.Machine.RegisterSet
> import SEL4.Machine.Hardware.X64
> import Data.Array
> import Data.Word
> import Data.Bits

\end{impdetails}

\subsection{Capabilities}

There are six ARM-specific capability types: the global ASID control capability, ASID pools, page tables, page directories, and pages.

> data ArchCapability
>     = ASIDPoolCap {
>         capASIDPool :: PPtr ASIDPool,
>         capASIDBase :: ASID }
>     | ASIDControlCap
>     | IOPortCap {
>         capIOPortFirstPort :: Word, -- FIXME
>         capIOPortLastPort :: Word }
>     | IOSpaceCap {
>         capIODomainID :: Word16, --FIXME types
>         capIOPCIDevice :: Word16 }
>     | IOPageTableCap {
>         capIOPTBasePtr :: PPtr IOPTE, -- ?? FIXME WTF TYPES
>         capIOPTMappedAddress :: Maybe (IOASID, VPtr) }
>     | PageCap {
>         capVPBasePtr :: PPtr Word,
>         capVPRights :: VMRights,
>         capVPMapType :: VMMapType,
>         capVPSize :: VMPageSize,
>         capVPMappedAddress :: Maybe (ASID, VPtr) }
>     | PageTableCap {
>         capPTBasePtr :: PPtr PTE,
>         capPTMappedAddress :: Maybe (ASID, VPtr) }
>     | PageDirectoryCap {
>         capPDBasePtr :: PPtr PDE,
>         capPDMappedAddress :: Maybe (ASID, VPtr) }
>     | PDPointerTableCap {
>         capPDPTBasePtr :: PPtr PDPTE,
>         capPDPTMappedAddress :: Maybe (ASID, VPtr) }
>     | PML4Cap {
>         capPML4BasePtr :: PPtr PML4E,
>         capPML4MappedASID :: Maybe ASID }
>     deriving (Eq, Show)

\subsection{Kernel Objects}

The ARM kernel stores one ARM-specific type of object in the PSpace: ASID pools, which are second level nodes in the global ASID table. 

> data ArchKernelObject
>     = KOASIDPool ASIDPool
>     | KOPTE PTE
>     | KOPDE PDE
>     | KOPDPTE PDPTE
>     | KOPML4E PML4E
>     -- FIXME x64: more here? io stuff?
>     deriving Show

> archObjSize ::  ArchKernelObject -> Int
> archObjSize a = case a of 
>                 KOASIDPool _ -> pageBits
>                 KOPTE _ -> 3 
>                 KOPDE _ -> 3
>                 KOPDPTE _ -> 3
>                 KOPML4E _ -> 3

\subsection{ASID Pools}

An ASID pool is an array of pointers to page directories. This is used to implement virtual ASIDs on ARM; it is not accessed by the hardware.

> newtype ASIDPool = ASIDPool (Array ASID (Maybe (PPtr PDE)))
>     deriving Show

An ASID is an unsigned word. Note that it is a \emph{virtual} address space identifier, and may not correspond to any hardware-defined identifier --- especially on ARMv5 and earlier, where the only identifier implemented in hardware is the 4-bit domain number.

> newtype ASID = ASID Word64
>     deriving (Show, Eq, Ord, Enum, Real, Integral, Num, Bits, Ix, Bounded)

> newtype IOASID = IOASID Word16
>     deriving (Show, Eq, Ord, Enum, Real, Integral, Num, Bits, Ix, Bounded)

ASIDs are mapped to address space roots by a global two-level table. The actual ASID values are opaque to the user, as are the sizes of the levels of the tables; ASID allocation calls will simply return an error once the available ASIDs are exhausted.
> -- FIXME x64: these need to be changed for 64bit

> asidHighBits :: Int
> asidHighBits = 8

> asidLowBits :: Int
> asidLowBits = 10

> asidBits :: Int
> asidBits = asidHighBits + asidLowBits

> asidRange :: (ASID, ASID)
> asidRange = (0, (1 `shiftL` asidBits) - 1)

> asidHighBitsOf :: ASID -> ASID
> asidHighBitsOf asid = (asid `shiftR` asidLowBits) .&. mask asidHighBits


