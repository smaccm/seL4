% Copyright 2014, General Dynamics C4 Systems
%
% This software may be distributed and modified according to the terms of
% the GNU General Public License version 2. Note that NO WARRANTY is provided.
% See "LICENSE_GPLv2.txt" for details.
%
% @TAG(GD_GPL)
%

This module contains an instance of the machine-specific kernel API for the ARM architecture.

> module SEL4.API.Types.X64 where

> import SEL4.API.Types.Universal(APIObjectType, apiGetObjectSize)
> import SEL4.Machine.Hardware.X64

FIXME There are ??? x86 64bit-specific object types:
virtual pages, page tables, and page directories.

> data ObjectType
>     = APIObjectType APIObjectType
>     | SmallPageObject
>     | LargePageObject
>     | HugePageObject
>     | PageTableObject
>     | PageDirectoryObject
>     | PDPointerTableObject
>     | PML4Object
>     deriving (Show, Eq)

> instance Bounded ObjectType where
>     minBound = APIObjectType minBound
>     maxBound = PML4Object

> instance Enum ObjectType where
>     fromEnum e = case e of
>         APIObjectType a -> fromEnum a
>         SmallPageObject -> apiMax + 1
>         LargePageObject -> apiMax + 2
>         HugePageObject -> apiMax + 3
>         PageTableObject -> apiMax + 4
>         PageDirectoryObject -> apiMax + 5
>         PDPointerTableObject -> apiMax + 6
>         PML4Object -> apiMax + 7
>         where apiMax = fromEnum (maxBound :: APIObjectType)
>     toEnum n
>         | n <= apiMax = APIObjectType $ toEnum n
>         | n == apiMax + 1 = SmallPageObject
>         | n == apiMax + 2 = LargePageObject
>         | n == apiMax + 3 = HugePageObject
>         | n == apiMax + 4 = PageTableObject
>         | n == apiMax + 5 = PageDirectoryObject
>         | n == apiMax + 6 = PDPointerTableObject
>         | n == apiMax + 7 = PML4Object
>         | otherwise = error "toEnum out of range for ARM.ObjectType"
>         where apiMax = fromEnum (maxBound :: APIObjectType)

> fromAPIType = APIObjectType

> toAPIType (APIObjectType a) = Just a
> toAPIType _ = Nothing

> pageType = SmallPageObject

> getObjectSize :: ObjectType -> Int -> Int
> getObjectSize SmallPageObject _ = pageBitsForSize X64Page
> getObjectSize LargePageObject _ = pageBitsForSize X64LargePage
> getObjectSize HugePageObject _ = pageBitsForSize X64HugePage
> getObjectSize PageTableObject _ = ptBits
> getObjectSize PageDirectoryObject _ = ptBits
> getObjectSize PDPointerTableObject _ = ptBits
> getObjectSize PML4Object _ = ptBits
> getObjectSize (APIObjectType apiObjectType) size = apiGetObjectSize apiObjectType size

