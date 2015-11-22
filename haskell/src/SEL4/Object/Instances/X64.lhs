% Copyright 2014, General Dynamics C4 Systems
%
% This software may be distributed and modified according to the terms of
% the GNU General Public License version 2. Note that NO WARRANTY is provided.
% See "LICENSE_GPLv2.txt" for details.
%
% @TAG(GD_GPL)
%

This module defines instances of "PSpaceStorable" for X64-specific kernel objects. This includes page table and page directory entries, and ASID pools.

> module SEL4.Object.Instances.X64 where

\begin{impdetails}

> import SEL4.Machine.Hardware.X64(PTE(..), PDE(..), PDPTE(..), PML4E(..))
> import SEL4.Object.Structures
> import SEL4.Model
> import Data.Helpers

\end{impdetails}

> instance PSpaceStorable PDE where
>     makeObject = InvalidPDE
>     injectKO = KOArch . KOPDE
>     projectKO o = case o of 
>                   KOArch (KOPDE p) -> return p
>                   _ -> typeError "PDE" o

> instance PSpaceStorable PTE where
>     makeObject = InvalidPTE
>     injectKO = KOArch . KOPTE
>     projectKO o = case o of 
>                 KOArch (KOPTE p) -> return p
>                 _ -> typeError "PTE" o

> instance PSpaceStorable PDPTE where
>     makeObject = InvalidPDPTE
>     injectKO = KOArch . KOPDPTE
>     projectKO o = case o of 
>                 KOArch (KOPDPTE p) -> return p
>                 _ -> typeError "PDPTE" o

> instance PSpaceStorable PML4E where
>     makeObject = InvalidPML4E
>     injectKO = KOArch . KOPML4E
>     projectKO o = case o of 
>                 KOArch (KOPML4E p) -> return p
>                 _ -> typeError "PML4E" o

FIXME x64: What is the max size of an asidpool?

> instance PSpaceStorable ASIDPool where
>     makeObject = ASIDPool $
>         funPartialArray (const Nothing) (0,1023)
>     injectKO = KOArch . KOASIDPool
>     projectKO o = case o of
>         KOArch (KOASIDPool e) -> return e
>         _ -> typeError "ASID pool" o

