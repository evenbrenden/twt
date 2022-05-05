#! /usr/bin/env nix-shell
#! nix-shell chapter15.4-singletons.nix -i "ghcid -c 'ghci -Wall'"

{-# LANGUAGE DataKinds #-}
{-# LANGUAGE EmptyCase #-}
{-# LANGUAGE FlexibleInstances #-}
{-# LANGUAGE GADTs #-}
{-# LANGUAGE InstanceSigs #-}
{-# LANGUAGE KindSignatures #-}
{-# LANGUAGE PolyKinds #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE StandaloneDeriving #-}
{-# LANGUAGE StandaloneKindSignatures #-}
{-# LANGUAGE TypeApplications #-}
{-# LANGUAGE TemplateHaskell #-}
{-# LANGUAGE TypeFamilies #-}
{-# LANGUAGE UndecidableInstances #-}

import           Data.Singletons.Prelude
import           Data.Singletons.TH

singletons [d|
    data TimeOfDay
        = Morning
        | Afternoon
        | Evening
        | Night
        deriving (Eq, Ord, Show)
    |]

-- > sing @'[ 'Morning, 'Evening]
-- SCons SMorning (SCons SEvening SNil)
