#! /usr/bin/env nix-shell
#! nix-shell -p ghc ghcid -i "ghcid -c 'ghci -Wall'"

{-# LANGUAGE ConstraintKinds #-}
{-# LANGUAGE DataKinds #-}
{-# LANGUAGE GADTs #-}
{-# LANGUAGE FlexibleInstances #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE TypeApplications #-}
{-# LANGUAGE TypeFamilies #-}
{-# LANGUAGE TypeOperators #-}
{-# LANGUAGE UndecidableInstances #-}

import           Data.Kind                      ( Constraint
                                                , Type
                                                )

data HList (ts :: [Type]) where
    HNil ::HList '[]
    (:#) ::t -> HList ts -> HList (t ': ts)
infixr 5 :#

hLength :: HList ts -> Int
hLength HNil      = 0
hLength (_ :# ts) = 1 + hLength ts

hHead :: HList (t ': ts) -> t
hHead (t :# _) = t

showBool :: HList '[_1 , Bool , _2] -> String
showBool (_ :# b :# _ :# HNil) = show b

instance Eq (HList '[]) where
    HNil == HNil = True

instance (Eq t, Eq (HList ts)) => Eq (HList (t ': ts)) where
    (a :# as) == (b :# bs) = a == b && as == bs

-- Implement Ord for HList

instance Ord (HList '[]) where
    compare HNil HNil = EQ

instance (Ord t, Ord (HList ts)) => Ord (HList (t ': ts)) where
    compare (a :# as) (b :# bs) = case compare a b of
        EQ -> compare as bs
        c  -> c

-- Implement Show for HList

instance Show (HList '[]) where
    show HNil = "HNil"

instance (Show t, Show (HList ts)) => Show (HList (t ': ts)) where
    show (a :# as) = show a <> " :# " <> show as
