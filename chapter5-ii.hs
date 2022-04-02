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

type family All (c :: Type -> Constraint) (ts :: [Type]) :: Constraint where
    All c '[] = ()
    All c (t ': ts) = (c t, All c ts)

instance All Eq ts => Eq (HList ts) where
    HNil      == HNil      = True
    (a :# as) == (b :# bs) = a == b && as == bs

--- Exercise 5.3-iii
-- Rewrite the Ord and Show instances in terms of All.

instance (All Eq ts, All Ord ts) => Ord (HList ts) where
    compare HNil      HNil      = EQ
    compare (a :# as) (b :# bs) = case compare a b of
        EQ -> compare as bs
        c  -> c

instance All Show ts => Show (HList ts) where
    show HNil      = "HNil"
    show (a :# as) = show a <> " :# " <> show as
