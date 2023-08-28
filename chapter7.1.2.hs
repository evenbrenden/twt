#! /usr/bin/env nix-shell
#! nix-shell --pure -i "ghcid -c 'ghci -Wall'"

{-# LANGUAGE ConstraintKinds #-}
{-# LANGUAGE FlexibleInstances #-}
{-# LANGUAGE GADTs #-}
{-# LANGUAGE KindSignatures #-}
{-# LANGUAGE RankNTypes #-}
{-# LANGUAGE UndecidableInstances #-}

import           Data.Kind                      ( Constraint
                                                , Type
                                                )
import           Data.Typeable

data Has (c :: Type -> Constraint) where
    Has ::c t => t -> Has c

elimHas :: (forall a . c a => a -> r) -> Has c -> r
elimHas f (Has a) = f a

type HasShow = Has Show
type Dynamic = Has Typeable

isMempty :: (Monoid a, Eq a) => a -> Bool
isMempty a = a == mempty

class (Monoid a, Eq a) => MonoidEq a
instance (Monoid a, Eq a) => MonoidEq a

foo :: Has MonoidEq
foo = Has [True]

bar :: Has MonoidEq
bar = Has EQ

testFoo :: Bool
testFoo = elimHas isMempty foo

testBar :: Bool
testBar = elimHas isMempty bar
