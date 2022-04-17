#! /usr/bin/env nix-shell
#! nix-shell -p ghc ghcid -i "ghcid -c 'ghci -Wall -Wno-orphan -Wno-orphans'"

{-# LANGUAGE DefaultSignatures #-}
{-# LANGUAGE DeriveAnyClass #-}
{-# LANGUAGE DeriveGeneric #-}
{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE StandaloneDeriving #-}
{-# LANGUAGE TypeOperators #-}

import           GHC.Generics

class GEq a where
    geq :: a x -> a x -> Bool

instance GEq U1 where
    geq U1 U1 = True

instance GEq V1 where
    geq _ _ = True

instance Eq a => GEq (K1 _1 a) where
    geq (K1 a) (K1 b) = a == b

instance (GEq a, GEq b) => GEq (a :+: b) where
    geq (L1 a1) (L1 a2) = geq a1 a2
    geq (R1 b1) (R1 b2) = geq b1 b2
    geq _       _       = False

instance (GEq a, GEq b) => GEq (a :*: b) where
    geq (a1 :*: b1) (a2 :*: b2) = geq a1 a2 && geq b1 b2

instance GEq a => GEq (M1 _x _y a) where
    geq (M1 a1) (M1 a2) = geq a1 a2

genericEq :: (Generic a, GEq (Rep a)) => a -> a -> Bool
genericEq a b = geq (from a) (from b)

data Foo a b c = F0 | F1 a | F2 a b deriving Generic

instance (Eq a, Eq b, Eq c) => Eq (Foo a b c) where
    (==) = genericEq

class MyEq a where
    eq :: a -> a -> Bool
    default eq
        :: (Generic a, GEq (Rep a))
        => a
        -> a
        -> Bool
    eq a b = geq (from a) (from b)

data Bar a b c = B0 | B1 a | B2 a b deriving (Generic, MyEq)

-- Exercise 13.2-i
-- Provide a generic instance for the Ord class

class GOrd a where
    gcompare :: a x -> a x -> Ordering

instance GOrd U1 where
    gcompare U1 U1 = EQ

instance GOrd V1 where
    gcompare _ _ = EQ

instance Ord a => GOrd (K1 _1 a) where
    gcompare (K1 a) (K1 b) = compare a b

instance (GOrd a, GOrd b) => GOrd (a :+: b) where
    gcompare (L1 a1 ) (L1 a2 ) = gcompare a1 a2
    gcompare (R1 b1 ) (R1 b2 ) = gcompare b1 b2
    gcompare (L1 _a1) (R1 _b2) = LT
    gcompare (R1 _b1) (L1 _a2) = GT

instance (GOrd a, GOrd b) => GOrd (a :*: b) where
    gcompare (a1 :*: b1) (a2 :*: b2) =
        let as = gcompare a1 a2
            bs = gcompare b1 b2
        in  if as == EQ then bs else as

instance GOrd a => GOrd (M1 _x _y a) where
    gcompare (M1 a1) (M1 a2) = gcompare a1 a2

genericOrd :: (Generic a, GOrd (Rep a)) => a -> a -> Ordering
genericOrd a b = gcompare (from a) (from b)

instance (Ord a, Ord b, Ord c) => Ord (Foo a b c) where
    compare = genericOrd

-- > genericOrd "b" "a"
-- GT

class MyOrd a where
    kompare :: a -> a -> Ordering
    default kompare
        :: (Generic a, GOrd (Rep a))
        => a
        -> a
        -> Ordering
    kompare a b = gcompare (from a) (from b)

deriving instance (Ord a, Ord b, Ord c) => MyOrd (Bar a b c)

-- Exercise 13.2-ii
-- Use GHC.Generics to implement the function exNihilo :: Maybe a. This function should give a value of Just a if a has exactly one data constructor which takes zero arguments. Otherwise, exNihilo should return Nothing.

class GExNihilo a where
    gexNihilo :: Maybe (a x)

instance GExNihilo U1 where
    gexNihilo = Just U1

instance GExNihilo V1 where
    gexNihilo = Nothing

instance GExNihilo (K1 _1 a) where
    gexNihilo = Nothing

instance GExNihilo (a :+: b) where
    gexNihilo = Nothing

instance GExNihilo (a :*: b) where
    gexNihilo = Nothing

instance GExNihilo a => GExNihilo (M1 _x _y a) where
    gexNihilo = fmap M1 gexNihilo

exNihilo :: (Generic a, GExNihilo (Rep a)) => Maybe a
exNihilo = to <$> gexNihilo

-- > exNihilo @Bool
-- Nothing
-- > exNihilo @()
-- Just ()
