#! /usr/bin/env nix-shell
#! nix-shell -p ghc ghcid -i "ghcid -c 'ghci -Wall'"

{-# LANGUAGE DataKinds #-}
{-# LANGUAGE KindSignatures #-}
{-# LANGUAGE PolyKinds #-}
{-# LANGUAGE TypeFamilies #-}
{-# LANGUAGE TypeInType #-}
{-# LANGUAGE TypeOperators #-}
{-# LANGUAGE UndecidableInstances #-}

import           Data.Kind                      ( Type )

-- Exercise 10.2-i
-- Defunctionalize listToMaybe at the type-level.

type Exp a = a -> Type

type family Eval (e :: Exp a) :: a

data ListToMaybe :: [a] -> Exp (Maybe a)
type instance Eval (ListToMaybe '[]) = 'Nothing
type instance Eval (ListToMaybe (a ': _1)) = 'Just a

-- Exercise 10.2-ii
-- Defunctionalize foldr :: (a -> b -> b) -> b -> [a] -> b.

data Foldr :: (a -> b -> Exp b) -> b -> [a] -> Exp b
type instance Eval (Foldr _1 b '[]) = b
type instance Eval (Foldr f b (a ': as)) = Eval (f a (Eval (Foldr f b as)))

data FlipConst :: a -> b -> Exp b
type instance Eval (FlipConst a b) = b

-- > :kind! Eval (Foldr FlipConst "c" '["a", "b"])
-- Eval (Foldr FlipConst "c" '["a", "b"]) :: GHC.Types.Symbol
-- = "c"
