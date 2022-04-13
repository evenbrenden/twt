#! /usr/bin/env nix-shell
#! nix-shell -p ghc ghcid -i "ghcid -c 'ghci -Wall'"

{-# LANGUAGE DataKinds #-}
{-# LANGUAGE KindSignatures #-}
{-# LANGUAGE PolyKinds #-}
{-# LANGUAGE TypeFamilies #-}
{-# LANGUAGE TypeOperators #-}

import           Data.Kind                      ( Constraint
                                                , Type
                                                )

type Exp a = a -> Type
type family Eval (e :: Exp a) :: a

data Map :: (a -> Exp b) -> f a -> Exp (f b)
type instance Eval (Map f '[]) = '[]
type instance Eval (Map f (a ': as)) = Eval (f a) ': Eval (Map f as)

data (++) :: [a] -> [a] -> Exp [a]
type instance Eval ((++) '[] ys) = ys
type instance Eval ((++) (x ': xs) ys) = x ': Eval ((++) xs ys)

data Mappend :: a -> a -> Exp a
type instance Eval (Mappend '() '()) = '()
type instance Eval (Mappend (a :: Constraint) (b :: Constraint)) = (a, b)
type instance Eval (Mappend (a :: [k]) (b :: [k])) = Eval (a ++ b)

data Mempty :: k -> Exp k
type instance Eval (Mempty '()) = '()
type instance Eval (Mempty (_1 :: Constraint)) = (() :: Constraint)
type instance Eval (Mempty (_1 :: [k])) = '[]

-- > :k! Eval (Mempty '["a"])
-- Eval (Mempty '["a"]) :: [GHC.Types.Symbol]
-- = '[]
-- > :k! Eval (Mappend '["a"] '["b"])
-- Eval (Mappend '["a"] '["b"]) :: [GHC.Types.Symbol]
-- = '["a", "b"]

-- Exercise 10.4-i
-- Write a promoted functor instance for tuples.

type instance Eval (Map f '(a, b))
    = '(a, Eval (f b))

data Nothing :: a -> Exp (Maybe a)
type instance Eval (Nothing _1) = 'Nothing

-- > :k! Eval (Map Nothing '(1, 2))
-- Eval (Map Nothing '(1, 2)) :: (GHC.Types.Nat, Maybe GHC.Types.Nat)
-- = '(1, 'Nothing)
