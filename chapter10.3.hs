#! /usr/bin/env nix-shell
#! nix-shell --pure -i "ghcid -c 'ghci -Wall'"

{-# LANGUAGE DataKinds #-}
{-# LANGUAGE KindSignatures #-}
{-# LANGUAGE PolyKinds #-}
{-# LANGUAGE TypeFamilies #-}
{-# LANGUAGE TypeOperators #-}
{-# LANGUAGE UndecidableInstances #-}

import           Data.Kind                      ( Constraint
                                                , Type
                                                )

type Exp a = a -> Type
type family Eval (e :: Exp a) :: a

data Snd :: (a, b) -> Exp b
type instance Eval (Snd '(a, b)) = b

data MapList :: (a -> Exp b) -> [a] -> Exp [b]
type instance Eval (MapList f '[]) = '[]
type instance Eval (MapList f (a ': as)) = Eval (f a) ': Eval (MapList f as)

data FromMaybe :: a -> Maybe a -> Exp a
type instance Eval (FromMaybe _1 ( 'Just a)) = a
type instance Eval (FromMaybe a 'Nothing) = a

data Pure :: a -> Exp a
type instance Eval (Pure x) = x

data (=<<) :: (a -> Exp b) -> Exp a -> Exp b
type instance Eval (k =<< e) = Eval (k (Eval e))
infixr 0 =<<

data (<=<) :: (b -> Exp c) -> (a -> Exp b) -> a -> Exp c
type instance Eval ((f <=< g) x) = Eval (f (Eval (g x)))
infixr 1 <=<

data TyEq :: a -> b -> Exp Bool
type instance Eval (TyEq a b) = TyEqImpl a b
type family TyEqImpl (a :: k) (b :: k) :: Bool where
    TyEqImpl a a = 'True
    TyEqImpl a b = 'False

-- > :k! Eval (TyEq 1 1)
-- Eval (TyEq 1 1) :: Bool
-- = 'True
-- > :k! Eval (TyEq "a" "b")
-- Eval (TyEq "a" "b") :: Bool
-- = 'False

data Collapse :: [Constraint] -> Exp Constraint
type instance Eval (Collapse '[]) = (() :: Constraint)
type instance Eval (Collapse (a ': as)) = (a, Eval (Collapse as))

data Pure1 :: (a -> b) -> a -> Exp b
type instance Eval (Pure1 f x) = f x

type All (c :: k -> Constraint) (ts :: [k]) = Collapse =<< MapList (Pure1 c) ts

-- > :k! Eval (All Eq '[Int, Bool])
-- Eval (All Eq '[Int, Bool]) :: Constraint
-- = (Eq Int, (Eq Bool, () :: Constraint))
