#! /usr/bin/env nix-shell
#! nix-shell chapter15.nix -i "ghcid -c 'ghci -Wall -Wno-unused-imports'"

{-# LANGUAGE ConstraintKinds #-}
{-# LANGUAGE EmptyCase #-}
{-# LANGUAGE FlexibleInstances #-}
{-# LANGUAGE GADTs #-}
{-# LANGUAGE InstanceSigs #-}
{-# LANGUAGE MultiParamTypeClasses #-}
{-# LANGUAGE RankNTypes #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE StandaloneDeriving #-}
{-# LANGUAGE TemplateHaskell #-}
{-# LANGUAGE TypeApplications #-}
{-# LANGUAGE TypeFamilies #-}
{-# LANGUAGE TypeInType #-}
{-# LANGUAGE UndecidableInstances #-}

import           Data.Aeson
import           Data.Constraint
import           Data.Kind                      ( Type )
import           Data.Maybe                     ( mapMaybe )
import           Data.Singletons.Prelude
import           Data.Singletons.TH

data Sigma (f :: k -> Type) where
    Sigma ::Sing a -> f a -> Sigma f

withSigma :: (forall (a :: k) . Sing a -> f a -> r) -> Sigma f -> r
withSigma c (Sigma s f) = c s f

toSigma :: SingI a => f a -> Sigma f
toSigma fa = Sigma sing fa

fromSigma
    :: forall k (a :: k) (f :: k -> Type)
     . (SingI a, SDecide k)
    => Sigma f
    -> Maybe (f a)
fromSigma (Sigma s f) = case s %~ sing @a of
    Proved    Refl -> Just f
    Disproved _    -> Nothing

class Dict1 c (f :: k -> Type) where
    dict1 :: Sing (a :: k) -> Dict (c (f a))

instance ( Dict1 Eq (f :: k -> Type)
         , SDecide k
         ) => Eq (Sigma f) where
    Sigma sa fa == Sigma sb fb = case sa %~ sb of
        Proved Refl -> case dict1 @_ @Eq @f sa of
            Dict -> fa == fb
        Disproved _ -> False

instance ( Dict1 Show (f :: k -> Type)
         , Show (Demote k)
         , SingKind k
         ) => Show (Sigma f) where
    show (Sigma sa fa) = case dict1 @_ @Show @f sa of
        Dict -> mconcat ["Sigma ", show $ fromSing sa, " (", show fa, ")"]

-- Exercise 15.5-i
-- Provide an instance of Ord for Sigma by comparing
-- the fs if the singletons are equal, comparing the
-- singletons at the term-level otherwise.

-- PASS
