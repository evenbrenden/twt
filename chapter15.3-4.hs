#! /usr/bin/env nix-shell
#! nix-shell -p ghc ghcid -i "ghcid -c 'ghci -Wall -Wno-unused-top-binds'"

{-# LANGUAGE DataKinds #-}
{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE FlexibleInstances #-}
{-# LANGUAGE GADTs #-}
{-# LANGUAGE KindSignatures #-}
{-# LANGUAGE PolyKinds #-}
{-# LANGUAGE RankNTypes #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE TypeFamilies #-}
{-# LANGUAGE TypeFamilyDependencies #-}
{-# LANGUAGE TypeInType #-}
{-# LANGUAGE TypeOperators #-}
{-# LANGUAGE UndecidableInstances #-}

import           Data.Void
import           Unsafe.Coerce                  ( unsafeCoerce )

data family Sing (a :: k)

data SomeSing k where
    SomeSing ::Sing (a :: k) -> SomeSing k

withSomeSing :: SomeSing k -> (forall (a :: k) . Sing a -> r) -> r
withSomeSing (SomeSing s) f = f s

class SingKind k where
    type Demote k = r | r -> k
    toSing :: Demote k -> SomeSing k
    fromSing :: Sing (a :: k) -> Demote k

-- Bool
data instance Sing (a :: Bool) where
    STrue ::Sing 'True
    SFalse ::Sing 'False

instance SingKind Bool where
    type Demote Bool = Bool
    toSing True  = SomeSing STrue
    toSing False = SomeSing SFalse
    fromSing STrue  = True
    fromSing SFalse = False

class SingI (a :: k) where
    sing :: Sing a

instance SingI 'True where
    sing = STrue

instance SingI 'False where
    sing = SFalse

-- Maybe
data instance Sing (a :: Maybe k) where
    SJust ::Sing (a :: k) -> Sing ('Just a)
    SNothing ::Sing 'Nothing

instance (k ~ Demote k, SingKind k) => SingKind (Maybe k) where
    type Demote (Maybe k) = Maybe k
    toSing (Just a) = withSomeSing (toSing a) $ SomeSing . SJust
    toSing Nothing  = SomeSing SNothing
    fromSing (SJust a) = Just $ fromSing a
    fromSing SNothing  = Nothing

-- > withSomeSing (toSing (Just True)) fromSing
-- Just True
-- > withSomeSing (toSing (Just False)) fromSing
-- Just False
-- > withSomeSing (toSing @(Maybe Bool) Nothing) fromSing
-- Nothing

instance SingI a => SingI ('Just a) where
    sing = SJust sing

instance SingI 'Nothing where
    sing = SNothing

-- List
data instance Sing (a :: [k]) where
    SNil ::Sing '[]
    SCons ::Sing (h :: k)
          -> Sing (t :: [k])
          -> Sing (h ': t)

instance (k ~ Demote k, SingKind k) => SingKind [k] where
    type Demote [k] = [k]
    toSing []      = SomeSing SNil
    toSing (h : t) = withSomeSing (toSing h)
        $ \sh -> withSomeSing (toSing t) $ \st -> SomeSing $ SCons sh st
    fromSing SNil          = []
    fromSing (SCons sh st) = fromSing sh : fromSing st

-- > withSomeSing (toSing [True, False]) fromSing
-- [True,False]

-- Exercise 15.3-i
-- Provide instances of SingI for lists.

instance SingI '[] where
    sing = SNil

instance (SingI h, SingI t) => SingI (h ': t) where
    sing = SCons sing sing

-- > :set -XDataKinds
-- > :set -XTypeApplications
-- > sing @'[ 'True, 'False]
--
-- <interactive>:3:7: error:
--     • Expected a type, but ‘'[ 'True, 'False]’ has kind ‘[Bool]’
--     • In the type ‘'[ 'True, 'False]’
--       In the expression: sing @'[ 'True, 'False]
--       In an equation for ‘it’: it = sing @'[ 'True, 'False]

-- Decisions, Decisions
class SDecide k where
    (%~) :: Sing (a ::k ) -> Sing (b :: k) -> Decision (a :~: b)

data a :~: b where
    Refl ::a :~: a

data Decision a
    = Proved a
    | Disproved (a -> Void)

instance (Eq (Demote k), SingKind k) => SDecide k where
    a %~ b = if fromSing a == fromSing b
        then Proved $ unsafeCoerce Refl
        else Disproved $ const undefined

instance SDecide Bool where
    STrue  %~ STrue  = Proved Refl
    SFalse %~ SFalse = Proved Refl
    _      %~ _      = Disproved $ const undefined

-- Exercise 15.4-i
-- Give instances of SDecide for Maybe.

instance SDecide a => SDecide (Maybe a) where
    (SJust a) %~ (SJust b) = case a %~ b of
        Proved    Refl -> Proved Refl
        Disproved _    -> Disproved $ const undefined
    _ %~ _ = Disproved $ const undefined
