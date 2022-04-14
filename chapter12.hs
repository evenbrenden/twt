#! /usr/bin/env nix-shell
#! nix-shell chapter11.nix -i "ghcid -c 'ghci -Wall -Wno-missing-methods -Wno-orphans'"

{-# LANGUAGE AllowAmbiguousTypes #-}
{-# LANGUAGE ConstraintKinds #-}
{-# LANGUAGE DataKinds #-}
{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE FlexibleInstances #-}
{-# LANGUAGE GADTs #-}
{-# LANGUAGE MultiParamTypeClasses #-}
{-# LANGUAGE OverloadedLabels #-}
{-# LANGUAGE PolyKinds #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE StandaloneKindSignatures #-}
{-# LANGUAGE TypeApplications #-}
{-# LANGUAGE TypeFamilies #-}
{-# LANGUAGE TypeOperators #-}
{-# LANGUAGE UndecidableInstances #-}

import           Data.Kind                      ( Constraint
                                                , Type
                                                )
import           Data.Proxy
import qualified Data.Vector                   as V
import           Fcf                     hiding ( Any )
import           GHC.OverloadedLabels           ( IsLabel(..) )
import           GHC.TypeLits            hiding ( type (+) )
import           Unsafe.Coerce

instance
    ( TypeError
        ( 'Text "Attempting to interpret a number as a function"
     ':$$: 'Text "in the type '"
     ':<>: 'ShowType (a -> b)
     ':<>: 'Text "'"
     ':$$: 'Text "Did you forget to specify the function you wanted?"
        )
    ) => Num (a -> b) where

-- OpenSum

type FriendlyFindElem :: (k -> Type) -> k -> [k] -> Exp Nat
type family FriendlyFindElem (f :: k -> Type) (t :: k) (ts :: [k]) where
    FriendlyFindElem f t ts =
        FromMaybe
            ( TypeError
            ( 'Text "Attempted to call 'friendlyPrj' to produce a '"
        ':<>: 'ShowType (f t)
        ':<>: 'Text "'."
        ':$$: 'Text "But the OpenSum can only contain one of:"
        ':$$: 'Text "  "
        ':<>: 'ShowType ts
            )) =<< FindIndex (TyEq t) ts

friendlyPrj
    :: forall f t ts
     . (KnownNat (Eval (FriendlyFindElem f t ts)), Member t ts)
    => OpenSum f ts
    -> Maybe (f t)
friendlyPrj (UnsafeOpenSum i f) =
    if i == findElem @t @ts then Just $ unsafeCoerce f else Nothing

data OpenSum (f :: k -> Type) (ts :: [k]) where
    UnsafeOpenSum ::Int -> f t -> OpenSum f ts

type FindElem (key :: k) (ts :: [k])
    = FromMaybe Stuck =<< FindIndex (TyEq key) ts

type Member t ts = KnownNat (Eval (FindElem t ts))

findElem :: forall t ts . Member t ts => Int
findElem = fromIntegral . natVal $ Proxy @(Eval (FindElem t ts))

inj :: forall f t ts . Member t ts => f t -> OpenSum f ts
inj = UnsafeOpenSum (findElem @t @ts)

-- OpenProduct

type family RequireUniqueKey
        (result :: Bool)
        (key :: Symbol)
        (t :: k)
        (ts :: [(Symbol, k)]) :: Constraint where
    RequireUniqueKey 'True key t ts = ()
    RequireUniqueKey 'False key t ts =
        TypeError
            ( 'Text "Attempting to add a field named '"
        ':<>: 'Text key
        ':<>: 'Text "' with type "
        ':<>: 'ShowType t
        ':<>: 'Text " to an OpenProduct."
        ':$$: 'Text "But the OpenProduct already has a field '"
        ':<>: 'Text key
        ':<>: 'Text "' with type "
        ':<>: 'ShowType (LookupType key ts)
        ':$$: 'Text "Consider using 'update' "
        ':<>: 'Text "instead of 'insert'."
            )

friendlyInsert
    :: RequireUniqueKey (Eval (UniqueKey key ts)) key t ts
    => Key key
    -> f t
    -> OpenProduct f ts
    -> OpenProduct f ('(key, t) ': ts)
friendlyInsert _ ft (OpenProduct v) = OpenProduct $ V.cons (Any ft) v

data Any (f :: k -> Type) where
    Any ::f t -> Any f

data OpenProduct (f :: k -> Type) (ts :: [(Symbol, k)]) where
    OpenProduct ::V.Vector (Any f) -> OpenProduct f ts

nil :: OpenProduct f '[]
nil = OpenProduct V.empty

data Key (key :: Symbol) = Key

type UniqueKey (key :: k) (ts :: [(k, t)]) =
    Null =<< Filter (TyEq key <=< Fst) ts

type LookupType (key :: k) (ts :: [(k, t)]) =
    FromMaybe Stuck =<< Lookup key ts

instance (key ~ key') => IsLabel key (Key key') where
    fromLabel = Key

insertExample :: OpenProduct Maybe '[ '("another", Bool), '("key", [Char])]
insertExample = let hello   = friendlyInsert #key (Just "hello") nil
                    another = friendlyInsert #another (Just True) hello
                in another
