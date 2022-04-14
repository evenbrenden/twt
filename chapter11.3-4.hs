#! /usr/bin/env nix-shell
#! nix-shell chapter11.nix -i "ghcid -c 'ghci -Wall'"

{-# LANGUAGE AllowAmbiguousTypes #-}
{-# LANGUAGE ConstraintKinds #-}
{-# LANGUAGE DataKinds #-}
{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE FlexibleInstances #-}
{-# LANGUAGE GADTs #-}
{-# LANGUAGE KindSignatures #-}
{-# LANGUAGE MultiParamTypeClasses #-}
{-# LANGUAGE OverloadedLabels #-}
{-# LANGUAGE PolyKinds #-}
{-# LANGUAGE RankNTypes #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE TypeApplications #-}
{-# LANGUAGE TypeFamilies #-}
{-# LANGUAGE TypeOperators #-}
{-# LANGUAGE UndecidableInstances #-}

import           Data.Kind                      ( Type )
import           Data.Proxy                     ( Proxy(..) )
import qualified Data.Vector                   as V
import           Fcf                     hiding ( Any )
import           GHC.OverloadedLabels           ( IsLabel(..) )
import           GHC.TypeLits
import           Unsafe.Coerce                  ( unsafeCoerce )

data Any (f :: k -> Type) where
    Any ::f t -> Any f

data OpenProduct (f :: k -> Type) (ts :: [(Symbol, k)]) where
    OpenProduct ::V.Vector (Any f) -> OpenProduct f ts

nil :: OpenProduct f '[]
nil = OpenProduct V.empty

data Key (key :: Symbol) = Key

type UniqueKey (key :: k) (ts :: [(k, t)]) =
    Null =<< Filter (TyEq key <=< Fst) ts

insert
    :: Eval (UniqueKey key ts) ~ 'True
    => Key key
    -> f t
    -> OpenProduct f ts
    -> OpenProduct f ('(key, t) ': ts)
insert _ ft (OpenProduct v) = OpenProduct $ V.cons (Any ft) v

insertExample :: OpenProduct Maybe '[ '("another", Bool), '("key", [Char])]
insertExample = let hello   = insert #key (Just "hello") nil
                    another = insert #another (Just True) hello
                in another

type FindElem (key :: Symbol) (ts :: [(Symbol, k)]) =
    Eval (FromMaybe Stuck =<< FindIndex (TyEq key <=< Fst) ts)

findElem :: forall key ts . KnownNat (FindElem key ts) => Int
findElem = fromIntegral . natVal $ Proxy @(FindElem key ts)

findElemExample :: Int
findElemExample = findElem @"key" @'[ '("another", Bool), '("key", [Char])]
-- > findElemExample
-- 1

type LookupType (key :: k) (ts :: [(k, t)]) =
    FromMaybe Stuck =<< Lookup key ts

get
    :: forall key ts f
     . KnownNat (FindElem key ts)
    => Key key
    -> OpenProduct f ts
    -> f (Eval (LookupType key ts))
get _ (OpenProduct v) =
    unAny $ V.unsafeIndex v $ findElem @key @ts
  where
    unAny (Any a) = unsafeCoerce a

getExample :: Maybe String
getExample = get #key insertExample
-- > getExample
-- Just "hello"

type UpdateElem (key :: Symbol) (t :: k) (ts :: [(Symbol, k)]) =
    SetIndex (FindElem key ts) '(key, t) ts

update
    :: forall key ts t f
     . KnownNat (FindElem key ts)
    => Key key
    -> f t
    -> OpenProduct f ts
    -> OpenProduct f (Eval (UpdateElem key t ts))
update _ ft (OpenProduct v) =
    OpenProduct $ v V.// [(findElem @key @ts, Any ft)]

updateExample :: OpenProduct Maybe '[ '("another", Bool), '("key", [Char])]
updateExample = update #key (Just "bye") insertExample

getExample' :: Maybe String
getExample' = get #key updateExample
-- > getExample'
-- Just "bye"

instance (key ~ key') => IsLabel key (Key key') where
    fromLabel = Key

-- Exercise 11.3-i
-- Implement delete for OpenProducts.

type DeleteElem (key :: Symbol) (ts :: [(Symbol, k)]) =
    Filter (Not <=< TyEq key <=< Fst) ts

delete
    :: forall key ts f
     . KnownNat (FindElem key ts)
    => Key key
    -> OpenProduct f ts
    -> OpenProduct f (Eval (DeleteElem key ts))
delete _ (OpenProduct v) =
    let (a, b) = V.splitAt (findElem @key @ts) v
    in  OpenProduct $ a <> V.tail b

deleteExample :: OpenProduct Maybe '[ '("key", [Char])]
deleteExample = delete #another insertExample

-- Exercise 11.3-ii
-- Implement upsert (update or insert) for OpenProducts.
--   Hint: write a type family to compute a MAYBE NAT
-- corresponding to the index of the key in the list of
-- types, if it exists. Use class instances to lower this
-- kind to the term-level, and then pattern match on it
-- to implement upsert.

-- PASS
