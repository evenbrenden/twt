#! /usr/bin/env nix-shell
#! nix-shell --pure -i "ghcid -c 'ghci -Wall'"

{-# LANGUAGE AllowAmbiguousTypes #-}
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

type FindElem (key :: Symbol) (ts :: [(Symbol, k)]) =
    Eval (FromMaybe Stuck =<< FindIndex (TyEq key <=< Fst) ts)

findElem :: forall key ts . KnownNat (FindElem key ts) => Int
findElem = fromIntegral . natVal $ Proxy @(FindElem key ts)

-- Exercise 12-i
-- Add helpful type errors to OpenProduct's update and delete functions.

type family FriendlyFindElem
        (caller :: Symbol)
        (key :: Symbol)
        (ts :: [(Symbol, k)]) where
    FriendlyFindElem caller key ts =
            FromMaybe
                ( TypeError
                ( 'Text "Attempted to call '"
            ':<>: 'Text caller
            ':<>: 'Text "' with the key '"
            ':<>: 'Text key
            ':<>: 'Text "'."
            ':$$: 'Text "But the OpenProduct has the keys:"
            ':$$: 'Text "  "
            ':<>: 'ShowType (Eval (Map Fst ts))
                )) =<< FindIndex (TyEq key <=< Fst) ts

type UpdateElem (key :: Symbol) (t :: k) (ts :: [(Symbol, k)]) =
    SetIndex (FindElem key ts) '(key, t) ts

friendlyUpdate
    :: forall key ts t f
     . ( KnownNat (Eval (FriendlyFindElem "friendlyUpdate" key ts))
       , KnownNat (FindElem key ts))
    => Key key
    -> f t
    -> OpenProduct f ts
    -> OpenProduct f (Eval (UpdateElem key t ts))
friendlyUpdate _ ft (OpenProduct v) =
    OpenProduct $ v V.// [(findElem @key @ts, Any ft)]

-- > :set -XOverloadedLabels
-- > friendlyUpdate #newkey (Just "yo") insertExample
-- ...
--     • Attempted to call 'friendlyUpdate' with the key 'newkey'.
--       But the OpenProduct has the keys:
--         '["another", "key"]
-- ...

type DeleteElem (key :: Symbol) (ts :: [(Symbol, k)]) =
    Filter (Not <=< TyEq key <=< Fst) ts

friendlyDelete
    :: forall key ts f
     . ( KnownNat (Eval (FriendlyFindElem "friendlyDelete" key ts))
       , KnownNat (FindElem key ts))
    => Key key
    -> OpenProduct f ts
    -> OpenProduct f (Eval (DeleteElem key ts))
friendlyDelete _ (OpenProduct v) =
    let (a, b) = V.splitAt (findElem @key @ts) v
    in  OpenProduct $ a <> V.tail b

-- > :set -XOverloadedLabels
-- > friendlyDelete #newkey insertExample
-- ...
--     • Attempted to call 'friendlyDelete' with the key 'newkey'.
--       But the OpenProduct has the keys:
--         '["another", "key"]
-- ...

-- Exercise 12-ii
-- Write a closed type family of kind [K] -> ERRORMESSAGE that pretty prints a list. Use it to improve the error message from FriendlyFindElem.

type family ShowList (ts :: [k]) :: ErrorMessage where
    ShowList '[]       = 'Text ""
    ShowList (a : '[]) = 'ShowType a
    ShowList (a : as)  = 'ShowType a ':<>: 'Text ", " ':<>: ShowList as

type family FriendlierFindElem
        (caller :: Symbol)
        (key :: Symbol)
        (ts :: [(Symbol, k)]) where
    FriendlierFindElem caller key ts =
            FromMaybe
                ( TypeError
                ( 'Text "Attempted to call '"
            ':<>: 'Text caller
            ':<>: 'Text "' with the key '"
            ':<>: 'Text key
            ':<>: 'Text "'."
            ':$$: 'Text "But the OpenProduct has the keys:"
            ':$$: 'Text "  "
            ':<>: ShowList (Eval (Map Fst ts))
                )) =<< FindIndex (TyEq key <=< Fst) ts

friendlierDelete
    :: forall key ts f
     . ( KnownNat (Eval (FriendlierFindElem "friendlyDelete" key ts))
       , KnownNat (FindElem key ts))
    => Key key
    -> OpenProduct f ts
    -> OpenProduct f (Eval (DeleteElem key ts))
friendlierDelete _ (OpenProduct v) =
    let (a, b) = V.splitAt (findElem @key @ts) v
    in  OpenProduct $ a <> V.tail b

-- > :set -XOverloadedLabels
-- > friendlierDelete #newkey insertExample
-- ...
--     • Attempted to call 'friendlyDelete' with the key 'newkey'.
--       But the OpenProduct has the keys:
--         "another", "key"
-- ...
