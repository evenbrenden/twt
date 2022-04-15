#! /usr/bin/env nix-shell
#! nix-shell chapter12.nix -i "ghcid -c 'ghci -Wall'"

{-# LANGUAGE AllowAmbiguousTypes #-}
{-# LANGUAGE ConstraintKinds #-}
{-# LANGUAGE DataKinds #-}
{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE GADTs #-}
{-# LANGUAGE PolyKinds #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE StandaloneKindSignatures #-}
{-# LANGUAGE TypeApplications #-}
{-# LANGUAGE TypeFamilies #-}
{-# LANGUAGE TypeOperators #-}
{-# LANGUAGE UndecidableInstances #-}

import           Data.Kind                      ( Type )
import           Data.Proxy
import           Fcf                     hiding ( Any )
import           GHC.TypeLits            hiding ( type (+) )
import           Unsafe.Coerce

type family FriendlyFindElem
        (f :: k -> Type)
        (t :: k)
        (ts :: [k]) where
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
