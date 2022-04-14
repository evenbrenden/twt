#! /usr/bin/env nix-shell
#! nix-shell chapter11.nix -i "ghcid -c 'ghci -Wall'"

{-# LANGUAGE AllowAmbiguousTypes #-}
{-# LANGUAGE ConstraintKinds #-}
{-# LANGUAGE DataKinds #-}
{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE FlexibleInstances #-}
{-# LANGUAGE GADTs #-}
{-# LANGUAGE PolyKinds #-}
{-# LANGUAGE RankNTypes #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE TypeApplications #-}
{-# LANGUAGE TypeFamilies #-}
{-# LANGUAGE TypeOperators #-}
{-# LANGUAGE UndecidableInstances #-}

import           Data.Functor.Identity
import           Data.Kind                      ( Type )
import           Data.Proxy
import           Fcf
import           GHC.TypeLits            hiding ( type (+) )
import           Unsafe.Coerce

data OpenSum (f :: k -> Type) (ts :: [k]) where
    UnsafeOpenSum ::Int -> f t -> OpenSum f ts

type FindElem (key :: k) (ts :: [k])
    = FromMaybe Stuck =<< FindIndex (TyEq key) ts

type Member t ts = KnownNat (Eval (FindElem t ts))

findElem :: forall t ts . Member t ts => Int
findElem = fromIntegral . natVal $ Proxy @(Eval (FindElem t ts))

findElemExample :: Int
findElemExample = findElem @Bool @'[Int , Bool , String]
-- > findElemExample
-- 1

inj :: forall f t ts . Member t ts => f t -> OpenSum f ts
inj = UnsafeOpenSum (findElem @t @ts)

injExample :: OpenSum Identity '[Bool , Int]
injExample = inj @Identity @Int @'[Bool , Int] $ Identity 99

prj :: forall f t ts . Member t ts => OpenSum f ts -> Maybe (f t)
prj (UnsafeOpenSum i f) =
    if i == findElem @t @ts then Just $ unsafeCoerce f else Nothing

prjExample :: Maybe Int
prjExample = fmap runIdentity $ prj @Identity @Int @'[Bool , Int] injExample
-- > prjExample
-- Just 99

decompose :: OpenSum f (t ': ts) -> Either (f t) (OpenSum f ts)
decompose (UnsafeOpenSum 0 t) = Left $ unsafeCoerce t
decompose (UnsafeOpenSum n t) = Right $ UnsafeOpenSum (n - 1) t

decomposeExample
    :: Either (Identity Bool) (Either (Identity Int) (OpenSum Identity '[]))
decomposeExample = fmap decompose $ decompose injExample

match :: forall f ts b . (forall t . f t -> b) -> OpenSum f ts -> b
match fn (UnsafeOpenSum _ t) = fn t

matchExample :: String
matchExample = match (const "🤯") injExample

-- Exercise 11.2-i
-- Write weaken :: OpenSum f ts -> OpenSum f (x ': ts)

weaken :: OpenSum f ts -> OpenSum f (x ': ts)
weaken (UnsafeOpenSum n t) = UnsafeOpenSum (n + 1) t

weakenExample :: OpenSum Identity '[String , Bool , Int]
weakenExample = weaken @Identity @'[Bool , Int] injExample
