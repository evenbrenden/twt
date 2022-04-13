#! /usr/bin/env nix-shell
#! nix-shell -p ghc ghcid -i "ghcid -c 'ghci -Wall'"

-- https://www.haskellforall.com/2021/04/how-to-replace-proxy-with.html

{-# LANGUAGE AllowAmbiguousTypes #-}
{-# LANGUAGE DataKinds #-}
{-# LANGUAGE FlexibleInstances #-}
{-# LANGUAGE KindSignatures #-}
{-# LANGUAGE PolyKinds #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE TypeApplications #-}
{-# LANGUAGE TypeFamilies #-}
{-# LANGUAGE TypeOperators #-}
{-# LANGUAGE UndecidableInstances #-}

import           Data.Kind                      ( Type )
import           Data.Proxy                     ( Proxy(..) )
import           GHC.TypeLits

data (a :: k1) :<< (b :: k2)
infixr 5 :<<

class HasPrintf a where
    type Printf a :: Type
    format :: String -> Printf a

instance KnownSymbol text => HasPrintf (text :: Symbol) where
    type Printf text = String
    format s = s <> (symbolVal (Proxy @text))

instance (HasPrintf a, KnownSymbol text) => HasPrintf ((text :: Symbol) :<< a) where
    type Printf (text :<< a) = Printf a
    format s = format @a (s <> symbolVal (Proxy @text))

instance (HasPrintf a, Show param) => HasPrintf ((param :: Type) :<< a) where
    type Printf (param :<< a) = param -> Printf a
    format s param = format @a (s <> show param)

instance {-# OVERLAPPING #-} HasPrintf a => HasPrintf (String :<< a) where
    type Printf (String :<< a) = String -> Printf a
    format s param = format @a (s <> param)

printf :: forall a. HasPrintf a => Printf a
printf = format @a mempty

example :: String
example = printf @(Int :<< "+" :<< Int :<< "=3") 1 2
