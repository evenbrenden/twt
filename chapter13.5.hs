#! /usr/bin/env nix-shell
#! nix-shell chapter13.5.nix -i "ghcid -c 'ghci -Wall -Wno-unused-imports'"

{-# LANGUAGE RankNTypes #-}
{-# LANGUAGE TypeApplications #-}

import           Control.Monad.Codensity
import           Data.Functor.Day.Curried
import           Data.Functor.Yoneda

newtype Nobuo f a = Nobuo
    { runNobuo :: forall b. (a -> b) -> f b
    }

instance Functor (Nobuo f) where
    fmap f (Nobuo y) = Nobuo (\k -> y (k . f))

-- > :t runNobuo
-- runNobuo :: Nobuo f a -> (a -> b) -> f b
-- > :t flip fmap
-- flip fmap :: Functor f => f a -> (a -> b) -> f b
