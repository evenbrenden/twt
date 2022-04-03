#! /usr/bin/env nix-shell
#! nix-shell -p ghc ghcid -i "ghcid -c 'ghci -Wall'"

{-# LANGUAGE GADTs #-}
{-# LANGUAGE RankNTypes #-}

-- Exercise 7.1-iii
-- Write the `Show` instance for `HasShow` in terms of `elimHasShow`.

data HasShow where
    HasShow ::Show t => t -> HasShow

elimHasShow :: (forall a . Show a => a -> r) -> HasShow -> r
elimHasShow f (HasShow a) = f a

instance Show HasShow where
    show hs = "HasShow " ++ elimHasShow show hs
