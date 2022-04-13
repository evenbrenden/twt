#! /usr/bin/env nix-shell
#! nix-shell -p ghc ghcid -i "ghcid -c 'ghci -Wall'"

{-# LANGUAGE FlexibleInstances #-}
{-# LANGUAGE FunctionalDependencies #-}
{-# LANGUAGE MultiParamTypeClasses #-}
{-# LANGUAGE TypeFamilies #-}
{-# LANGUAGE UndecidableInstances #-}

import           Prelude                 hiding ( fst )

-- Exercise 10.1-i
-- Defunctionalize listToMaybe :: [a] -> Maybe a.

class Eval l t | l -> t where
    eval :: l -> t

data ListToMaybe a = ListToMaybe [a]

instance Eval (ListToMaybe a) (Maybe a) where
    eval (ListToMaybe []     ) = Nothing
    eval (ListToMaybe (a : _)) = Just a
