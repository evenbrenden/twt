#! /usr/bin/env nix-shell
#! nix-shell -p ghc ghcid -i "ghcid -c 'ghci -Wall'"

{-# LANGUAGE FunctionalDependencies #-}

-- Exercise 10.1-i
-- Defunctionalize listToMaybe :: [a] -> Maybe a.

class Eval l t | l -> t where
    eval :: l -> t

data ListToMaybe a = ListToMaybe [a]

instance Eval (ListToMaybe a) (Maybe a) where
    eval (ListToMaybe []     ) = Nothing
    eval (ListToMaybe (a : _)) = Just a
