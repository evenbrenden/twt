#! /usr/bin/env nix-shell
#! nix-shell -p ghc ghcid -i "ghcid -c 'ghci -Wall'"

{-# LANGUAGE RoleAnnotations #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE TypeApplications #-}
{-# LANGUAGE TypeFamilies #-}

import           Data.Coerce                    ( coerce )
import qualified Data.Map                      as M
import           Data.Monoid                    ( Product(..)
                                                , Sum(..)
                                                )

slowSum :: [Int] -> Int
slowSum = getSum . mconcat . fmap Sum

fastSum :: [Int] -> Int
fastSum = getSum . mconcat . coerce

sum2Product :: Product Int
sum2Product = coerce (1867 :: Sum Int) :: Product Int

newtype Reverse a = Reverse
    { getReverse :: a
    } deriving (Eq, Show)

instance Ord a => Ord (Reverse a) where
    compare (Reverse a) (Reverse b) = compare b a

x :: M.Map Char (Reverse Bool)
x = coerce (M.singleton 'S' True) :: M.Map Char (Reverse Bool)

y :: M.Map Char (Reverse Bool)
y = M.fromList [('S', Reverse { getReverse = True })]

-- Won't (and shouldn't) work
-- h :: M.Map (Reverse Char) Bool
-- h = coerce (M.singleton 'S' True) :: M.Map (Reverse Char) Bool

data BST v
    = Empty
    | Branch (BST v) v (BST v)

type role BST nominal
