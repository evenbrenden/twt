#! /usr/bin/env nix-shell
#! nix-shell -i "ghcid -c 'ghci -Wall -Wno-missing-methods -Wno-orphans'"

{-# LANGUAGE DataKinds #-}
{-# LANGUAGE TypeOperators #-}
{-# LANGUAGE UndecidableInstances #-}

import           GHC.TypeLits            hiding ( type (+) )

instance
    ( TypeError
        ( 'Text "Attempting to interpret a number as a function"
     ':$$: 'Text "in the type '"
     ':<>: 'ShowType (a -> b)
     ':<>: 'Text "'"
     ':$$: 'Text "Did you forget to specify the function you wanted?"
        )
    ) => Num (a -> b) where
