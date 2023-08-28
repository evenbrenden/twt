#! /usr/bin/env nix-shell
#! nix-shell --pure -i "ghcid -c 'ghci -Wall'"

{-# LANGUAGE GADTs #-}
{-# LANGUAGE RankNTypes #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE TypeApplications #-}

import           Data.Foldable                  ( asum )
import           Data.Maybe                     ( fromMaybe )
import           Data.Typeable

data Dynamic where
    Dynamic ::Typeable t => t -> Dynamic

elimDynamic :: (forall a . Typeable a => a -> r) -> Dynamic -> r
elimDynamic f (Dynamic a) = f a

fromDynamic :: Typeable a => Dynamic -> Maybe a
fromDynamic = elimDynamic cast

liftD2
    :: forall a b r
     . (Typeable a, Typeable b, Typeable r)
    => Dynamic
    -> Dynamic
    -> (a -> b -> r)
    -> Maybe Dynamic
liftD2 d1 d2 f = fmap Dynamic . f <$> fromDynamic @a d1 <*> fromDynamic @b d2

pyPlus :: Dynamic -> Dynamic -> Dynamic
pyPlus a b = fromMaybe (error "bad types for pyPlus") $ asum
    [ liftD2 @String @String a b (++)
    , liftD2 @Int @Int a b (+)
    , liftD2 @String @Int a b $ \strA intB -> strA ++ show intB
    , liftD2 @Int @String a b $ \intA strB -> show intA ++ strB
    ]

test1 :: Maybe Int
test1 = fromDynamic @Int (pyPlus (Dynamic @Int 1) (Dynamic @Int 2))

test2 :: Maybe String
test2 = fromDynamic @String (pyPlus (Dynamic "hello") (Dynamic " world"))

test3 :: Maybe String
test3 = fromDynamic @String (pyPlus (Dynamic @Int 4) (Dynamic " minute"))
