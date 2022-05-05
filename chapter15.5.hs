#! /usr/bin/env nix-shell
#! nix-shell chapter15.nix -i "ghcid -c 'ghci -Wall'"

{-# LANGUAGE ConstraintKinds #-}
{-# LANGUAGE EmptyCase #-}
{-# LANGUAGE FlexibleInstances #-}
{-# LANGUAGE GADTs #-}
{-# LANGUAGE InstanceSigs #-}
{-# LANGUAGE MultiParamTypeClasses #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE RankNTypes #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE StandaloneDeriving #-}
{-# LANGUAGE StandaloneKindSignatures #-}
{-# LANGUAGE TemplateHaskell #-}
{-# LANGUAGE TypeApplications #-}
{-# LANGUAGE TypeFamilies #-}
{-# LANGUAGE TypeInType #-}
{-# LANGUAGE UndecidableInstances #-}

import           Data.Aeson
import           Data.Constraint
import           Data.Foldable
import           Data.Kind                      ( Type )
import           Data.Maybe                     ( mapMaybe )
import           Data.Singletons.Prelude
import           Data.Singletons.TH

data Sigma (f :: k -> Type) where
    Sigma ::Sing a -> f a -> Sigma f

withSigma :: (forall (a :: k) . Sing a -> f a -> r) -> Sigma f -> r
withSigma c (Sigma s f) = c s f

toSigma :: SingI a => f a -> Sigma f
toSigma fa = Sigma sing fa

fromSigma
    :: forall k (a :: k) (f :: k -> Type)
     . (SingI a, SDecide k)
    => Sigma f
    -> Maybe (f a)
fromSigma (Sigma s f) = case s %~ sing @a of
    Proved    Refl -> Just f
    Disproved _    -> Nothing

class Dict1 c (f :: k -> Type) where
    dict1 :: Sing (a :: k) -> Dict (c (f a))

instance ( Dict1 Eq (f :: k -> Type)
         , SDecide k
         ) => Eq (Sigma f) where
    Sigma sa fa == Sigma sb fb = case sa %~ sb of
        -- IDK how calling dict1 without any instances can work
        -- The @_ is needed for some GHC > 8.6.5 (and IDK why)
        Proved Refl -> case dict1 @_ @Eq @f sa of
            Dict -> fa == fb
        Disproved _ -> False

instance ( Dict1 Show (f :: k -> Type)
         , Show (Demote k)
         , SingKind k
         ) => Show (Sigma f) where
    show (Sigma sa fa) = case dict1 @_ @Show @f sa of
        Dict -> mconcat ["Sigma ", show $ fromSing sa, " (", show fa, ")"]

-- Exercise 15.5-i
-- Provide an instance of Ord for Sigma by comparing
-- the fs if the singletons are equal, comparing the
-- singletons at the term-level otherwise.

instance ( Dict1 Eq (f :: k -> Type)
         , Dict1 Ord (f :: k -> Type)
         , Ord (Demote k)
         , SDecide k
         , SingKind k
         ) => Ord (Sigma f) where
    compare (Sigma sa fa) (Sigma sb fb) = case sa %~ sb of
        Proved Refl -> case dict1 @_ @Ord @f sa of
            Dict -> compare fa fb
        Disproved _ -> compare (fromSing sa) (fromSing sb)

-- Structured Logging
singletons [d|
    data LogType
        = JsonMsg
        | TextMsg
        deriving (Eq, Ord, Show)
    |]

data family LogMsg (msg :: LogType)

newtype instance LogMsg 'JsonMsg = Json Value
    deriving (Eq, Show)

newtype instance LogMsg 'TextMsg = Text String
    deriving (Eq, Show)

instance ( c (LogMsg 'JsonMsg)
         , c (LogMsg 'TextMsg)
         ) => Dict1 c LogMsg where
    dict1 SJsonMsg = Dict
    dict1 STextMsg = Dict

logs :: [Sigma LogMsg]
logs =
    [ toSigma $ Text "hello"
    , toSigma $ Json $ object ["world" .= (5 :: Int)]
    , toSigma $ Text "structured logging is cool"
    ]

showLogs :: [Sigma LogMsg] -> [String]
showLogs logMsgs = fmap
    (withSigma $ \sa fa -> case dict1 @_ @Show @LogMsg sa of
        Dict -> show fa
    )
    logMsgs

allOut :: IO ()
allOut = traverse_ putStrLn (showLogs logs)

catSigmas :: forall k (a :: k) f . (SingI a, SDecide k) => [Sigma f] -> [f a]
catSigmas = mapMaybe fromSigma

jsonLogs :: [LogMsg 'JsonMsg]
jsonLogs = catSigmas logs

jsonOnly :: String
jsonOnly = show jsonLogs

textLogs :: [LogMsg 'TextMsg]
textLogs = catSigmas logs

textOnly :: String
textOnly = show textLogs
