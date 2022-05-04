#! /usr/bin/env nix-shell
#! nix-shell chapter15.2.nix -i "ghcid -c 'ghci -Wall -Wno-unused-top-binds'"

{-# LANGUAGE ConstraintKinds #-}
{-# LANGUAGE DataKinds #-}
{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE GADTs #-}
{-# LANGUAGE RankNTypes #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE TypeApplications #-}
{-# LANGUAGE TypeFamilies #-}
{-# LANGUAGE TypeFamilyDependencies #-}

import           Control.Monad.Trans.Writer
import           Data.Constraint                ( Dict(..) )
import           Data.Foldable                  ( for_ )
import           Data.Kind                      ( Type )

data SBool (b :: Bool) where
    STrue ::SBool 'True
    SFalse ::SBool 'False

fromSBool :: SBool b -> Bool
fromSBool STrue  = True
fromSBool SFalse = False

data SomeSBool where
    SomeSBool ::SBool b -> SomeSBool

withSomeSBool :: SomeSBool -> (forall (b :: Bool) . SBool b -> r) -> r
withSomeSBool (SomeSBool s) f = f s

toSBool :: Bool -> SomeSBool
toSBool True  = SomeSBool STrue
toSBool False = SomeSBool SFalse

class Monad (LoggingMonad b) => MonadLogging (b :: Bool) where
    type LoggingMonad b = (r :: Type -> Type) | r -> b
    logMsg :: String -> LoggingMonad b ()
    runLogging :: LoggingMonad b a -> IO a

instance MonadLogging 'False where
    type LoggingMonad 'False = IO
    logMsg _ = pure ()
    runLogging = id

instance MonadLogging 'True where
    type LoggingMonad 'True = WriterT [String] IO
    logMsg s = tell [s]
    runLogging m = do
        (a, w) <- runWriterT m
        for_ w putStrLn
        pure a

program :: MonadLogging b => LoggingMonad b ()
program = do
    logMsg "hello world"
    pure ()

dict :: (c 'True, c 'False) => SBool b -> Dict (c b)
dict STrue  = Dict
dict SFalse = Dict

main :: IO ()
main = do
    bool <- read <$> getLine
    withSomeSBool (toSBool bool) $ \(sb :: SBool b) ->
        case dict @MonadLogging sb of
            Dict -> runLogging @b program
