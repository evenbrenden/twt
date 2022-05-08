#! /usr/bin/env nix-shell
#! nix-shell -i "ghcid -c 'ghci -Wall'"

{-# LANGUAGE RankNTypes #-}

import           Data.IORef
import           System.IO.Unsafe               ( unsafePerformIO )

newtype ST s a = ST
    { unsafeRunST :: a
    }

instance Functor (ST s) where
    fmap f (ST a) = seq a . ST $ f a

instance Applicative (ST s) where
    pure = ST
    ST f <*> ST a = seq f . seq a ST $ f a

instance Monad (ST s) where
    ST a >>= f = seq a $ f a

newtype STRef s a = STRef
    { unSTRef :: IORef a
    }

newSTRef :: a -> ST s (STRef s a)
newSTRef = pure . STRef . unsafePerformIO . newIORef

readSTRef :: STRef s a -> ST s a
readSTRef = pure . unsafePerformIO . readIORef . unSTRef

writeSTRef :: STRef s a -> a -> ST s ()
writeSTRef ref = pure . unsafePerformIO . writeIORef (unSTRef ref)

modifySTRef :: STRef s a -> (a -> a) -> ST s ()
modifySTRef ref f = do
    a <- readSTRef ref
    writeSTRef ref $ f a

runST :: (forall s . ST s a) -> a
runST = unsafeRunST

safeExample :: ST s String
safeExample = do
    ref <- newSTRef "hello"
    modifySTRef ref (++ " world")
    readSTRef ref

runExample :: String
runExample = runST safeExample

runSTNoTrick :: ST s a -> a
runSTNoTrick = unsafeRunST

runExampleNoTrick :: STRef s Bool
runExampleNoTrick = runSTNoTrick $ newSTRef True
