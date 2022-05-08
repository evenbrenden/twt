#! /usr/bin/env nix-shell
#! nix-shell -i "ghcid -c 'ghci -Wall'"

{-# LANGUAGE RankNTypes #-}

newtype Cont a = Cont
    { unCont :: forall r. (a -> r) -> r
    }

-- Exercise 6.4-i
-- Provide a Functor instance for Cont. Hint: use lots of type holes, and an explicit lambda whenever looking for a function type. The implementation is sufficiently difficult that trying to write it point-free will be particularly mind-bending.

instance Functor Cont where
    fmap f c = Cont $ \ar -> unCont c (ar . f)

-- Exercise 6.4-ii
-- Provide the Applicative instances for Cont.

instance Applicative Cont where
    pure a = Cont $ \ar -> ar a
    cf <*> ca =
        let abrr = unCont cf
            arr  = unCont ca
        in  Cont $ \br -> abrr $ \ab -> arr $ br . ab

-- Exercise 6.4-iii
-- Provide the Monad instances for Cont.

instance Monad Cont where
    return = pure
    ma >>= amb =
        let arr = unCont ma in Cont $ \br -> arr (\a -> unCont (amb a) br)

-- Exercise 6.4-iv
-- There is also a monad transformer version of Cont. Implement it.

newtype ContT m a = ContT
    { unContT :: forall r. (a -> m r) -> m r
    }

instance Monad m => Functor (ContT m) where
    fmap f c = ContT $ \ar -> unContT c (ar . f)

instance Monad m => Applicative (ContT m) where
    pure a = ContT $ \ar -> ar a
    cf <*> ca =
        let abrr = unContT cf
            arr  = unContT ca
        in  ContT $ \br -> abrr $ \ab -> arr $ br . ab

instance Monad m => Monad (ContT m) where
    return = pure
    ma >>= amb =
        let arr = unContT ma in ContT $ \br -> arr (\a -> unContT (amb a) br)
