#! /usr/bin/env nix-shell
#! nix-shell chapter14.nix -i "ghcid -c 'ghci -Wall'"

{-# LANGUAGE DataKinds #-}
{-# LANGUAGE GeneralizedNewtypeDeriving #-}
{-# LANGUAGE InstanceSigs #-}
{-# LANGUAGE PolyKinds #-}
{-# LANGUAGE RankNTypes #-}
{-# LANGUAGE RebindableSyntax #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE TypeApplications #-}
{-# LANGUAGE TypeFamilies #-}
{-# LANGUAGE TypeOperators #-}

import           Control.Monad.Indexed
import           Data.Coerce
import           Fcf
import           GHC.TypeLits                   ( Nat )
import qualified GHC.TypeLits                  as TL
import           Language.Haskell.DoNotation   as LHDN
import           Prelude                 hiding ( Monad(..)
                                                , getContents
                                                , putStr
                                                )
import qualified System.IO                     as SIO
import           System.IO               hiding ( Handle
                                                , getContents
                                                , openFile
                                                , putStr
                                                )
import qualified System.IO.Strict              as SIOS

newtype Ix m i j a = Ix
    { unsafeRunIx :: m a
    }
    deriving (Functor, Applicative, Monad)

instance Functor m => IxFunctor (Ix m) where
    imap = fmap

instance Applicative m => IxPointed (Ix m) where
    ireturn = Prelude.pure

instance Applicative m => IxApplicative (Ix m) where
    iap :: forall i j k a b . Ix m i j (a -> b) -> Ix m j k a -> Ix m i k b
    iap = coerce $ (<*>) @m @a @b

instance Monad m => IxMonad (Ix m) where
    ibind :: forall i j k a b . (a -> Ix m j k b) -> Ix m i j a -> Ix m i k b
    ibind = coerce $ (=<<) @m @a @b

data LinearState = LinearState
    { linearNextKey  :: Nat
    , linearOpenKeys :: [Nat]
    }

newtype Linear s (i :: LinearState) (j :: LinearState) a = Linear
    { unsafeRunLinear :: Ix IO i j a
    }
    deriving (IxFunctor, IxPointed, IxApplicative, IxMonad)

newtype Handle s key = Handle
    { unsafeGethandle :: SIO.Handle
    }

openFile
    :: FilePath
    -> IOMode
    -> Linear
           s
           ( 'LinearState next open)
           ( 'LinearState (next TL.+ 1) (next ': open))
           (Handle s next)
openFile = coerce SIO.openFile

type IsOpen (key :: k) (ts :: [k]) = IsJust =<< Find (TyEq key) ts

type Close (key :: k) (ts :: [k]) = Filter (Not <=< TyEq key) ts

closeFile
    :: Eval (IsOpen key open) ~ 'True
    => Handle s key
    -> Linear
           s
           ( 'LinearState next open)
           ( 'LinearState next (Eval (Close key open)))
           ()
closeFile = coerce SIO.hClose

runLinear
    :: (forall s . Linear s ( 'LinearState 0 '[]) ( 'LinearState n '[]) a)
    -> IO a
runLinear = coerce

readme
    :: forall k (s :: k) (next :: Nat) (open :: [Nat])
     . Linear
        s
        ( 'LinearState next open)
        ( 'LinearState (next TL.+ 1) (next : open))
        (Handle s next)
readme = openFile "README.md" ReadMode

openClose :: IO ()
openClose = runLinear $ readme >>= closeFile

-- > runLinear readme
-- ...
--       Expected type: Linear s ('LinearState 0 '[]) ('LinearState 1 '[]) a
--         Actual type: Linear
--                        s ('LinearState 0 '[]) ('LinearState (0 TL.+ 1) '[0]) (Handle s 0)
-- ...

-- > runLinear $ readme >>= \f -> closeFile f >> closeFile f
-- ...
--     • Couldn't match type ‘'False’ with ‘'True’
--         arising from a use of ‘closeFile’
-- ...

-- > runLinear $ readme >>= \f -> closeFile f >> LHDN.pure f
-- ...
--     • Couldn't match type ‘a’ with ‘Handle s 0’
--         because type variable ‘s’ would escape its scope
-- ...

getContents
    :: Handle s key
    -> Linear s ( 'LinearState next open) ( 'LinearState next open) String
getContents h = coerce $ SIOS.hGetContents (coerce h)

putStr
    :: String -> Linear s ( 'LinearState next open) ( 'LinearState next open) ()
putStr = coerce SIO.putStr

printReadme :: IO ()
printReadme = runLinear $ do
    f <- readme
    g <- getContents f
    putStr g
    closeFile f

getReadme :: IO String
getReadme = runLinear $ do
    f <- readme
    g <- getContents f
    closeFile f
    LHDN.pure g
