{-# LANGUAGE DeriveFunctor              #-}
{-# LANGUAGE GeneralizedNewtypeDeriving #-}
{-# LANGUAGE MultiParamTypeClasses      #-}
{-# LANGUAGE StandaloneDeriving         #-}

module Prolog
    ( test
    , runLogic
    ) where

import           Control.Applicative
import           Control.Monad
import           Control.Monad.IO.Class
import           Control.Monad.Plus
import           Data.IORef

import           BackT

data Logical a
    = Value a
    | Var (LogicVar a)

type LogicVar a = IORef (Maybe (Logical a))

newtype Logic a =
    Logic (BackT IO a)
    deriving (Functor, Applicative, Monad, MonadIO, Alternative, MonadPlus)

variable :: Logic (Logical a)
variable = liftM Var (liftIO (newIORef Nothing))

follow :: Logical a -> Logic (Logical a)
follow (Value a) = return (Value a)
follow (Var r) = do
    v <- liftIO (readIORef r)
    case v of
        Nothing  -> return (Var r)
        Just val -> follow val

class Unifiable a where
    unify :: a -> a -> Logic ()

instance Unifiable Integer where
    unify a b = guard (a == b)

instance Unifiable a =>
         Unifiable (Logical a) where
    unify a b = do
        a' <- follow a
        b' <- follow b
        case (a', b') of
            (Var ra, Var rb)
                | ra == rb -> return ()
            (Var ra, _) -> instantiate ra b'
            (_, Var rb) -> instantiate rb a'
            (Value av, Value bv) -> unify av bv

instantiate r v =
    liftIO (writeIORef r (Just v)) `mplus` do
        liftIO (writeIORef r Nothing)
        mzero

data List a
    = Nil
    | Cons a
           (Logical (List a))

instance Unifiable a =>
         Unifiable (List a) where
    unify Nil Nil = return ()
    unify (Cons x xs) (Cons y ys) = do
        unify x y
        unify xs ys
    unify _ _ = mzero

appendL xs ys zs =
    do unify xs (Value Nil)
       unify ys zs
       `mplus` do
        x <- variable
        xs' <- variable
        zs' <- variable
        unify xs (Value (Cons x xs'))
        unify zs (Value (Cons x zs'))
        appendL xs' ys zs'

------------------------------------------------------------------------------
-- test appendL
test
    :: Unifiable a
    => [a] -> Logic ([a], [a])
test zs = do
    xs <- variable
    ys <- variable
    appendL xs ys (toLogical zs)
    liftM2 (,) (fromLogical xs) (fromLogical ys)

toLogical :: [a] -> Logical (List (Logical a))
toLogical []     = Value Nil
toLogical (x:xs) = Value (Cons (Value x) (toLogical xs))

fromLogical :: Logical (List (Logical a)) -> Logic [a]
fromLogical xxs = do
    xxs' <- follow xxs
    case xxs' of
        (Value Nil) -> return []
        (Value (Cons x xs)) -> do
            (Value x') <- follow x
            xs' <- fromLogical xs
            return (x' : xs')

runLogic :: Logic a -> IO [a]
runLogic (Logic bt) = runBackT bt
