{-# LANGUAGE DeriveAnyClass #-}
{-# LANGUAGE DeriveFoldable #-}
{-# LANGUAGE DeriveFunctor #-}
{-# LANGUAGE GeneralizedNewtypeDeriving #-}
{-# LANGUAGE StandaloneDeriving #-}
{-# LANGUAGE MultiParamTypeClasses #-}

module StateT
    ( StateT(StateT), runStateT
    ) where

import           Control.Applicative (Applicative(..), Alternative(..))
import           Control.Monad (MonadPlus, Monad, liftM, ap, mzero, mplus)
import           Control.Monad.Trans.Class (MonadTrans, lift)
import           Control.Monad.Fix

newtype StateT s m a =
    StateT {runStateT :: s -> m (a,s)}

instance MonadTrans (StateT s) where
  lift m = StateT (\s -> do
                           a <- m
                           return (a,s))

instance Monad m => Monad (StateT s m) where
    return a = StateT (\s -> return (a,s))
    m >>= f = StateT (\s -> do
                     (a, s') <- runStateT m s
                     runStateT (f a) s')

instance MonadPlus m => MonadPlus (StateT s m) where
    mzero = lift mzero
    m `mplus` m' = StateT (\s ->
          runStateT m s `mplus` runStateT m' s)

instance Monad m => Applicative (StateT s m) where
    pure  = return
    (<*>) = ap

instance Monad m => Functor (StateT s m) where
    fmap = liftM

instance MonadPlus m => Alternative (StateT s m) where
    (<|>) = mplus
    empty = mzero

instance MonadFix m => MonadFix (StateT s m) where
    mfix f = StateT $ \s ->
         mfix (\ ~(x, _) -> runStateT (f x) s)