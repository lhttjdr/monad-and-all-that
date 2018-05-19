{-# LANGUAGE DeriveAnyClass             #-}
{-# LANGUAGE DeriveFoldable             #-}
{-# LANGUAGE DeriveFunctor              #-}
{-# LANGUAGE GeneralizedNewtypeDeriving #-}
{-# LANGUAGE MultiParamTypeClasses      #-}
{-# LANGUAGE StandaloneDeriving         #-}

module BackT
  ( BackT(BackT)
  , unBackT
  , runBackT
  , mplus
  , mzero
  ) where

import           Control.Applicative       (Alternative (..), Applicative (..))
import           Control.Monad             (Monad, MonadPlus, ap, liftM, liftM2,
                                            mplus, mzero, (>=>))
import           Control.Monad.IO.Class    (MonadIO, liftIO)
import           Control.Monad.Trans.Class (MonadTrans, lift)

import           MaybeT                    (MaybeT (..), mapMaybeT, runMaybeT)

newtype BackT m a = BackT
  { unBackT :: MaybeT m (a, BackT m a)
  }

instance MonadTrans BackT where
  lift m =
    BackT
      (do a <- lift m
          return (a, mzero))

instance Monad m =>
         Functor (BackT m) where
  fmap = liftM

instance Monad m =>
         Applicative (BackT m) where
  pure = return
  (<*>) = ap

instance Monad m =>
         Alternative (BackT m) where
  (<|>) = mplus
  empty = mzero

instance Monad m =>
         Monad (BackT m) where
  return a = lift (return a)
  x >>= f =
    BackT
      (do (a, back) <- unBackT x
          unBackT (f a `mplus` (back >>= f)))

instance Monad m =>
         MonadPlus (BackT m) where
  mzero = BackT mzero
  x `mplus` y =
    BackT
      (do (a, back) <- unBackT x
          return (a, back `mplus` y)
     `mplus` unBackT y)

instance MonadIO m =>
         MonadIO (BackT m) where
  liftIO io = lift (liftIO io)

-- (r -> a -> mr) combine
-- r  initial value
fold
  :: Monad m
  => (r -> a -> m r) -> r -> BackT m a -> m r
fold f res s = do
  ma <- runMaybeT . unBackT $ s
  case ma of
    Nothing -> return res
    Just (x, xs) -> do
      res' <- f res x
      fold f res' xs

connect
  :: Monad m
  => ([a] -> [a]) -> a -> m ([a] -> [a])
connect res x = return $ res . (x :)

runBackT
  :: (Monad m)
  => BackT m a -> m [a]
runBackT s = liftM ($ []) (fold connect id s)
