{-# LANGUAGE DeriveAnyClass             #-}
{-# LANGUAGE DeriveFoldable             #-}
{-# LANGUAGE DeriveFunctor              #-}
{-# LANGUAGE GeneralizedNewtypeDeriving #-}
{-# LANGUAGE MultiParamTypeClasses      #-}
{-# LANGUAGE StandaloneDeriving         #-}

module MaybeT
  ( MaybeT(MaybeT)
  , runMaybeT
  , maybeT
  , mapMaybeT
  , test
  ) where

import           Control.Applicative       (Alternative (..), Applicative (..))
import           Control.Monad             (Monad (..), MonadPlus (..), ap,
                                            liftM, mplus, mzero)
import           Control.Monad.Trans.Class (MonadTrans (..), lift)

newtype MaybeT m a = MaybeT
  { runMaybeT :: m (Maybe a)
  }

instance MonadTrans MaybeT where
  lift m = MaybeT (liftM Just m)

instance (Functor m) =>
         Functor (MaybeT m) where
  fmap f = MaybeT . fmap (fmap f) . runMaybeT

instance Applicative m =>
         Applicative (MaybeT m) where
  pure = MaybeT . pure . Just
  f <*> x = MaybeT $ (<*>) <$> runMaybeT f <*> runMaybeT x

instance Monad m =>
         Alternative (MaybeT m) where
  empty = MaybeT $ return Nothing
  x <|> y =
    MaybeT $ do
      maybe_value <- runMaybeT x
      case maybe_value of
        Nothing -> runMaybeT y
        Just _  -> return maybe_value

instance Monad m =>
         Monad (MaybeT m) where
  return x = MaybeT (return (Just x))
  m >>= f =
    MaybeT
      (do ma <- runMaybeT m
          case ma of
            Nothing -> return Nothing
            Just a  -> runMaybeT (f a))

instance Monad m =>
         MonadPlus (MaybeT m) where
  mzero = MaybeT (return Nothing)
  m `mplus` m' =
    MaybeT
      (do ma <- runMaybeT m
          case ma of
            Nothing -> runMaybeT m'
            Just a  -> return (Just a))

maybeT
  :: Monad m
  => Maybe a -> MaybeT m a
maybeT = MaybeT . return

mapMaybeT
  :: (Monad m, Monad n)
  => (m (Maybe a) -> n (Maybe b)) -> MaybeT m a -> MaybeT n b
mapMaybeT f = MaybeT . f . runMaybeT

--------------------------------------------------------------
m1 :: MaybeT [] Integer
m1 = MaybeT [Just 1, Just 2, Just 30]

f1 :: Integer -> MaybeT [] Integer
f1 n =
  MaybeT
    [ Just n
    , if n < 10
        then Just (n * 50)
        else Nothing
    ]

test = runMaybeT (m1 >>= f1)
