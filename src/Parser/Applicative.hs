{-# OPTIONS_GHC -Wall #-}
{-# LANGUAGE DeriveFunctor #-}
{-# LANGUAGE GeneralizedNewtypeDeriving #-}
{-# LANGUAGE MultiParamTypeClasses #-}

module Parser.Applicative
    ( Parser
    , runMonadic
    , expr
    ) where

import Data.List (nub)

import Control.Applicative hiding (WrappedMonad, many, some)
import Control.Monad
import Control.Monad.State

{- Monad -> Applicative -}
newtype WrappedMonad m a = Wrap
    { unWrap :: m a
    } deriving (Functor)

instance Monad m =>
         Applicative (WrappedMonad m) where
    pure a = Wrap (return a)
    Wrap f <*> Wrap x = Wrap (liftM2 ($) f x)

instance MonadPlus m =>
         Alternative (WrappedMonad m) where
    empty = Wrap mzero
    Wrap a <|> Wrap b = Wrap (a `mplus` b)

some
    :: Alternative f
    => f a -> f [a]
-- some f = pure (:) <*> f <*> many f
some f = s
  where
    s = (:) <$> f <*> (s <|> pure [])

many
    :: Alternative f
    => f a -> f [a]
many f = some f <|> pure []

optional :: Alternative f => f a -> f (Maybe a)
-- optional f = pure Just <*> f <|> pure Nothing
optional f = Just <$> f <|> pure Nothing

{- Product -}
data Prod f g a =
    Prod (f a)
         (g a)
    deriving (Functor)

instance (Applicative f, Applicative g) =>
         Applicative (Prod f g) where
    pure x = Prod (pure x) (pure x)
    Prod f g <*> ~(Prod x y) = Prod (f <*> x) (g <*> y)

instance (Alternative f, Alternative g) =>
         Alternative (Prod f g) where
    empty = Prod empty empty
    Prod x1 y1 <|> Prod x2 y2 = Prod (x1 <|> x2) (y1 <|> y2)


{- Compose -}
newtype Compose f g a =
    Comp (f (g a))
    deriving (Functor)

instance (Applicative f, Applicative g) =>
         Applicative (Compose f g) where
    pure x = Comp (pure (pure x))
    Comp f <*> Comp x = Comp (pure (<*>) <*> f <*> x)

newtype Monadic a =
    Monadic (WrappedMonad (StateT String Maybe) a)
    deriving (Functor, Applicative, Alternative)

class Alternative p =>
      Parser p where
    exactly :: Char -> p Char

instance Parser Monadic where
    exactly t =
        Monadic
            (Wrap
                 (do ts <- get
                     case ts of
                         [] -> mzero
                         t':ts' -> do
                             guard (t == t')
                             put ts'
                             return t))

anyof :: Parser f => [Char] -> f Char
anyof xs = foldl1 (<|>) (map exactly xs)

number
    :: Parser p
    => p Integer
number = read <$> some (anyof ['0' .. '9'])

expr
    :: Parser p
    => p Integer
expr = (+) <$> term <* exactly '+' <*> expr <|> term

term
    :: Parser p
    => p Integer
term = (*) <$> factor <* exactly '*' <*> term <|> factor

factor
    :: Parser p
    => p Integer
factor = number <|> exactly '(' *> expr <* exactly ')'

runMonadic :: Monadic a -> String -> Maybe (a, String)
runMonadic (Monadic m) ts = runStateT (unWrap m) ts