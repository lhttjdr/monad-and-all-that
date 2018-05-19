{-# LANGUAGE DeriveFunctor              #-}
{-# LANGUAGE GeneralizedNewtypeDeriving #-}
{-# LANGUAGE MultiParamTypeClasses      #-}

module Parser.BeforeApplicative
    ( Parser
    , runParser
    , expr
    ) where

import           Data.Char

import           Prelude hiding ((<*>), (*>), (<*))

import           Control.Applicative hiding (many, some, (<*>), (*>), (<*))
import           Control.Monad
import           Control.Monad.State

type Token = Char

-- [t] is state
newtype Parser t a =
    Parser (StateT [t] [] a)
    deriving ( Functor
             , Applicative
             , Monad
             , MonadState [t]
             , Alternative
             , MonadPlus
             )

runParser :: Parser t a -> [t] -> [] (a, [t])
runParser (Parser m) ts = runStateT m ts

token :: Parser Token Token
token = do
    toks <- get
    case toks of
        [] -> mzero
        (t:toks') -> do
            put toks'
            return t

satisfy p = do
    t <- token
    guard (p t)
    return t

many
    :: MonadPlus m
    => m a -> m [a]
many p = some p `mplus` return []

some
    :: MonadPlus m
    => m a -> m [a]
some p = liftM2 (:) p (many p)

number :: Parser Token Integer
number = do
    ds <- some (satisfy isDigit)
    return (read ds)

exactly t = satisfy (== t)


(<*>) :: Monad m => m (a -> b) -> m a -> m b
f <*> x = liftM2 ($) f x

(<*) :: Monad m => m a -> m b -> m a
a <* b = return const <*> a <*> b

(*>) :: Monad m => m a -> m b -> m b
a *> b = return (const id) <*> a <*> b


expr :: Parser Token Integer
expr =
    return (+) <*> term <* exactly '+' <*> expr
    `mplus` term
term:: Parser Token Integer
term =
    return (*) <*> factor <* exactly '*' <*> term
    `mplus` factor

factor :: Parser Token Integer
factor =
    number
    `mplus` (exactly '(' *> expr <* exactly ')')