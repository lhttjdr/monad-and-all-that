{-# LANGUAGE DeriveFunctor              #-}
{-# LANGUAGE GeneralizedNewtypeDeriving #-}
{-# LANGUAGE MultiParamTypeClasses      #-}

module Parser.Naive
    ( Parser
    , runParser
    , expr
    ) where

import           Data.Char

import           Control.Applicative hiding (many, some)
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

satisfy :: (Token -> Bool) -> Parser Token Token
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

exactly :: Token -> Parser Token Token
exactly t = satisfy (== t)

expr :: Parser Token Integer
expr =
    do a <- term
       exactly '+'
       b <- expr
       return (a + b)
       `mplus` term

term :: Parser Token Integer
term =
    do a <- factor
       exactly '*'
       b <- term
       return (a * b)
       `mplus` factor

factor :: Parser Token Integer
factor =
    number `mplus` do
        exactly '('
        a <- expr
        exactly ')'
        return a
