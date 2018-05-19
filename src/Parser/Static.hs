{-# LANGUAGE AutoDeriveTypeable #-}
{-# LANGUAGE DeriveFunctor #-}
{-# LANGUAGE GeneralizedNewtypeDeriving #-}
{-# LANGUAGE MultiParamTypeClasses #-}

module Parser.Static
    ( Parser
    , Static
    , runEmpty
    , runStarts
    , runStatic
    , expr
    , exactly
    ) where

import Data.List (nub)

import Control.Applicative (Applicative(..),Alternative(..))
import Control.Monad
import Control.Monad.State

-- | Two important concept: Product & Compose
-- Product
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

-- Compose
newtype Compose f g a =
    Comp (f (g a))
    deriving (Functor)

instance (Applicative f, Applicative g) =>
         Applicative (Compose f g) where
    pure x = Comp (pure (pure x))
    Comp f <*> Comp x = Comp (pure (<*>) <*> f <*> x)

-- | Parser class
class Alternative p =>
      Parser p where
    exactly :: Char -> p Char
    anyof :: [Char] -> p Char
    anyof xs = foldl1 (<|>) (map exactly xs)

-- | Can a parser match the empty string?
newtype Empty a =
    Empty Bool
    deriving (Functor)

instance Applicative Empty where
    -- pattern only match empty string
    pure _ = Empty True
    -- connect f x can match empty iff both of them can accept empty
    Empty f <*> Empty x = Empty (f && x)

instance Alternative Empty where
    -- pattern match nothing
    empty = Empty False
    -- union f g can match empty if any one of them accept empty
    Empty f <|> Empty g = Empty (f || g)

instance Parser Empty where
    -- pattern exactly match something
    exactly _ = Empty False

runEmpty :: Empty a -> Bool
runEmpty (Empty a) = a

-- | What tokens can a parse start with?
newtype Starts a =
    Starts [Char]
    deriving (Show)

-- NOTICE: `Fuctor` instance is required by ghc compiler
-- if declare `Starts` as `Applicative` and `Alternative`.
-- However it is obviously call a non-implemented method `(<*>)`.
-- It not necessary for programmer because we guarantee
-- that `Starts` will never use `(<*>)` or `fmap`.
-- But compiler does not know that. It general case, they are necessary.
instance Functor Starts where
    fmap f x = pure f <*> x

instance Applicative Starts where
    -- not accept any pattern
    pure x = Starts []
    -- connect pattern ts and ts'
    -- If ts can accept empty, it may start with `ts'`. Thus, ts ++ ts' is accepted.
    -- Otherwise, only `ts` is accepted.
    -- But whether `ts` can accept empty depends on `Empty ts` instead of `Starts ts`.
    -- Starts ts <*> Starts ts' = ???

instance Alternative Starts where
    -- no pattern
    empty = Starts []
    -- union of two patterns
    Starts ts <|> Starts ts' = Starts (nub (ts ++ ts'))

instance Parser Starts where
    -- the pattern only match t
    exactly t = Starts [t]

-- NOTICE :: `Starts` should never used by any caller except the `Static` below.
-- Here is just for demonstration.
-- We don't implement a complete `Applicative` instance.
-- So, it will throw an uncaught exception when `(*)` is needed.
runStarts :: Starts a -> [Char]
runStarts (Starts xs) = xs

instance (Parser f, Parser g) =>
         Parser (Prod f g) where
    exactly t = Prod (exactly t) (exactly t)

-- | Combine two Parser
newtype Static a =
    Static (Prod Starts Empty a)
    deriving (Parser)

instance Functor Static where
    fmap f x = pure f <*> x

instance Applicative Static where
    pure x = Static (pure x)
    Static (Prod (Starts ts) (Empty e)) <*> ~(Static (Prod (Starts ts') (Empty e'))) =
        Static
            (Prod
                 (Starts
                      (ts ++
                       if e
                           then ts'
                           else []))
                 (Empty e <*> Empty e'))

instance Alternative Static where
    empty = Static (Prod empty empty)
    Static (Prod (Starts ts) (Empty e)) <|> (Static (Prod (Starts ts') (Empty e'))) =
        Static (Prod ((Starts ts) <|> (Starts ts')) ((Empty e) <|> (Empty e')))
    -- There are standard implements of `some` and `many` in applicative library.
    -- Here is just for demonstration.
    some f = s
       where
         s = (:) <$> f <*> (s <|> pure [])
    many f = some f <|> pure []

runStatic :: Static a -> ([Char], Bool)
runStatic (Static (Prod (Starts ts) (Empty e))) = (ts, e)

-- | some parsers
--   Here `p` can be `Starts` `Empty` or `Static`.
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