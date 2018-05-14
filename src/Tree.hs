module Tree (
    Tree(Leaf, Branch), Maybe(Nothing, Just)
    )where

-- hide standard definition
import Prelude hiding(Functor, fmap, Maybe, Nothing, Just, Monad, return, (>>=), liftM, liftM2, sequence)
--------------------------------------------------
-- Preliminaries
--------------------------------------------------
-- a special full binary tree
data Tree a = Leaf a | Branch (Tree a) (Tree a)
    deriving (Eq, Show)

-- a map function on Tree
treeMap:: (a -> b) -> Tree a -> Tree b
treeMap f (Leaf a) = Leaf (f a)
treeMap f (Branch l r) = Branch (treeMap f l) (treeMap f r)

-- e.g. Haskell type class mechanism
class Functor f where
    fmap :: ( a -> b ) -> f a -> f b

instance Functor Tree where
    fmap f (Leaf a) = Leaf (f a)
    fmap f (Branch l r) = Branch (fmap f l) (fmap f r)

-- Maybe makes Haskell can throw exception (Nothing)
data Maybe a = Nothing | Just a

instance Show a => Show (Maybe a) where
    show Nothing   = "Nothing"
    show (Just a)  = "Just("++show a++")"