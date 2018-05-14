{-# LANGUAGE MultiParamTypeClasses #-} -- for define State monad

module Tree.Monad (
    number, zipTree, unState
    )where

-- hide standard definition
import Prelude hiding(Functor, fmap, Maybe, Nothing, Just, Monad, return, (>>=), liftM, liftM2, sequence)
import Tree (Tree(..), Maybe(..))

class Monad m where
    return :: a -> m a
    (>>=)  :: m a -> (a -> m b) -> m b

newtype State s a = MkState {unState :: s -> (a, s)}
instance Monad (State s) where
    return x = MkState (\s -> (x, s))
    f >>= g = MkState (\s -> let (a, s') = unState f s in
                           unState (g a) s')

instance Monad Maybe where
    return x = Just x
    x >>= f = case x of
        Nothing -> Nothing
        Just x' -> f x'

zipTree :: Tree a -> Tree b -> Maybe (Tree (a, b))
zipTree (Leaf a) (Leaf b) =
    return (Leaf (a,b))
zipTree (Branch l r) (Branch l' r') =
    zipTree l l' >>= \l'' ->
    zipTree r r' >>= \r'' ->
    return (Branch l'' r'')
zipTree _ _ = Nothing

number :: (Num a) => Tree b -> State a (Tree a)
number (Branch l r) =
    number l >>= \l' ->
    number r >>= \r' ->
    return (Branch l' r')
number (Leaf a) = tick >>= (\a->
    return (Leaf a))

tick :: Num a => State a a
tick = MkState (\s -> (s, s + 1))