{-# LANGUAGE MultiParamTypeClasses #-} -- for define State monad

module Tree.BeforeMonad.Number (
   number
   )where

-- hide standard definition
import Prelude hiding(Functor, fmap, Maybe, Nothing, Just, Monad, return, (>>=), liftM, liftM2, sequence)
import Tree (Tree(..))

type State s a = s -> (a, s)

return x = \s -> (x, s)
x >>= f = \s -> let (a, s') = x s in f a s'

number (Branch l r) =
    number l >>= \l' ->
    number r >>= \r' ->
    return (Branch l' r')

number (Leaf a) = tick >>= (\s ->
    return (Leaf s))

tick s = (s, s + 1)

-- Remark:
-- In code above, `State s a` is NOT a type, but a type synonym of `s -> (a, s)`.