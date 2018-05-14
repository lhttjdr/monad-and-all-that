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

{-
-- basic version
number (Branch l r) =
    number l >>= \l' ->
    number r >>= \r' ->
    return (Branch l' r')

number (Leaf a) = tick >>= (\s ->
    return (Leaf s))

tick s = (s, s + 1)
-}

-- ie:  get 1 -> (1, 1)
get :: State s s
get = \s -> (s, s)

-- ie: (put 5) 1 -> ((), 5)
put :: s -> State s ()
put x = \s-> ((), x)

tick :: Num a => State a a
tick = get >>= (\n -> put (n + 1) >>= (\_ -> return n))

liftM2 f ma mb =
    ma >>= \a ->
    mb >>= \b ->
    return (f a b)

liftM f ma =
    ma >>= \a -> return (f a)

number :: Num a => Tree b -> State a (Tree a)
number (Branch l r) =
    liftM2 Branch (number l) (number r)
number (Leaf a) =
    liftM Leaf tick

-- Remark:
-- The type of `tick` is `State a a`, however the type of `number (Leaf a)` is `State a (Tree a)`.
-- Let notation `m` be a monad to replace `State a`, it becomes `m a -> m (Tree a)`.
-- Here we know `Leaf :: a- > Tree a`.
-- Thus, `liftM :: (a -> b) -> m a -> m b` is a `fmap` on monad.