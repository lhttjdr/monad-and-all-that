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
{-
-- basic version
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
number (Leaf a) = tick

-- tick :: Num a => State a (Tree a)
-- tick = MkState (\s -> (Leaf s, s + 1))
-- NOTICE:
--   Now, >>= does not work with mixing `State a a` and `State a (Tree a)` up.
--   In previous version, `State s` is not a type, but a type synonym of `s -> (a, s)`.
-}

--  here is a more readable version

-- ie:  get 1 -> (1, 1)
get :: State s s
get = MkState(\s -> (s, s))

-- ie: (put 5) 1 -> ((), 5)
put :: s -> State s ()
put x = MkState (\s-> ((), x))

tick = get >>= (\n -> put (n + 1) >>= (\_ -> return n))

{-
-- do-notation is syntax sugar for standard Monad class
tick :: State Int Int
tick = do
    n <- get
    put (n + 1)
    return n
-}

liftM2 :: Monad m => (a -> b -> c) -> m a -> m b -> m c
liftM2 f ma mb =
    ma >>= \a ->
    mb >>= \b ->
    return (f a b)

liftM :: Monad m => (a -> b) -> m a -> m b
liftM f ma =
    ma >>= \a -> return (f a)

sequence :: Monad m => [m a] -> m [a]
sequence = foldr (liftM2 (:)) (return [])

zipTree :: Tree a -> Tree b -> Maybe (Tree (a,b))
zipTree (Leaf a) (Leaf b) =
    return (Leaf (a, b))
zipTree (Branch l r) (Branch l' r') =
    liftM2 Branch (zipTree l l') (zipTree r r')
zipTree _ _ = Nothing

number :: Num a => Tree b -> State a (Tree a)
number (Branch l r) =
    liftM2 Branch (number l) (number r)
number (Leaf a) =
    liftM Leaf tick

tick :: Num a => State a a