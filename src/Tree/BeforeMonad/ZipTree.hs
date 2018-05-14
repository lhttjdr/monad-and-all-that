module Tree.BeforeMonad.ZipTree (
    zipTree
    )where

-- hide standard definition
import Prelude hiding(Functor, fmap, Maybe, Nothing, Just, Monad, return, (>>=), liftM, liftM2, sequence)
import Tree (Tree(..), Maybe(..))

return :: a-> Maybe a
return x = Just x

(>>=):: Maybe a -> (a-> Maybe b) -> Maybe b
x >>= f = case x of
    Nothing -> Nothing
    Just x' -> f x'

{-
-- basic version

zipTree :: Tree a -> Tree b -> Maybe (Tree (a,b))
zipTree (Leaf a) (Leaf b) =
    return (Leaf (a,b))
zipTree (Branch l r) (Branch l' r') =
    zipTree l l' >>= \l'' ->
    zipTree r r' >>= \r'' ->
    return (Branch l'' r'')
zipTree _ _ = Nothing

-}

liftM2 f ma mb =
    ma >>= \a ->
    mb >>= \b ->
    return (f a b)

liftM f ma =
    ma >>= \a -> return (f a)

zipTree :: Tree a -> Tree b -> Maybe (Tree (a,b))
zipTree (Leaf a) (Leaf b) =
    return (Leaf (a, b))
zipTree (Branch l r) (Branch l' r') =
    liftM2 Branch (zipTree l l') (zipTree r r')
zipTree _ _ = Nothing