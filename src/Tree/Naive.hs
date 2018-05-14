module Tree.Naive (
    number, zipTree
    )where

-- hide standard definition
import Prelude hiding(Functor, fmap, Maybe, Nothing, Just, Monad, return, (>>=), liftM, liftM2, sequence)
import Tree (Tree(..), Maybe(..))

----------------------------------------------------------
-- Problem & Pseudocode
----------------------------------------------------------
{-
-- Problem I: number the leaves on a Tree in pre-order
--    * Lazy evaluation ==> pre-order traversal
--    * tick() is a counter, which will return a value and increase itself by 1
--      Haskell is pure, so it's impossible to implement such tick()
number:: Tree a -> Tree b
number (Leaf a) = Leaf (tick())
number (Branch l r) = Branch (number l) (number r)

-- Problem II : zip two trees.
-- Only zip two trees with same shape.
-- It will fails when Tree a has different shape from Tree b.

zipTree:: Tree a -> Tree b -> Tree (a,b)
zipTree (Leaf a) (Leaf b) = Leaf (a,b)
zipTree (Branch l r) (Branch l' r') =
    Branch (zipTree l l') (zipTree r r')
zipTree _ _ = throw TreesOfDifferentShape

... catch (zipTree t1 t2) ...

-- Haskell is pure, it's impossible to throw an exception.
-}

------------------------------------------------------------------
-- Solution : Naive version
------------------------------------------------------------------

-- variable s with type b is a counter, which is passed through in backtracking
number:: Num b => Tree a -> b -> (Tree b, b)
number (Leaf a) s = (Leaf s, s + 1 )
number (Branch l r) s =
    let (l', s') = number l s
        (r', s'') = number r s'
    in (Branch l' r', s'')

zipTree :: Tree a -> Tree b -> Maybe (Tree (a,b))
zipTree (Leaf a) (Leaf b) =
    Just (Leaf (a, b))
zipTree (Branch l r) (Branch l' r') =
    case zipTree l l' of
        Nothing -> Nothing
        Just l'' -> case zipTree r r' of
            Nothing -> Nothing
            Just r'' ->
                Just (Branch l'' r'')
zipTree _ _ = Nothing