module Main where

import Tree
import Tree.Naive as N
import Tree.BeforeMonad as B
import Tree.Monad as M

tree1 :: Tree Char
tree1 = Branch (Leaf 'A') (Branch (Branch (Leaf 'B') (Leaf 'C')) (Leaf 'D'))

tree2 :: Tree Integer
tree2 = Branch (Leaf 300) (Branch (Branch (Leaf 20) (Leaf 1)) (Leaf (-4)))

tree3 :: Tree Char
tree3 = Branch (Branch (Leaf 'X') (Leaf 'T')) (Branch (Branch (Leaf 'Y') (Leaf 'Z')) (Leaf 'W'))

main :: IO ()
main = do
    print "Monad and all that..."
    print ">>> Test 1: number a tree"
    print $ "original tree: " ++ show tree1
    print $ "(numbered tree, counter): " ++ show (unState (M.number tree1) 1)
    print ">>> Test 2: zip two trees"
    print ">>>> Test 2.1 same shape"
    print $ "tree 1: " ++ show tree1
    print $ "tree 2: " ++ show tree2
    print $ "zipped tree: " ++ show (M.zipTree tree1 tree2)
    print ">>>> Test 2.2 different shape"
    print $ "tree 1: " ++ show tree1
    print $ "tree 2: " ++ show tree3
    print $ "zipped tree: " ++ show (M.zipTree tree1 tree3)