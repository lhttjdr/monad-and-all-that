module Main where

import Tree
import Tree.Naive as N
import Tree.BeforeMonad as B
import Tree.Monad as M
import Tree.AfterMonad as A

import System.Random (mkStdGen)
import Random.Monad as RM

import IO

tree1 :: Tree Char
tree1 = Branch (Leaf 'A') (Branch (Branch (Leaf 'B') (Leaf 'C')) (Leaf 'D'))

tree2 :: Tree Integer
tree2 = Branch (Leaf 300) (Branch (Branch (Leaf 20) (Leaf 1)) (Leaf (-4)))

tree3 :: Tree Char
tree3 = Branch (Branch (Leaf 'X') (Leaf 'T')) (Branch (Branch (Leaf 'Y') (Leaf 'Z')) (Leaf 'W'))

main :: IO ()
main = do
    putStrLn "Monad and all that..."
    putStrLn $ "tree1: " ++ show tree1
    putStrLn $ "tree2: " ++ show tree2
    putStrLn $ "tree3: " ++ show tree3
    putStrLn ">>> Test 1: number a tree"
    putStrLn $ "(number tree1, counter): " ++ show (M.unState (M.number tree1) 1)
    putStrLn ">>> Test 2: zip two trees"
    putStrLn ">>>> Test 2.1 same shape"
    putStrLn $ "zipTree tree1 tree2: " ++ show (M.zipTree tree1 tree2)
    putStrLn ">>>> Test 2.2 different shape"
    putStrLn $ "zipTree tree1 tree3: " ++ show (M.zipTree tree1 tree3)
    putStrLn ">>> Test 3: RandomList"
    print $ RM.unRandom (randomList (randomInt 869)) (mkStdGen 656868565)
    putStrLn ">>> Test 4: IO - change the world"
    changeWorld