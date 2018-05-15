import Test.Hspec
import Test.QuickCheck
import Control.Exception (evaluate)

import Prelude hiding (Maybe, Just, Nothing)

import Tree (Tree(..), Maybe(..))
import Tree.Naive as N
import Tree.BeforeMonad as B
import Tree.Monad as M
import Tree.AfterMonad as A

tree1 :: Tree Char
tree1 = Branch (Leaf 'A') (Branch (Branch (Leaf 'B') (Leaf 'C')) (Leaf 'D'))

tree2 :: Tree Integer
tree2 = Branch (Leaf 300) (Branch (Branch (Leaf 20) (Leaf 1)) (Leaf (-4)))

numbered_tree12 :: Integer -> (Tree Integer, Integer)
numbered_tree12 s = (Branch (Leaf s) (Branch (Branch (Leaf (s + 1)) (Leaf (s + 2))) (Leaf (s + 3))), s + 4)

zipped_tree12 :: Tree (Char, Integer)
zipped_tree12 = Branch (Leaf ('A', 300)) (Branch (Branch (Leaf ('B', 20)) (Leaf ('C', 1))) (Leaf ('D', -4)))


tree3 :: Tree String
tree3 = Branch (Branch (Leaf "red") (Leaf "yellow")) (Branch (Branch (Leaf "green") (Leaf "blue")) (Leaf "orange"))

tree4 :: Tree Double
tree4 = Branch (Branch (Leaf 3.14) (Leaf 1.414)) (Branch (Branch (Leaf 0.618) (Leaf 9.8)) (Leaf 7.9))

numbered_tree34 :: Integer -> (Tree Integer, Integer)
numbered_tree34 s = (Branch (Branch (Leaf s) (Leaf (s + 1))) (Branch (Branch (Leaf (s + 2)) (Leaf (s + 3))) (Leaf (s + 4))), s + 5)

zipped_tree34 :: Tree (String, Double)
zipped_tree34 = Branch (Branch (Leaf ("red",3.14)) (Leaf ("yellow",1.414))) (Branch (Branch (Leaf ("green",0.618)) (Leaf ("blue",9.8))) (Leaf ("orange",7.9)))

main :: IO ()
main = hspec $ do
    describe "Number a Tree" $ do
        it ("Tree 1:" ++ show tree1) $ do
            (N.number tree1 5) `shouldBe` (numbered_tree12 5)
            (B.number tree1 6) `shouldBe` (numbered_tree12 6)
            (M.unState (M.number tree1) 7) `shouldBe` (numbered_tree12 7)
            (A.unState (A.number tree1) 100) `shouldBe` (numbered_tree12 100)
        it ("Tree 2: " ++ show tree2) $ do
            (N.number tree2 5) `shouldBe` (numbered_tree12 5)
            (B.number tree2 6) `shouldBe` (numbered_tree12 6)
            (M.unState (M.number tree2) 7) `shouldBe` (numbered_tree12 7)
            (A.unState (A.number tree2) 100) `shouldBe` (numbered_tree12 100)
        it ("Tree 3: " ++ show tree3) $ do
            (N.number tree3 5) `shouldBe` (numbered_tree34 5)
            (B.number tree3 6) `shouldBe` (numbered_tree34 6)
            (M.unState (M.number tree3) 7) `shouldBe` (numbered_tree34 7)
            (A.unState (A.number tree3) 100) `shouldBe` (numbered_tree34 100)
        it ("Tree 4: " ++ show tree4) $ do
            (N.number tree4 5) `shouldBe` (numbered_tree34 5)
            (B.number tree4 6) `shouldBe` (numbered_tree34 6)
            (M.unState (M.number tree4) 7) `shouldBe` (numbered_tree34 7)
            (A.unState (A.number tree4) 100) `shouldBe` (numbered_tree34 100)
    describe "Zip Two Trees" $ do
        it ("Tree 1 & Tree 2:" ++ show (Just zipped_tree12)) $ do
            (N.zipTree tree1 tree2) `shouldBe` (Just zipped_tree12)
            (B.zipTree tree1 tree2) `shouldBe` (Just zipped_tree12)
            (M.zipTree tree1 tree2) `shouldBe` (Just zipped_tree12)
            (A.zipTree tree1 tree2) `shouldBe` (Just zipped_tree12)
        it ("Tree 3 & Tree 4:" ++ show (Just zipped_tree34)) $ do
            (N.zipTree tree3 tree4) `shouldBe` (Just zipped_tree34)
            (B.zipTree tree3 tree4) `shouldBe` (Just zipped_tree34)
            (M.zipTree tree3 tree4) `shouldBe` (Just zipped_tree34)
            (A.zipTree tree3 tree4) `shouldBe` (Just zipped_tree34)
        it ("Tree 1 & Tree 3: Nothing") $ do
            (N.zipTree tree1 tree3) `shouldBe` (Nothing)
            (B.zipTree tree1 tree3) `shouldBe` (Nothing)
            (M.zipTree tree1 tree3) `shouldBe` (Nothing)
            (A.zipTree tree1 tree3) `shouldBe` (Nothing)
        it ("Tree 2 & Tree 4: Nothing") $ do
            (N.zipTree tree2 tree4) `shouldBe` (Nothing)
            (B.zipTree tree2 tree4) `shouldBe` (Nothing)
            (M.zipTree tree2 tree4) `shouldBe` (Nothing)
            (A.zipTree tree2 tree4) `shouldBe` (Nothing)