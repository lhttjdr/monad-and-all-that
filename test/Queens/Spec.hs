import Test.Hspec
import Test.QuickCheck
import Control.Exception (evaluate)

import Queens

import Data.List
import Data.Set hiding (filter)

queues' :: Int -> [[Int]]
queues' n = filter (\x -> test (zip [1..n] x)) candidates
    where candidates = permutations [1..n]
          unSafe (x1, y1) (x2, y2) = (y1 == y2) ||
                                     ((abs (x1 - x2)) == (abs (y1 - y2)))
          test [] = True
          test (q:qs) = (not $ (any (\x -> unSafe q x) qs)) && (test qs)

unordered :: (Ord a) => [[a]] -> Set [a]
unordered = fromList

main :: IO ()
main = hspec $ do
    describe "N Queens Problem" $ do
        it ("8 queens") $ do
            (unordered ((queens 8)::[[Int]])) `shouldBe` (unordered (queues' 8))
        it ("7 queens") $ do
            (unordered ((queens 7)::[[Int]])) `shouldBe` (unordered (queues' 7))