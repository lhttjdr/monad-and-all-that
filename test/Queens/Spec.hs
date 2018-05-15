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

standard :: Int -> Set [Int]
standard n = fromList $ queues' n

test :: Int -> Set [Int]
test n = fromList ((queens n)::[[Int]])

main :: IO ()
main = hspec $ do
    describe "N Queens Problem" $ do
        it ("8 queens") $ do
            (test 8) `shouldSatisfy` (== (standard 8))
        it ("7 queens") $ do
            (test 7) `shouldSatisfy` (== (standard 7))