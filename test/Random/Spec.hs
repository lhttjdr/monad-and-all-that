import Test.Hspec
import Test.QuickCheck
import Control.Exception (evaluate)

import Random (randomInt, randomPair)
import Random.Naive as N hiding (randomInt)
import Random.Monad as M hiding (randomInt)

import System.Random (StdGen, mkStdGen)

seed :: StdGen
seed = mkStdGen 314159265

main :: IO ()
main = hspec $ do
    describe "a bounded random non-negtive integer x" $ do
        it ("Test property: x <= bound") $ do
            property $ \bound -> let x = randomInt bound seed in
                                     case bound of
                                         _ | bound <= 0 -> x == 0
                                           | otherwise -> x <= bound
    describe "a pair of bounded random non-negtive integer (x, y)" $ do
        it ("Test property: x <= boundX, y <= boundY") $ do
            property $ \boundX boundY -> let (x, y) = randomPair (randomInt boundX) (randomInt boundY) seed in
                                     case boundX of
                                         _ | boundX <= 0 -> x == 0
                                           | otherwise -> x <= boundX
                                     &&
                                     case boundY of
                                         _ | boundY <= 0 -> y == 0
                                           | otherwise -> y <= boundY
    describe "a bounded random non-negtive integer list [x]" $ do
        it ("Test property: for all x, x <= bound") $ do
            property $ \bound -> let xs = N.randomList (randomInt bound) seed in
                                     all (\x -> case bound of
                                                     _ | bound <= 0 -> x == 0
                                                       | otherwise  -> x <= bound) xs