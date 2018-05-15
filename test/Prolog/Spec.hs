import Test.Hspec
import Test.QuickCheck
import Control.Exception (evaluate)

import Prolog

import Data.Set

unordered ::(Ord a) => [([a],[a])] -> Set ([a],[a])
unordered = fromList

main :: IO ()
main = hspec $ do
    describe "Prolog like Logic Evaluation" $ do
        it ("[1,2,3]") $ do
            res <- runLogic (test ([1,2,3]::[Integer]))
            (unordered res)
                `shouldBe`
                 (unordered [([],[1,2,3]),([1],[2,3]),([1,2],[3]),([1,2,3],[])])