import Test.Hspec
import Test.QuickCheck
import Control.Exception (evaluate)

import BackT

main :: IO ()
main = hspec $ do
    describe "BackT = List Monad Transformer (Backtracking features)" $ do
        it ("Test `BackT Maybe Integer`") $ do
            (runBackT ((return 1 `mplus` return 2 `mplus` return 3)::BackT Maybe Integer)) `shouldBe` Just [1,2,3]
            (runBackT ((mzero)::BackT Maybe Integer)) `shouldBe` Just []
        it ("Test `BackT [] Integer`") $ do
            (runBackT ((return 1 `mplus` return 2 `mplus` return 3)::BackT [] Integer)) `shouldBe` [[1,2,3]]
            (runBackT ((mzero)::BackT [] Integer)) `shouldBe` [[]]