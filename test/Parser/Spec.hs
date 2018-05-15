import Test.Hspec
import Test.QuickCheck
import Control.Exception (evaluate)

import Parser

main :: IO ()
main = hspec $ do
    describe "Parser = State Monad Transformer + List Monad (Backtracking)" $ do
        it ("#1 : (1+2+3)*5-5") $ do
            (runParser expr "(1+2+3)*5+5") `shouldBe` [(35,""),(30,"+5"),(6,"*5+5")]
        it ("#2 : (1*2+3*(4+5+6*7))+4+5*(1+2)") $ do
            (runParser expr "(1*2+3*(4+5+6*7))+4+5*(1+2)") `shouldBe`[(174,""),(164,"*(1+2)"),(159,"+5*(1+2)"),(155,"+4+5*(1+2)")]