import Test.Hspec
import Test.QuickCheck
import Control.Exception (evaluate, NoMethodError(..))


import Control.Applicative
import Parser.Naive as N
import Parser.BeforeApplicative as B
import Parser.Applicative as A
import Parser.Static as S

main :: IO ()
main = hspec $ do
    describe "Parser = State Monad Transformer + List Monad (Backtracking)" $ do
        it ("#1 : (1+2+3)*5-5") $ do
            (N.runParser N.expr "(1+2+3)*5+5") `shouldBe` [(35,""),(30,"+5"),(6,"*5+5")]
        it ("#2 : (1*2+3*(4+5+6*7))+4+5*(1+2)") $ do
            (N.runParser N.expr "(1*2+3*(4+5+6*7))+4+5*(1+2)") `shouldBe`[(174,""),(164,"*(1+2)"),(159,"+5*(1+2)"),(155,"+4+5*(1+2)")]
    describe "Parser (liftM revised version)" $ do
        it ("#1 : (1+2+3)*5-5") $ do
            (B.runParser B.expr "(1+2+3)*5+5") `shouldBe` [(35,""),(30,"+5"),(6,"*5+5")]
        it ("#2 : (1*2+3*(4+5+6*7))+4+5*(1+2)") $ do
            (B.runParser B.expr "(1*2+3*(4+5+6*7))+4+5*(1+2)") `shouldBe`[(174,""),(164,"*(1+2)"),(159,"+5*(1+2)"),(155,"+4+5*(1+2)")]
    describe "Parser (Monadic version)" $ do
        it ("#1 : (1+2+3)*5-5") $ do
            (A.runMonadic A.expr "(1+2+3)*5+5") `shouldBe` Just (35,"")
        it ("#2 : (1*2+3*(4+5+6*7))+4+5*(1+2)") $ do
            (A.runMonadic A.expr "(1*2+3*(4+5+6*7))+4+5*(1+2)") `shouldBe` Just (174,"")
    describe "Static: (1) Empty: if parser can match empty string" $ do
        it ("#1 : expression should not match empty") $ do
            (S.runEmpty S.expr) `shouldBe` False
        it ("#2 : zero or more expressions can match empty") $ do
            (S.runEmpty (many S.expr)) `shouldBe` True
    describe "Static: (2) Starts: list characters that parser can accept at first position" $ do
        it ("#1 : `x|y`") $ do
            (S.runStarts (exactly 'x' <|> exactly 'y')) `shouldBe` "xy"
        it ("#2 : `some (exactly 'x'))`") $ do
           evaluate(S.runStarts (some (exactly 'x'))) `shouldThrow` anyException
    describe "Static: combine (1) and (2)" $ do
        it ("#1 : `x|y`") $ do
            (S.runStatic ((exactly 'x') <|> (exactly 'y'))) `shouldBe` ("xy",False)
        it ("#2 : `some (exactly ' ') *> exactly 'x')`") $ do
            (S.runStatic ((some (exactly ' ') *> exactly 'x'))) `shouldBe` (" ",False)
        it ("#3 : `many (exactly ' ') *> exactly 'x')`") $ do
            (S.runStatic (many (exactly ' ') *> exactly 'x')) `shouldBe` (" x",False)
        it ("#4 : `expr`") $ do
            (S.runStatic S.expr) `shouldBe` ("0123456789(",False)