import Test.Hspec
import Test.QuickCheck
import Control.Exception (evaluate)

import MaybeT

m1 :: MaybeT [] Integer
m1 = MaybeT [ Just 1 , Just 2 , Just 30]

f1 :: Integer -> MaybeT [] Integer
f1 n = MaybeT [ Just n, if n < 10 then Just ( n * 50) else Nothing ]

main :: IO ()
main = hspec $ do
    describe "MaybeT" $ do
        it ("Test `MaybeT [] Integer`: both list and maybe features.") $ do
            runMaybeT (m1 >>= f1) `shouldBe` [Just 1,Just 50,Just 2,Just 100,Just 30,Nothing]