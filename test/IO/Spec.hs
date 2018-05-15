import Test.Hspec
import Test.QuickCheck
import Control.Exception (evaluate)

import IO

main :: IO ()
main = hspec $ do
    describe "IO Monad - task of changing the world" $ do
        it ("Test IO task : print 'Hello Haskell!' ") $ do
            changeWorld