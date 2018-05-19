module Main where

import Parser.Static
import Control.Applicative



main :: IO ()
main = print (runStatic ((some ((exactly ' ')::Static Char) *> (exactly 'x')::Static Char)))

-- putStrLn "Please use `stack test`!"