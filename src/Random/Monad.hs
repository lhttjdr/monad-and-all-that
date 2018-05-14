module Random.Monad (
    randomInt, randomList, unRandom
    )where

import System.Random (StdGen, split, next)
import Prelude hiding (Monad, return, (>>=))

class Monad m where
    return :: a -> m a
    (>>=)  :: m a -> (a -> m b) -> m b

newtype Random a =
    MkRandom { unRandom :: StdGen -> a }

instance Monad Random where
    return a = MkRandom (\seed -> a)
    x >>= f = MkRandom (\seed ->
        let (seed1, seed2) = split seed
            a              = unRandom x seed1
         in unRandom (f a) seed2)

generate:: Random Int
generate = MkRandom (\seed -> fst (next seed))

randomInt :: Int -> Random Int
randomInt bound = MkRandom(\seed ->
    let (n, seed') = next seed in
    n `mod` bound)

randomList:: Random a -> Random [a]
randomList randomEl =
    randomInt 5 >>= \n ->
        case n of
            0 -> return []
            _ -> randomEl >>= \x ->
                     randomList randomEl >>= \xs ->
                     return (x: xs)