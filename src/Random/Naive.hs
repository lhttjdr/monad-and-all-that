module Random.Naive(
    randomInt, randomList
    ) where

import System.Random (StdGen, split, next)

randomInt :: Int -> StdGen -> Int
randomInt bound seed =
    let (n, seed') = next seed in n `mod` bound

randomList:: (StdGen->a) -> StdGen -> [a]
randomList randomEl seed =
    let (seed1, seed2)=split seed in
    case randomInt 5 seed1 of
        0 -> []
        _ -> let (seed3, seed4) = split seed2 in
             randomEl seed3 : randomList randomEl seed4