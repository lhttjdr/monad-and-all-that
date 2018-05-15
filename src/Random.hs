module Random (
    randomInt, randomPair
    )where

import System.Random (StdGen, split, next)

randomInt :: Int -> StdGen -> Int
randomInt bound seed
    | bound <= 0 = 0
    | otherwise = let (n, seed') = next seed in n `mod` bound

randomPair randomFst randomSnd seed =
    let (seed1, seed2) = split seed in
    (randomFst seed1, randomSnd seed2)