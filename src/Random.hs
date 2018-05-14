module Random (
    randomInt, randomPair
    )where

import System.Random (StdGen, split, next)

randomInt bound seed =
    let (n, seed') = next seed in n `mod` bound

randomPair randomFst randomSnd seed =
    let (seed1, seed2) = split seed in
    (randomFst seed1, randomSnd seed2)