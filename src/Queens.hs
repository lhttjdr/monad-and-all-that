module Queens(
    queens
    ) where

import Data.List
import Control.Monad

-- `qs :: [a]` is the row labels of queens on (n-1)-th, (n-2)-th, (n-3)-th ... columns
-- `q :: a` is the row label of queen on n-th column

safe q qs = q `notElem` concat[
    qs, -- same row
    zipWith (+) qs [1..], -- upper-left
    zipWith (-) qs [1..] -- lower-left
    ]

queens 0 = return []
queens n = do
    qs <- queens (n-1)
    q <- foldr1 mplus (map return [1..8])
    guard (safe q qs)
    return (q:qs)