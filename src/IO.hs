{-# LANGUAGE UnboxedTuples #-} -- basic tuple without advanced features
{-# LANGUAGE MagicHash #-} -- to access internal types/functions/variables (ending with #)
module IO (
    changeWorld
    )where

import GHC.Base (IO(..), unIO, State#, RealWorld)

-- newtype IO a = IO (State# RealWorld -> (# State# RealWorld, a #))
-- unIO :: IO a -> (State# RealWorld -> (# State# RealWorld, a #))
-- Thus, IO (unIO a) = a

-- It is impossible to obtain `State# RealWorld` variables directly.
-- For demonstration, using `unIO` can get event to "change" world.

-- change the world by printing "Haskell!" on screen
event1 :: State# RealWorld -> (# State# RealWorld, () #)
event1 = unIO (putStr "Haskell!")

-- change the world by printing "Hello, " on screen
event2 :: State# RealWorld -> (# State# RealWorld, () #)
event2 = unIO (putStr "Hello, ")

-- change the world by printing "\n" on screen
event3 :: State# RealWorld -> (# State# RealWorld, () #)
event3 = unIO (putStr "\n")

-- put event1, event2, event3 on timeline
event :: State# RealWorld -> (# State# RealWorld, () #)
event world0 = let (# world2, _ #) = event1 world1
                   (# world1, _ #) = event2 world0
                   (# world3, r #) = event3 world2
               in  (# world3, r #)

-- make event a task, which means the event is delayed
changeWorld = IO event