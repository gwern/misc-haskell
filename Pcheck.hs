module Pcheck (parTest, parCheck) where

import Control.Monad (replicateM_, liftM)
import Control.Concurrent.Chan (newChan, writeChan, getChanContents)
import Control.Concurrent (forkIO)
import Test.QuickCheck (quickCheck', Testable())

-- | Takes a list of functions using parCheck, and returns True iff all return
-- True. Evaluates them in parallel.
parTest :: [IO Bool] -> IO Bool
parTest = andTest . parList
    where andTest :: IO [Bool] -> IO Bool
          andTest = liftM and

-- | Takes a list of functions (presumably using parCheck) and evaluates all in parallel.
parList :: [IO a] -> IO [a]
parList fs = do chan <- newChan
                mapM_ (\m -> forkIO (m >>= writeChan chan)) fs
                liftM (take n) $ getChanContents chan
                    where n = length fs

{- | Test in parallel. Forks off a QuickCheck 'n' times; QuickCheck tests using
 the proposition 't'. Returns True if all tests were passed, else
 False. Should be run with parallelizing options like with +RTS -N4 -RTS &etc. -}
parCheck :: (Testable prop) => prop -> Int -> IO Bool
parCheck t n = do chan <- newChan
                  replicateM_ n (forkIO ((writeChan chan) =<< (quickCheck' t)))
                  liftM (and . take n) $ getChanContents chan
