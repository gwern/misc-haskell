#!/usr/bin/env runhaskell

import HSH (runSL)
import Monad (liftM)
import Data.List (genericIndex)

main :: IO ()
main = do usage <- liftM (flip genericIndex (1 :: Integer)) (liftM words $ runSL ("grep -- eth0 /proc/net/dev"))
          putStrLn $ show ((read usage :: Integer) `div` gig) ++ " gigabytes used."
                   where gig = 1073741824 :: Integer -- 2^30
