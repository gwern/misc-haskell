#!/usr/bin/env runhaskell

import HSH (runSL)
import Monad (liftM)

main :: IO ()
main = do usage <- liftM (drop 5 . concat . take 1 . words) $ runSL $ "grep -- eth1 /proc/net/dev"
          putStrLn $ ((take 10) $ show ((read usage :: Double) / gig)) ++ " gigabytes used."
                   where gig = 2^(30 :: Integer) -- 1073741824
