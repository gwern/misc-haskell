#!/usr/bin/env runhaskell

import HSH (runSL)
import CLIUtils (cutR)
import Monad (liftM)

{- TODO:
Previously I had it as:
main = putStrLn =<< (runSL $ "uptime" -|- (cutR ' ' [10..14]))

However, on repeated invocations, it hangs and fails. When changed to the current version,
it does not. Why is that? One is supposed to be able to intersperse pure and shell functions
with -|-.

Need to figure out why.
-}

main :: IO ()
main = putStrLn =<< liftM (cutR ' ' [10..14]) (runSL "uptime")

