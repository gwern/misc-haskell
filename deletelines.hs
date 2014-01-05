#!/usr/bin/env runhaskell

import System.Environment (getArgs)
import CLIUtils (deleteL)

main :: IO ()
main = getArgs >>= \ ~[n, file] -> deleteL (read n :: Integer) file
