#!/usr/bin/env runhaskell

module Main (main) where

import System.Environment (getArgs)
import CLIUtils (lengthF)

main :: IO ()
main = mapM_ lengthF =<< getArgs

