#!/usr/bin/env runhaskell

import System.Environment (getArgs)
import CLIUtils (pager)

{- Given a list of files, run less on them or cat, depending on whether
   they are compressed or short enough to fit on the screen. -}
main :: IO ()
main = mapM_ pager =<< getArgs
