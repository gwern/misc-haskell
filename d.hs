#!/usr/bin/env runhaskell

import System.Environment (getArgs, getEnv)
import qualified Data.ByteString.Char8 as B (ByteString(), readFile,
                                             pack, lines, isInfixOf, putStr)
import Control.Monad (join, liftM, liftM2)

main :: IO ()
main = (liftM (head) $ liftM2 (filter . B.isInfixOf) dir dirs) >>= B.putStr
       where
         dir :: IO B.ByteString
         dir = liftM (B.pack . head) $ getArgs

         home :: IO String
         home = getEnv "HOME"

         dirs :: IO [B.ByteString]  -- storage file
         dirs = (liftM B.lines) $ join $ liftM (B.readFile . (++ "/.dload-dirs")) home