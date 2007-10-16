#!/usr/bin/env runhaskell

import qualified Data.ByteString.Char8 as B (unwords, words, readFile, putStrLn)
import Control.Monad (liftM, replicateM)
import System.Random (getStdRandom, randomR, mkStdGen)

main :: IO ()
main = do list <- liftM B.words $ B.readFile file -- :: IO [Data.ByteString.Base.ByteString]
          rns <- replicateM 10 $ getStdRandom $ (randomR (1,(length list))) :: IO [Int]
          B.putStrLn (B.unwords $ map (\z -> list !! (z - 1)) rns)
               where file = "/home/gwern/.sig.echelon" :: String

{-
There is a neat algorithm for selecting random elements of a list which does not require
knowing the length of the list; in other words, it would work on infinite as well as finite lists.

"The trick is to select a line by random chance, based on the number of lines we've read so far. The first line we will select 100% of the time. 50% of the time we will then replace it with the second line. 33.3% of the time we will replace that choice with the third line. Etc. The end result will be that we have fairly selected a random line just by reading through the File once."

The idea is: the first entry has 1/2 a chance of being selected; the second 1/3 and so on. You scan
down the list until such time as an entry is selected. But how to do this cleanly in Haskell?

The following is a broken sketch (maybe I should look at the guy's code...):
-- number :: [b] -> [(t, b)]
-- number list = zip [2..] list
 or
number = zip [1..] [2..]

iter tup = if  try == 0 then iter (tail tup) else try
       where f = head tup
                 try = (getStdRandom (randomR f))

main list = list !! (iter number)

(randomR :: RandomGen g => (a, a) -> g -> (a, g))

-}

n :: [b] -> b
n f = snd $ m $ zip [2..] f
--m :: [a] -> a
--m as = if rand (head as) == 1 then (head as) else (m as)
m = undefined
rand foo = fst $ randomR (1,foo) (mkStdGen 5)

module UnsafeRandom (pick) where
import System.Random
import Foreign

pick :: [a] -> a
pick rl = unsafePerformIO $ do
            r <- case rl of
                   []       -> error "empty list"
                   [x]      -> return x
                   z@(x:xs) -> pick' x xs $ length z
            return r
    where
      pick' x [] _ = do return x
      pick' x (y:ys) z = do
        r <- (getStdRandom . randomR) (1,z)
        let c' = if r == 1 then y else x
        pick' c' ys (z+1)

 > pick :: [a] -> IO a
 > pick []     = undefined
 > pick [x]    = do return x
 > pick (x:xs) = pick' x xs 2
 >
 > pick' :: (Num p, Random p) => t -> [t] -> p -> IO t
 > pick' curr []          _    = do return curr
 > pick' curr (next:rest) prob
 >   = do r <- getStdRandom (randomR (1,prob))
 >        let curr' = if r == 1 then next else curr
 >        pick' curr' rest (prob+1)
