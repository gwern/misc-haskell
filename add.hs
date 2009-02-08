{-

Here we have two examples of how we can define a abstract data type with a bunch
of fields, serialize them to disk (using arbitrary Ints to represent each field)
and then read them back in and restore the original list or whatevers of
abstract data types.

The question is, how can we turn functions into data which can be serialized
this way?
-}
{- http://hpaste.org/4265 -}
{-
module Main (main)
    where
import Data.Binary
data Math = Add | Subtract | Multiply
    deriving Show

eval :: (Num a) => Math -> a -> a -> a
eval f = case f of
           Add -> (+)
           Subtract -> (-)
           Multiply -> (*)

instance Binary Math where
      put Add = putWord8 0
      put Subtract = putWord8 1
      put Multiply = putWord8 2
      get = do tag_ <- getWord8
               case tag_ of
                 0 -> return Add
                 1 -> return Subtract
                 2 -> return Multiply


main = do encodeFile "tmp.s" [Add, Subtract, Multiply]
          a <- decodeFile "tmp.s"
          putStr $ show (a :: [Math])
-}

import Prelude hiding (reverse)
import Data.Binary
data STRING = Foldl | Flip | Colon | EmptyList
    deriving Show

-- eval :: (String a) => STRING -> a -> a -> a
-- eval f = case f of
--            Foldl -> (foldl1)
--            Flip -> (flip)
--            Colon -> (:)
--            EmptyList -> ([])


instance Binary STRING where
       put Foldl = putWord8 0
       put Flip = putWord8 1
       put Colon = putWord8 2
       put EmptyList = putWord8 3
       get = do tag_ <- getWord8
                case tag_ of
                  0 -> return Foldl
                  1 -> return Flip
                  2 -> return Colon
                  3 -> return EmptyList

main = do print              [Foldl, Flip, Colon, EmptyList]
          encodeFile "tmp.s" [Foldl, Flip, Colon, EmptyList]
          a <- decodeFile "tmp.s"
          print (a :: [STRING])

-- foldl $ flip (:) []
