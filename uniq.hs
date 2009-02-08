module Main where
import Control.Monad
import Data.List
import qualified Data.Set as Set

main :: IO ()
main = do files <- (liftM ((\h -> map (!!1) $ Set.toList $ Set.difference (Set.fromList $ i h) (j h))) $ getContents)
          mapM_ (\x -> putStrLn $ "rm -f \'" ++ x ++ "\'") files
                where i =  map words . sort . lines
                      j = Set.fromList . nubBy (\x y -> head x == head y) . i
