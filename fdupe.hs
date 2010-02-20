import Data.List.Split (splitOn)

main :: IO ()
main = interact (unlines . map (linkify . lines) . splitOn "\n\n") 

linkify :: [String] -> String
linkify (x:xs) = unlines $ map (\y -> "ln -sf " ++ show x ++ " " ++ show y) xs