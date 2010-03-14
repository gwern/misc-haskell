import Text.HTML.Download (openURL)
import Text.HTML.TagSoup (parseTags, Tag(TagOpen))
import Data.List (isInfixOf, isPrefixOf)
import Control.Monad (liftM,liftM2, zipWithM_)

main :: IO ()
main = do urls <- liftM extractURLs $ openURL "http://haskell.org/haskellwiki/Xmonad/Config_archive"
          -- urls are prefixed "/haskellwiki/", so in raws, drop 13:
          let raws = map (\x -> "http://haskell.org/haskellwiki/?title=" ++ drop 13 x ++ "&action=raw") $ filter (isInfixOf "xmonad.hs") urls
          let hs = map (liftM2 fmap filterHaskell openURL) raws
          zipWithM_ (\x y -> writeFile x =<< y) (map ((++ ".hs") . show) [(1::Int)..]) hs

filterHaskell :: String -> String -> String
filterHaskell url = addLink . unlines . takeWhile (not . isPrefixOf "</haskell>") . drop 1 . dropWhile (not . isPrefixOf "<haskell>") . lines
    where addLink = (("-- " ++ (reverse . drop 11 . reverse) url ++ "\n") ++) -- add commented url less "&action=raw"

extractURLs :: String -> [String]
extractURLs arg = [x | TagOpen "a" atts <- (parseTags arg), (_,x) <- atts, "/haskellwiki/Xmonad/Config_archive/" `isPrefixOf` x]