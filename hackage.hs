import Text.HTML.Download -- (openURL)
import Text.HTML.TagSoup -- (parseTags, Tag(TagOpen))
import Data.List -- (isInfixOf, isPrefixOf)
import Control.Monad -- (liftM,liftM2, zipWithM_)

main :: IO ()
main = do urls <- liftM extractURLs $ openURL "http://hackage.haskell.org/packages/archive/pkg-list.html"
          print urls
          packages <- mapM openURL urls
          print packages
          let urls' = concatMap extractRepo packages
          mapM_ putStrLn urls'

extractRepo :: String -> [String]
extractRepo arg = map (\(TagText x) -> x) $ (extractDarcsRepo soup : extractGitRepo soup)
     where soup = parseTags arg

extractDarcsRepo :: [Tag String] -> Tag String
extractDarcsRepo arg = TagText $ "darcs get " ++ y
                  where (TagOpen _ ((_,y):[])) =  head $ drop 1 $ dropWhile (\x -> x /= TagText "darcs get ") arg

extractGitRepo :: [Tag String] -> [Tag String]
extractGitRepo arg = filter (\x -> case x of
                                    (TagText z) -> if "git clone " `isInfixOf` z then True else False
                                    _ -> False) arg

extractURLs :: String -> [String]
extractURLs arg = ["http://hackage.haskell.org"++x | TagOpen "a" atts <- (parseTags arg),
                                                     (_,x) <- atts, "/package/" `isPrefixOf` x]