-- run this in the master directory of repos; for me, that is ~/bin
import Control.Monad (filterM)
import Data.List (isPrefixOf)
import Network.HTTP (getResponseBody, getRequest, simpleHTTP)
import System.Directory (doesDirectoryExist)
import System.FilePath (takeBaseName)
import System.Process (runProcess)
import Text.HTML.TagSoup (parseTags, Tag(TagOpen))

main :: IO ()
main = do u <- openURL "http://patch-tag.com/publicrepositoriessimplelisting"
          targets <- filterM (fmap not . doesDirectoryExist . takeBaseName) $ extractURLs u
          mapM_ (\x ->runProcess "darcs" ["get", "--lazy", "http://patch-tag.com"++x]
                          Nothing Nothing Nothing Nothing Nothing) targets

openURL :: String -> IO String
openURL x = getResponseBody =<< simpleHTTP (getRequest x)

extractURLs :: String -> [String]
extractURLs arg = [x | TagOpen "a" atts <- parseTags arg,
                       (_,x) <- atts,
                       "/r/" `isPrefixOf` x]