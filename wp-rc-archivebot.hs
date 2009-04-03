import Control.Concurrent (forkIO)
import Control.Monad
import Data.Char
import Data.List
import Data.Maybe
import Network.HTTP hiding (port)
import Network.Stream
import Network.URI
import System.Environment
import System.IO
import Text.Feed.Import
import Text.Feed.Types
import Text.HTML.TagSoup (parseTags, Tag(TagOpen))
import Text.RSS.Syntax
import qualified Data.ByteString.Char8 as B
import qualified Data.ByteString.Lazy as BS

main :: IO ()
main = do args <- getArgs
          let url = head args
          print "Starting"
          reader url

archiveBot :: String -> IO ()
archiveBot ls = print ls >> (liftM uniq $ fetchArticleURLs ls) >>= mapM_ archiveURL
                where uniq :: [String] -> [String] -- So hideous
                      uniq = filter (\x -> not $ or $ map (isInfixOf x) exceptions)
                      exceptions = ["http://wikimediafoundation.org/", "http://wikimediafoundation.org/wiki/Deductibility_of_donations", "http://wikimediafoundation.org/wiki/Fundraising", "http://wikimediafoundation.org/wiki/Privacy_policy", "http://www.mediawiki.org/", "http://www.wikimediafoundation.org", "&curid=", "index.php?title="]

fetchArticleURLs ::  String -> IO [String]
fetchArticleURLs x = print x >> (fmap extractURLs $ get' x)

unlazy :: BS.ByteString -> String
unlazy = B.unpack . B.concat . BS.toChunks

extractURLs :: String -> [String]
extractURLs arg = [x | TagOpen "a" atts <- (parseTags arg), (_,x) <- atts, "http://" `isPrefixOf` x]

archiveURL :: String -> IO ()
archiveURL url = forkIO (get' ("www.webcitation.org/archive?url=" ++ url ++ "&email=m@mailinator.com") >> return()) >> return ();

-- stolen from rss2irc & modified
-- | wait on an RSS thread, updating every interval seconds
reader ::  String -> IO ()
reader url = do
  print "reader"
  initialitems <- items url
  print "initial done"
  go initialitems
  print "go"
  where
    go old = do
        new <- items url
        print "items done"
        let diff = (foldl' (flip (deleteBy matchingTitles)) new old)
        forM_ (take 1000 diff) $ \item -> do
            case rssItemLink item of
                Nothing -> return ()
                Just t  -> archiveBot t
        go new


items :: String -> IO [RSSItem]
items url = do
  print url
  s <- get $ fromJust $ parseURI url
  print "parseURI done"
  let RSSFeed r = fromJust $ parseFeedString s
  return $ nubBy matchingTitles $ rssItems $ rssChannel r

matchingTitles :: RSSItem -> RSSItem -> Bool
matchingTitles x y = title x == title y
                     where title = fromJust . rssItemTitle

get' = get . fromJust . parseURI

get :: URI -> IO String
get uri = do
  resp <- simpleHTTP (request uri) >>= handleE (error . show)
  case rspCode resp of
    (2,0,0) -> return (rspBody resp)
    _ -> error (httpError resp)
    where
      httpError resp = showRspCode (rspCode resp) ++ " " ++ rspReason resp
      showRspCode (a,b,c) = map intToDigit [a,b,c]

request :: URI -> Request String
request uri = Request{rqURI=uri, rqMethod=GET, rqHeaders=[], rqBody=""}

handleE :: Monad m => (ConnError -> m a) -> Either ConnError a -> m a
handleE h (Left e) = h e
handleE _ (Right v) = return v
