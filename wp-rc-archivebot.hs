import Control.Concurrent (forkIO)
import Control.Monad (liftM, forM_)
import Data.Char (intToDigit)
import Data.List (isInfixOf, isPrefixOf, foldl', deleteBy, nubBy)
import Data.Maybe (fromJust)
import Network.HTTP hiding (port)
import Network.Stream (ConnError)
import Network.URI
import System.Environment (getArgs)

import Text.HTML.TagSoup (parseTags, Tag(TagOpen))

import Text.Feed.Import (parseFeedString)
import Text.Feed.Types (Feed(RSSFeed))
import Text.RSS.Syntax (rssChannel, rssItems, RSSItem(..))

main :: IO ()
main = do args <- getArgs
          -- Webcite requires a valid email, and they filter out public
          -- emails like mailinator.com. So we demand an email from the user.
          let email = head args
          -- This is largely intended for the English Wikipedia, so we default to En's NewPages
          -- but we let the user override; any second argument is assumed to be a MediaWiki RSS
          let url = if length args > 1 then args !! 1
                     else "http://en.wikipedia.org/w/index.php?title=Special:NewPages&feed=rss"
          reader email url

-- inspiried by rss2irc
-- | wait on an RSS thread, updating every so often. Each RSS item links to some diff or page,
-- in addition to whatever other content the RSS item may contain (date, summary, etc.)
-- This runs 'archiveBot' on just that link, ignoring the rest.
reader :: String -> String -> IO ()
reader email url = items url >>= go
 where
   go old = do new <- items url
               -- remove duplicates
               let diff = foldl' (flip $ deleteBy matchingTitles) new old
               forM_ (take 100 diff) $ \itm ->
                case rssItemLink itm of
                    Nothing -> return ()
                    Just t  -> ignore $ forkIO $ archiveBot email t
               go new

   matchingTitles :: RSSItem -> RSSItem -> Bool
   matchingTitles x y = let title = (fromJust . rssItemTitle) in title x == title y

   -- Actually fetch a RSS feed and turn it from String to [RSSItem]
   items :: String -> IO [RSSItem]
   items rurl = do s <- get' rurl
                   let RSSFeed r = fromJust $ parseFeedString s
                   return $ nubBy matchingTitles $ rssItems $ rssChannel r

-- | Given the URL of an article, we suck down the HTML, grep it for http:// links,
-- filter out certain links that we don't want to archive (boilerplate links, interwiki links)
-- and then fire off an archive request for each link left.
archiveBot :: String -> String -> IO ()
archiveBot email ls = liftM uniq (fetchArticleURLs ls) >>= mapM_ (archiveURL email)
 where  uniq :: [String] -> [String] -- So hideous
        uniq = filter (\x ->not $ any (flip isInfixOf x) exceptions)
        exceptions :: [String]
        exceptions = ["wikimediafoundation", "http://www.mediawiki.org/", "wikipedia",
                      "&curid=", "index.php?title=", "&action="]

-- | Run 'extractURLs' on some page's raw HTML
fetchArticleURLs ::  String -> IO [String]
fetchArticleURLs = fmap extractURLs . get'

-- | Use the TagSoup library to extract all the hyperlinks in a page. This is really parsing the HTML,
-- so hopefully there won't be any spurious links.
extractURLs :: String -> [String]
extractURLs arg = [x | TagOpen "a" atts <- parseTags arg, (_,x) <- atts, "http://" `isPrefixOf` x]

-- | Webcitation.org is set up so one can archive a url just by doing a request for 'webcitation.org/archive?url=url&email=email'
-- So it's very easy, given a URL and an email, to archive a page. You don't even need to see what the response was.
archiveURL :: String -> String -> IO ()
archiveURL email url = print url' >> ignore (get' url')
              where url' = "http://www.webcitation.org/archive?url=" ++ escapeURIString isAllowedInURI url ++ "&email=" ++ email

-- | Convenient wrapper over the complexity of Network.HTTP. Given a URL, we get the raw HTML. No fuss, no muss.
-- Of course, this means we paper over a bunch of possible errors and issues, but we've no time for them! There are links to archive!
get' :: String -> IO String
get' = get . fromJust . parseURI
 where get :: URI -> IO String
       get uri = do resp <- simpleHTTP (request uri) >>= handleE (error . show)
                    case rspCode resp of
                        (2,0,0) -> return (rspBody resp)
                        _ -> error (httpError resp)
           where
               httpError resp = showRspCode (rspCode resp) ++ " " ++ rspReason resp
               showRspCode (a,b,c) = map intToDigit [a,b,c]

               request :: URI -> Request String
               request ur = Request{rqURI=ur, rqMethod=GET, rqHeaders=[], rqBody=""}

               handleE :: Monad m => (ConnError -> m a) -> Either ConnError a -> m a
               handleE h (Left e) = h e
               handleE _ (Right v) = return v

-- | Convenience function. 'forkIO' and 'forM_' demand return types of 'IO ()', but most interesting
-- IO functions don't return void. So one adds a call to 'return ()'; this just factors it out.
ignore ::(Monad m) => m a -> m ()
ignore x = x >> return ()