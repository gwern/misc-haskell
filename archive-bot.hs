{- Module      :  Main.hs
   License     :  public domain
   Maintainer  :  Gwern Branwen <gwern0@gmail.com>
   Stability   :  unstable
   Portability :  portable
   Functionality: retrieve specified articles from Wikipedia and request WebCite to archive all URLs found.
   USE: Print to stdin a succession of Wikipedia article names (whitespace in names should be escaped as '_').
        A valid invocation might be, say: '$echo Fujiwara_no_Teika Fujiwara_no_Shunzei | archive-bot'
        All URLs in [[Fujiwara no Teika]] and [[Fujiwara no Shunzei]] would then be backed up.
        If you wanted to run this on all of Wikipedia, you could take the current 'all-titles-in-ns0'
        gzipped file from [[WP:DUMP]], gunzip it, and then pipe it into archive-bot.
   TODO: send an equivalent request to the Internet Archive.
         Not in any way rate-limited.
   BUGS: Issues redundant archive requests.
         ByteString doesn't handle characters requiring more than 8 bits. This may not be a bug, since
         I am not sure URLs are supposed to contain such characters without escaping into 8 bit form. -}

module Main () where
import Monad (liftM) -- Is there any particular order you're s'posed to go in?
import Data.List (isPrefixOf)
import Data.List ()
import Control.Concurrent (forkIO)
import System.IO.Unsafe -- (unsafeInterleaveIO)
import System.IO.Unsafe ()
import qualified Data.ByteString.Char8 as B (ByteString(), readFile, pack, unpack, words)
import Text.HTML.TagSoup (parseTags, Tag(TagOpen))
import Text.HTML.Download (openURL)
import CLIUtils (deleteL)

main :: IO ()
main = ((forkIO . archiveBot) =<< (liftM (take 1000) $ liftM B.words $ B.readFile en)) >> deleteL 1000 en >> main
              where en = "/home/gwern/bin/pywikipedia/en"

archiveBot :: [B.ByteString] -> IO ()
archiveBot ls = mapM_ (archiveURL) =<< (liftM uniq $ mapM fetchArticleURLs ls)
                where uniq :: [[B.ByteString]] -> [B.ByteString] -- So hideous
                      uniq = filter (\x -> if x == B.pack "http://wikimediafoundation.org/" || x == B.pack "http://wikimediafoundation.org/wiki/Deductibility_of_donations" || x == B.pack "http://wikimediafoundation.org/wiki/Fundraising" || x == B.pack "http://wikimediafoundation.org/wiki/Privacy_policy" || x == B.pack "http://www.mediawiki.org/" || x == B.pack "http://www.wikimediafoundation.org" then False else True) . concat

fetchArticleURLs :: B.ByteString -> IO [B.ByteString]
-- fetchArticleURLs article = liftM extractURLs $ unsafeInterleaveIO $ openURL("http://en.wikipedia.org/wiki/" ++ B.unpack article)
fetchArticleURLs = fmap extractURLs . unsafeInterleaveIO . openURL . ("http://en.wikipedia.org/wiki/" ++) . B.unpack

extractURLs :: String -> [B.ByteString]
extractURLs arg = map B.pack $ [x | TagOpen "a" atts <- (parseTags arg), (_,x) <- atts, "http://" `isPrefixOf` x]

archiveURL :: B.ByteString -> IO ()
archiveURL url = openURL("www.webcitation.org/archive?url=" ++ (B.unpack url) ++ "&email=foo@mailinator.com") >> return ();
