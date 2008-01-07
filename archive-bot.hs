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
import Control.Concurrent (forkIO)
import System.IO.Unsafe (unsafeInterleaveIO)
import qualified Data.ByteString.Lazy.Char8 as B (ByteString(), readFile, pack, unpack, lines)
import Text.HTML.TagSoup (parseTags, Tag(TagOpen))
import Text.HTML.Download (openURL)

main :: IO ()
main = (archiveBot =<< (liftM B.lines $ B.readFile en)) >> return ()
              where en = "/home/gwern/bin/pywikipedia/en"

archiveBot :: [B.ByteString] -> IO ()
archiveBot ls = mapM_ (archiveURL) =<< (liftM uniq $ mapM fetchArticleURLs ls)
                where uniq :: [[B.ByteString]] -> [B.ByteString] -- So hideous
                      uniq = filter (`notElem` exceptions) . concat
                      exceptions = map B.pack ["http://wikimediafoundation.org/", "http://wikimediafoundation.org/wiki/Deductibility_of_donations", "http://wikimediafoundation.org/wiki/Fundraising", "http://wikimediafoundation.org/wiki/Privacy_policy", "http://www.mediawiki.org/", "http://www.wikimediafoundation.org"]

fetchArticleURLs :: B.ByteString -> IO [B.ByteString]
fetchArticleURLs = fmap extractURLs . unsafeInterleaveIO . openURL . ("http://en.wikipedia.org/wiki/" ++) . B.unpack

extractURLs :: String -> [B.ByteString]
extractURLs arg = map B.pack $ [x | TagOpen "a" atts <- (parseTags arg), (_,x) <- atts, "http://" `isPrefixOf` x]

archiveURL :: B.ByteString -> IO ()
archiveURL url = forkIO (openURL("www.webcitation.org/archive?url=" ++ (B.unpack url) ++ "&email=marudubshinki0@gmail.com") >> return()) >> return ();
