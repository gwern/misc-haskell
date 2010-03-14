{-# LANGUAGE ScopedTypeVariables #-}
module Main () where

import Data.List (isPrefixOf)
import Control.Concurrent (forkIO)
import System.IO.Unsafe (unsafeInterleaveIO)
import qualified Data.ByteString.Lazy.Char8 as B (ByteString(), getContents, unpack, words)
import Text.HTML.TagSoup (parseTags, Tag(TagOpen))
import Text.HTML.Download (openURL)
import Control.Exception as E (catch, SomeException)

main :: IO ()
main = mapM_ (forkIO . archiveURL) . concat =<< mapM fetchArticleURLs  =<< fmap B.words B.getContents

uniq :: [String] -> [String]
uniq = filter (\x -> x `elem`
                        ["http://wikimediafoundation.org/",
                        "http://wikimediafoundation.org/wiki/Deductibility_of_donations",
                        "http://wikimediafoundation.org/wiki/Fundraising",
                        "http://wikimediafoundation.org/wiki/Privacy_policy",
                        "http://www.mediawiki.org/",
                        "http://www.wikimediafoundation.org"])

fetchArticleURLs :: B.ByteString -> IO [String]
fetchArticleURLs article = E.catch (fmap extractURLs $ unsafeInterleaveIO $ openURL("http://en.wikipedia.org/wiki/" ++ B.unpack article))
                                    (\(_ :: SomeException) -> return [])

extractURLs :: String -> [String]
extractURLs arg = uniq $ [x | TagOpen "a" atts <- (parseTags arg), (_,x) <- atts, "http://" `isPrefixOf` x]

archiveURL :: String -> IO ()
archiveURL url = E.catch (openURL("www.webcitation.org/archive?url=" ++ url ++ "&email=foo@bar.com") >> return ())
                         (\(_ :: SomeException) -> return ())