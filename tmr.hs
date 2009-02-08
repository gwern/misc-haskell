import Text.HTML.TagSoup.Render
import Text.HTML.TagSoup

main :: IO ()
main = interact convertPre

convertPre :: String -> String
convertPre = renderNoQuoting . map convertToHaskell . canonicalizeTags . parseTags
             where renderNoQuoting = renderTagsOptions (renderOptions{optEscape = (:[])})

convertToHaskell :: Tag -> Tag
convertToHaskell (TagOpen "pre" atts) = TagOpen "haskell" atts
convertToHaskell (TagClose "pre") = TagClose "haskell"
convertToHaskell x = x

{- Parsec version from John MacFarlane:

import Text.ParserCombinators.Parsec

pPreHaskell = try $ do
  string "<pre class=\"haskell\">"
  body <- manyTill anyChar (try $ string "</pre>")
  return $ "<haskell>" ++ body ++ "</haskell>"

pOther = many (noneOf "<") <|> string "<"

pDoc = many (pPreHaskell <|> pOther) >>= return . concat

main = do
  c <- getContents
  case parse pDoc "" c of
       Left err  -> error $ show err
       Right res -> putStrLn res
-}