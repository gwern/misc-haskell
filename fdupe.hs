import Split (split)
import Control.Monad (liftM)

main :: IO ()
main = do arg <- liftM (concat . map (fun . lines) . breakBlankLines) $ getContents
          mapM_ putStrLn arg

breakBlankLines :: String -> [String]
breakBlankLines = map (\x -> if head x == '\n' then tail x else x) . filter (not . (==) "") . map concat . split (=="") . split (=='\n')

fun :: [String] -> [String]
fun strs = map (\x -> "ln -sf " ++ "\'" ++ (head strs) ++ "\' " ++ "\'" ++ x ++ "\'") $ tail strs
