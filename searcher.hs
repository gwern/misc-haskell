import System.Environment (getArgs)
import Language.Haskell.Exts
import qualified Data.Foldable as F (concat)
import Data.Generics.Uniplate.Data
-- import Debug.Trace

main :: IO ()
main = do (func:_) <- getArgs
          args <- fmap lines $ getContents
          mapM_ (checkAndPrint func) args

checkAndPrint :: String -> FilePath -> IO ()
checkAndPrint fn fs = do print fs
                         x <- readFile fs
                         let exts = F.concat $ readExtensions x
                         let parsed = parseFileContentsWithMode (defaultParseMode { fixities = fixes, extensions = exts }) x
                         case parsed of
                          ParseFailed _ _ -> (return ())
                          ParseOk a -> functionSearch fn a
                         return ()

-- the default fixities augmented with everything necessary to parse my corpus
fixes :: Maybe [Fixity]
fixes = Just $ baseFixities ++ infixr_ 0 ["==>"]

functionSearch :: String -> Module -> IO ()
functionSearch fun md = do
  let x = length [ () | Var (UnQual (Ident a)) <- universeBi md, a == fun]
  putStrLn $ "Found " ++ show x ++ " occurences of function " ++ fun