import System.Environment (getArgs)
import Language.Haskell.Exts
import qualified Data.Foldable as F (concat)
import Data.Generics.Uniplate.Data

main :: IO ()
main = do (func:_) <- getArgs
          args <- fmap lines $ getContents
          imports <- join $ mapM parseForImports args
          -- insert each import module into the IntMap
          -- print out IntMap sorting by module (descending order)

parseForImports :: String -> FilePath -> IO [Imports]
parseForImports fn fs = do print fs
                         x <- readFile fs
                         let exts = F.concat $ readExtensions x
                         let parsed = parseFileContentsWithMode (defaultParseMode { fixities = fixes, extensions = exts }) x
                         case parsed of
                          ParseFailed _ _ -> (return [])
                          ParseOk a -> return $ moduleCount a
            where -- the default fixities augmented with everything necessary to parse my corpus
                  fixes :: [Fixity]
                  fixes = baseFixities++[Fixity AssocRight 0 (VarOp (Symbol "==>"))]

functionSearch :: Module -> [Imports]
functionSearch md = do undefined
--  let x = length [ () | Var (UnQual (Ident a)) <- universeBi md, a == fun]