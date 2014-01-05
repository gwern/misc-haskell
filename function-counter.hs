import qualified Data.Data
import Language.Haskell.Exts
import qualified Data.Foldable as F (concat)
import Data.Generics.Uniplate.Data
import qualified Data.Map as M
import Data.List
import Data.Ord

main :: IO ()
main = do args <- fmap lines getContents
          db <- fmap (M.unionsWith (+)) $ mapM checkAndCount args
          print $ map show $ sortBy (comparing snd) $ M.toList db

checkAndCount :: FilePath -> IO (M.Map String Int)
checkAndCount   f  = do putStrLn f
                        x <- readFile f
                        let exts = F.concat $ readExtensions x
                        let parsed = parseFileContentsWithMode
                                       (defaultParseMode {fixities = fixes, extensions = exts}) x
                        case parsed of
                         ParseFailed _ _ -> return M.empty
                         ParseOk a -> return (functionSearch a)

-- the default fixities augmented with everything necessary to parse my corpus
fixes :: Maybe [Fixity]
fixes = Just $ baseFixities ++ infixr_ 0 ["==>", "#", ">==",">>==", ">.", "$$"]

functionSearch :: Data.Data.Data from => from -> M.Map String Int
functionSearch md = do
 let xs = [ a | Var (UnQual (Ident a)) <- universeBi md]
 M.fromListWith (+) $ zip xs (repeat 1) -- map (M.insertWith (+) xs 1 $ M.empty)