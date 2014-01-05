import Data.Maybe (fromJust)
import Distribution.PackageDescription
import Distribution.PackageDescription.Parse
import Control.Monad (unless)

main :: IO ()
main = do cbl <- getContents
          let desc = parsePackageDescription cbl
          case desc of
            ParseFailed _ -> return ()
            ParseOk _ d -> do let repos = repoPair $ extractHead $ extractRepos d
                              let cmd = concatMap shellify repos
                              unless (null cmd) $ putStrLn cmd

shellify :: (RepoType, String) -> String
shellify (rt,url) = case rt of
                       Darcs -> "darcs get " ++ url
                       Git -> "git clone " ++ url
                       SVN -> "svn clone " ++ url
                       CVS -> "cvs co " ++ url
                       Mercurial -> "hg clone " ++ url
                       _ -> ""

repoPair :: [SourceRepo] -> [(RepoType, String)]
repoPair = map (\x -> (fromJust $ repoType x, fromJust $ repoLocation x))

extractHead :: [SourceRepo] -> [SourceRepo]
extractHead rs = filter (\x -> isnothing x && ishead x) rs
                where ishead sr = case repoKind sr of
                        RepoHead -> True
                        _ -> False
                      isnothing ss = case repoType ss of
                                       Nothing -> False
                                       Just _ -> case repoLocation ss of
                                                     Nothing -> False
                                                     Just _ -> True

extractRepos :: GenericPackageDescription -> [SourceRepo]
extractRepos = sourceRepos . packageDescription
