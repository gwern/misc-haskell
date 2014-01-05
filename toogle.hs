{-
write function-based search:
   Example code: `(find-fn 1 2 3) -> [clojure.add]`; `(+ 1 2) -> 3`
   but there are lots of `Int -> Int -> Int` functions! So it returns multiple results:
   it answers `2 2 4` with `[clojure.core/unchecked-multiply clojure.core/+ clojure.core/* clojure.core/unchecked-add]` (since 2*2=4 and 2+2=4)
   it searches based on the function running in a jailed environment for that reason
   in haskell: Lambdabot already does type inference, type searching, and safe evaluation, so your 1 2 3 example would go like split on whitespace, map a :t type-inferrer, run a Hoogle search on x -> y -> ... -> z, and evaluate using the first _n_ hits and print out all the ones that didn't error out up to _x_ successes
-}
import System.Environment

main = do args <- getArgs
          exprs <- mapM (\foo -> spawn $ "ghc -e \':type " ++ foo ++ " | tail -1")
          let paramtypes = map (drop 2 $ takeWhile (\x -> x /= ':')) exprs
          let ftype = concat $ intersperse " -> " paramtypes
          functions <- fmap lines $ spawn $ "hoogle '" ++ x ++ "'"
          eval functions (concat args)
          where eval :: [String] -> String -> IO ()
                eval (f:fs) args = do (status,res) <- spawn $ "mueval -e " ++ f ++ args
                                      case status of
                                        Succeeded -> print (f ++ " " ++ res)
                                        Failed -> eval fs args