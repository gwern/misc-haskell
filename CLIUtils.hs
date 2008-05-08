{- Some Unix-like tools written in simple, clean Haskell
TODO: Possibly write a stronger version of FilePath? There could be several levels: filenames which
are legal; filenames which exist; filenames which exist and are writable by the current user.

-}
module CLIUtils
    ( --        main,
             io,
             showln,
             echo,
             uniq,
             rpt,
             headN,
             head10,
             tail',
             tac,
             rev,
             rev_w,
             wc_c,
             wc_l,
             wc_w,
             space,
             unspace,
             remove,
             upper,
             clean,
             clean',
             clean'',
             blank,
             join,
             tr,
             tr_d,
             grep,
             grep_v,
             num,
             cksum,
             cut,
             cutR,
             deleteL,
--             mv,
--             cp,
             lengthF,
             pager,
             isCompressed
            ) where
import Data.List (genericTake, genericDrop, genericIndex, nub, intersperse, delete, isPrefixOf, foldl')
import Data.Char (toUpper, isSpace, ord)
import Text.Printf ( printf )
-- import System.Environment (getProgName, getArgs)
-- import System.IO
import Monad (liftM)
-- import System.Directory (copyFile, renameFile)
import qualified Data.ByteString.Char8 as B (lines, unlines, readFile, writeFile)
import HSH (runIO)
import System.Environment (getEnv)
import Data.List (isInfixOf)

{-
main :: IO ()
main = do
  args <- getArgs
  calledBy <- getProgName
  if (genericlength args > 0) then
      case calledBy of
        "echo" -> mapM_ putStr args
        "grep" -> if ((args !! 0) == "-v") then io grep_v else io grep
        "tr"   -> if ((args !! 0) == "-d") then tr_d (head $ args !! 2) else tr (head $ args !! 0) (head $ args !! 1)
   else case calledBy of
    "blank" -> io blank
    "cksum" -> interact (showln . cksum)
    "clean" -> io clean''
    "echo"  -> io id
    "drop" -> interact drop'
    "head" -> io (return . headN)
    "join" -> io join
    "num" -> io num
    "remove" -> io (remove "str")
    "revw" -> io revw
    "reverse" -> io rev
    "reverseword" -> io revw
    "rpt" ->  io rpt
    "sort" -> interact sort
    "space" -> io space
    "tac" -> interact tac
    "take" -> io take'
    "tail" -> io (return . tail')
    "unspace" -> io unspace
    "upper" -> interact upper
    "uniq" -> interact uniq
    "wc_c" -> interact wc_c
    "wc_l" -> interact wc_l
    "wc_w" -> interact wc_w
    _      -> error "command not found"
-}
{-
-- And our main wrapper
main :: IO ()
main = do
    args <- getArgs
    who <- getProgName
    if (genericLength args > 0)
     then
      case who of
        "echo" -> mapM_ putStr args
        "grep" -> if ((args !! 0) == "-v") then io grep_v else io grep
        "tr"   -> if ((args !! 0) == "-d") then tr_d (Prelude.head $ args !! 2) else tr (Prelude.head $ args !! 0) (Prelude.head $ args !! 1)
        _      -> maybe (return ()) id $ lookup who $
                  [("blank",       io blank                  )
                  ,("cksum",       interact (showln . cksum) )
                  ,("clean",       io clean''                )
                  ,("echo" ,       interact id               )
--                  ,("drop",        interact drop'            )
--                  ,("head",        io (return . Coreutils.head)       )
                  ,("join",        io join                   )
                  ,("num",         io num                    )
                  ,("remove",      io (remove "str")         )
                  ,("revw",        io rev_w                  )
                  ,("reverse",     io rev                    )
                  ,("reverseword", io rev_w                  )
                  ,("rpt",         io rpt                    )
                  ,("sort",        interact sort             )
                  ,("space",       io space                  )
                  ,("tac",         interact tac              )
--                  ,("take",        io take'                  )
                  ,("tail",        io (return . tail')       )
                  --  ,( "tr"  ,    interact tr)
                  --  ,( "tr -d",   interact (tr_d . unwords))
                  ,("unspace",     io unspace                )
                  ,("upper",       interact upper            )
                  ,("uniq",        interact uniq             )
                  ,("wc_c",        interact wc_c             )
                  ,("wc_l",        interact wc_l             )
                  ,("wc_w",        interact wc_w             )
                  ]

     else putStrLn who
-}
-- First, helper functions
io :: ([String] -> [String]) -> IO ()
io f = interact (unlines . f . lines)

showln :: (Show a) => a -> [Char]
showln a = show a ++  "\n"

-- echo.
echo :: [String] -> [String]
echo = id

-- remove duplicate lines from a file (like uniq)
uniq :: String -> String
uniq = nub

-- repeat the input file infinitely
rpt :: [a] -> [a]
rpt = cycle

-- Preserve first n lines
headN :: Int -> String -> String
headN n = concat . genericTake n . lines

-- Return only the first 10
head10 :: String -> String
head10 = headN 10

-- Return the tail -1 line of input
tail' :: [String] -> String
tail' = last

-- Reverse lines in a file (tac)
tac :: String -> String
tac = reverse

-- Reverse characters on each line (rev)
rev, rev_w :: [String] -> [String]
rev = map reverse

-- Reverse words on each line
rev_w = map (unwords . reverse . words)

-- Count number of characters in a file (like wc -c)
wc_c, wc_l, wc_w :: String -> String
wc_c = showln . length

-- Count number of lines in a file, like wc -l
wc_l = showln . length . lines

-- Count number of words in a file (like wc -w)
wc_w = showln . length . words

-- double space a file
space, unspace :: [String] -> [String]
space = intersperse ""

-- undo double space
unspace = filter (not . null)

-- remove the first occurrence of the line "str"
remove :: String -> [String] -> [String]
remove = delete

-- make a string all upper case
upper :: String -> String
upper = map toUpper

-- remove leading space from each line
clean, clean', clean'' :: [String] -> [String]
clean = map (dropWhile isSpace)

-- remove trailing whitespace
clean' = map (reverse . dropWhile isSpace . reverse)

-- delete leading and trailing whitespace
clean'' = map (f . f)
 where f = reverse . dropWhile isSpace

-- insert blank space at beginning of each line
blank :: [String] -> [String]
blank = map (s ++)
 where s = replicate 8 ' '

-- join lines of a file
join :: [String] -> [String]
join = return . concat

-- Translate the letter 'e' to 'f', like tr 'e' 'f' (or y// in sed)
-- tr :: Char -> Char -> IO ()
tr :: Char -> Char -> String -> String
tr a b = map (\x -> if x == a then b else x)

-- Delete characters from a string.
tr_d :: Char -> String -> String
tr_d = filter . (/=)

grep, grep_v :: [String] -> [String]
-- lines matching "^foo" from a file
grep = filter (isPrefixOf "foo")
-- lines that don't match "^foo" (grep -v)
grep_v = filter (not . isPrefixOf "foo")

-- number each line of a file
num :: [String] -> [String]
num = zipWith (printf "%3d %s") [(1::Int)..]

-- Compute a simple cksum of a file
cksum :: [Char] -> Int
cksum = foldl' k 5381
 where k h c = h * 33 + ord c

{- Delete first n lines of a file. May have problems if file size exceeds physical memory. -}
deleteL :: Int -> FilePath -> IO ()
deleteL n file = B.writeFile file =<< (liftM dropN $ B.readFile file)
                 where dropN = B.unlines . genericDrop n . B.lines

-- TODO
-- mv, cp :: FilePath -> FilePath -> IO ()
-- mv = renameFile
-- cp = copyFile
-- rm []     = succeed
-- rm (x:xs) = removeFile x >> rm xs

-- rmdir []     = succeed
-- rmdir (x:xs) = removeDirectory x >> rmdir xs

-- recurmdir []     = succeed
-- recurmdir (x:xs) = removeDirectoryRecursive x >> recurmdir xs

-- mvdir []       = succeed
-- mvdir [_]      = putStrLn "unmatched amount of options" >> failure
-- mvdir (o:n:xs) = renameDirectory o n >> mvdir xs
-- uname _ = do
--            s <- getSystemID
--            putStrLn (concat (intersperse " " [systemName s,nodeName s,release s,version s,machine s]))
--            succeed

-- TODO

{- Utility function.
> split ' ' "foo bar baz" -> ["foo","bar","baz"] -}
split :: Char -> String -> [String]
split c s = case rest of
              []     -> [chunk]
              _:rst -> chunk : split c rst
    where (chunk, rest) = break (==c) s

{- cutR [2..4] ' ' "foo bar baz quux foobar" -> "bar baz quux"
   Might be better to use a Map, but that makes the function much longer for marginal
   performance benefit. -}
cutR :: Char -> [Int] -> String -> String
cutR delim ns y = concat $ intersperse [delim] $ map (\z -> string `genericIndex` (z - 1)) ns
    where string = split delim y

cut :: Char -> Int -> String -> String
cut dlmtr pos string = cutR dlmtr [pos] string

lengthF :: FilePath -> IO Int
lengthF = fmap length . fmap lines . readFile

pager :: String -> IO ()
pager file = do l <- lengthF file
                t <- liftM term termVar
                open (master l t file) (" '" ++ file ++ "'") -- Remember to quote odd names
                    where
                      term = flip any ["linux", "dumb", "console"] . (==) :: String -> Bool
                      termVar = getEnv "TERM" :: IO String

master :: Int -> Bool -> String -> String
master n t file
       | isCompressed file = "less"
       | otherwise = (if t && (n > 50)  then "less"  else if n > 30 then "less" else "cat")

isCompressed :: String -> Bool
isCompressed file = any (flip isInfixOf file) [".bz",".gz",".bz2",".Z",".a",".tar",".cab",".man",".so",".zip",".lha",".cpi",".rpm",".cpi",".cpio",".ace",".arc",".arj",".cab",".lha",".lzh",".zoo",".7z",".mo",".gmo",".rar",".deb"]

open :: String -> String -> IO ()
open prog file = runIO $ prog ++ " " ++ file
