module Split (words', lines', replace, replaceBy, split, split') where
import Data.List -- (intersperse, span, lines)
import Test.QuickCheck -- (quickCheck)
import Data.Char

-- mylines x = case breaks' x of
--                                                           [""] -> []
--                                                           ["\n"] -> [""]
--                                                           a -> if length a >= 3 then init . tail $ a else a
-- --                                                          a  -> (filter (not . (==) '\n') $ concat a)

-- breaks' y
--  | length y >= 3 = init . tail $ breaks  (=='\n')  y
--  | otherwise = breaks  (=='\n')  y

-- breaks :: (a -> Bool) -> [a] -> [[a]]
-- breaks f xs = case span f xs of
--                   (_, []) -> [xs]
--                   (x', xs') ->
--                       case break f xs' of
--                           (v, xs'') ->
--                               x' : v : breaks f xs''

-- prop1 :: (Eq a) => a -> [a] -> Bool
-- prop1 x y = (y == (concat $ intersperse [x] $ breaks (==x) y))

-- proplines x = (lines x == mylines x)

{- Utility function.
> split ' ' "foo bar baz" -> ["foo","bar","baz"] -}
-- split :: Char -> String -> [String]
-- split c s = case rest of
--               []     -> []
--               _:rst -> chunk : split c rst
--     where (chunk, rest) = break (==c) s

-- splitlines :: String -> [String]
-- splitlines x | x == "" = []
--              | x == "\n" = [""]
--              | head x == '\n' = "" : (splitlines $ tail x)
--              | otherwise = if (split' !! ((length split') - 1)) == "" then (dropLast $ split') else split'
--                  where split' = filter (not . (==) "") $ split '\n' x

-- splitlines :: String -> [String]
-- splitlines ('\n':[]) = [""]
-- splitlines str = map (\x -> if x == "\n" then "" else x) $ split '\n' str

-- split :: (Eq a) => a -> [a] -> [[a]]
-- split _ [] = []
-- split x ys
--       | x `elem` ys = map concat $ group $ split' x ys
--       | otherwise = return ys

-- split'' a b = map concat . groupBy (\x y -> not (x == [a] || y == [a])) $ split' a b

-- split''' "" = []
-- split''' a = if length str >= 2 then dropLast str else str
--     where str = replace "\n" "" $ split'' '\n' a

-- split' :: (Eq a) => a -> [a] -> [[a]]
-- split' _ [] = []
-- split' x (y:ys)
--        | x == y = [x] : split' x ys
--        | otherwise = [y] : split' x ys

-- prop' :: String -> Bool
-- prop' x = lines x == split''' x

-- deepCheck :: (Testable prop) => prop -> IO Bool
-- deepCheck = quickCheckWith 1000 2 1000000

-- test :: IO ()
-- test = quickCheck replaceLengthProp >> quickCheck replaceUndoableProp >> quickCheck replaceIdempotentProp >> quickCheck linesProp >> quickCheck wordsProp >> quickCheck splitUndoProp >> quickCheck splitUndoIdemProp >> quickCheck splitPreserveDelimsProp >> quickCheck dropShorterProp >> quickCheck dropDidDropProp >> quickCheck dropTailEquivProp >> quickCheck splitNeilProp


dropLast :: [a] -> [a]
dropLast [] = []
dropLast x = reverse . tail . reverse $ x

dropShorterProp :: [a] -> Property
dropShorterProp x = not (null x) ==> length x > (length $ dropLast x)
dropDidDropProp :: (Eq a) => [a] -> Property
dropDidDropProp x = not (null x) ==> dropLast x /= x
dropTailEquivProp :: (Eq a) => [a] -> Property
dropTailEquivProp x = not (null x) ==> tail x == dropLast x

replaceBy :: (a -> Bool) -> a -> [a] -> [a]
replaceBy a b = map (\x -> if a x then b else x)

replace :: (Eq a) => a -> a -> [a] -> [a]
replace a = replaceBy (==a)

replaceLengthProp :: (Eq a) => a -> a -> [a] -> Bool
replaceLengthProp x y z = (length $ replace x y z) == (length z)
replaceUndoableProp :: (Eq a) => a -> a -> [a] -> Bool
replaceUndoableProp x y z = if not (y `elem` z) then z == (replace y x $ replace x y z) else True
replaceIdempotentProp :: (Eq a) => a -> a -> [a] -> Bool
replaceIdempotentProp x y z = (replace x y $ replace x y z) == (replace x y z)

lines' :: String -> [String]
lines' s = removeTrailingNull (split' '\n' s)
 where
   removeTrailingNull :: [String] -> [String]
   removeTrailingNull y = case y of
                            [] -> []
                            [""] -> []
                            (x:xs) -> x : removeTrailingNull xs

linesProp :: String -> Bool
linesProp x = (Prelude.lines x == lines' x)

words' :: String -> [String]
words' = filter (not . all isSpace) . split isSpace

wordsProp :: String -> Bool
wordsProp x = (Prelude.words x == words' x)

split :: (a -> Bool) -> [a] -> [[a]]
split _ [] = []
split p s = let (l,s') = break p s in l : case s' of
                                           [] -> []
                                           (r:s'') -> [r] : split p s''

splitUndoProp, splitUndoIdemProp, splitPreserveDelimsProp :: (Eq a) => a -> [a] -> Bool
splitUndoProp x y = (concat $ split (==x) y) == y
splitUndoIdemProp x y = (concat $ concat $ split (==[x]) $ split (==x) y) == y
splitPreserveDelimsProp x y = (length $ elemIndices [x] $ split (==x) y) == (length $ elemIndices x y)

-- | A refined version of 'split'. This definition works like similar operators
-- in Java or Ruby or Perl, removing the delimiters from the resulting list.
-- Note that this split is not invertible.
split' :: (Eq a) => a -> [a] -> [[a]]
split' a b = filter (/= [a]) $ split (\x -> x==a) b


splitNeilProp x y = splitNeil x y == split' x y
 where splitNeil :: Eq a => a -> [a] -> [[a]]
       splitNeil x [] = []
       splitNeil x xs = if null b then [a] else a : splitNeil x (tail b)
           where (a,b) = break (== x) xs


splitTwanProp x y = not (null y) ==> (splitWithTwan (==x) y == split (==x) y)

-- <http://hackage.haskell.org/trac/ghc/ticket/2048>. see patch

-- | Break a list into pieces separated by the argument,
-- consuming the delimiter. I.e.
--
-- > split '\n' "a\nb\nd\ne" == ["a","b","d","e"]
-- > split 'a'  "aXaXaXa"    == ["","X","X","X",""]
-- > split 'x'  "x"          == ["",""]
splitTwan                   :: Eq a => a -> [a] -> [[a]]
splitTwan x                 =  splitWithTwan (x==)

-- | Splits a 'list into components delimited by separators,
-- where the predicate returns True for a separator element.
-- The resulting components do not contain the separators.
-- Two adjacent separators result in an empty component in the output.
splitWithTwan               :: (a -> Bool) -> [a] -> [[a]]
splitWithTwan p xs          =  ys : case zs of
                                  []   -> []
                                  _:ws -> splitWithTwan p ws
                           where (ys,zs) = break p xs

-- See <http://www.haskell.org/pipermail/haskell-cafe/2006-July/016574.html>
{- |
  A function inspired by the perl function split. A list is splitted
  on a seperator element in smaller non-empty lists.
  The seperator element is dropped from the resulting list.
-}
-- splitOn :: Eq a => a -- ^ seperator
--         -> [a] -- ^ list to split
--         -> [[a]]
-- splitOn x xs = let (l, r) = break (==x) xs in
--     (if null l then [] else [l]) ++ (if null r then [] else splitOn x $
-- tail r)


-- splitBy :: (a -> Bool) -- ^ whether char is a seperator
--         -> [a] -- ^ list to split
--         -> [[a]]
