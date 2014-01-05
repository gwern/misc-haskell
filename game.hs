{-# LANGUAGE GADTs, RankNTypes #-}
--
data Term x where
    K :: Term (a -> b -> a)
    S' :: Term ((a -> b -> c)  -> (a -> b) -> a -> c)
    Const :: a -> Term a
    (:@) :: Term (a -> b) -> (Term a) -> Term b
infixl 6 :@

eval'::Term a -> Term a
eval' (K :@ x :@ y) = x
eval' (S' :@ x :@ y :@ z) = x :@ z :@ (y :@ z)
eval' x = x

--

data Term' x where
     E :: (Integral a => a -> Bool) -> Term' (Integral a => a -> Bool)
     N :: Int -> Term' Int
     Q :: Int -> (Integral a => a -> Bool) -> Term' ((Integral a => a -> Bool), Int)

eval :: Term' x -> x
eval (N x) = x
--eval (E x) = x
-- eval (Q (x,y)) = eval x $ eval y

foo :: Bool
foo = eval (Q odd 1)

-- data S x where
--     Ns :: S [Int]
--     Eval :: S (Int -> Bool)
--     Map :: S ([Int] -> S a -> Bool)
--     Even :: S (Int -> Bool)
--     IsPrime :: S (Int -> Bool)
--     Expr :: S (a, b)

-- eval :: S x -> S x
-- eval (Expr (Even, Ns) = map isEven (
-- eval x = x

--
-- data Singleton a where
--     Even :: Singleton (Int -> Bool)
--     Odd :: Singleton (Int -> Bool)
--     Prime :: Singleton (Int -> Bool)
-- --    Const' :: [Int] -> Singleton [Int]

-- eval'

-- data Span a where
--  Map :: Span (Singleton a -> [Int] -> Bool)


-- data Boolean a where
--    Not :: Boolean (Bool -> Bool)
--    Xor :: Boolean (Bool -> Bool -> Bool)
-- 
--  -- BTW, type signatures are mandatory if you're using GADTs
-- translate :: Singleton a -> a
-- translate a = case a of 
--                Even -> not . odd
--                Odd -> odd
--                Prime -> isPrime


-- functions :: [Function a] -> [a]
-- functions = map translate
-- 
-- eval :: [a] -> [Function (a -> Bool)] -> Bool
-- eval nums exprs = and $ zipWith (\x y -> y x) nums $ functions exprs

isPrime :: Int -> Bool
isPrime n = aux primes
    where
      aux ps | head ps < n = aux $ tail ps
             | head ps == n = True
             | otherwise = False
      primes :: [Int]
      primes = sieve [2..]
      sieve (p:xs) = p : sieve [x | x <- xs, x `mod` p /= 0]
      sieve [] = undefined

{- -- apparently need GADTs for this... http://haskell.org/haskellwiki/GADT
data Function = Fold |
                Prime | Composite |
                Subtract | Add | Multiply | Divide | Exponent |
                GreaterThan | LessThan 

foo xs y = zipWith ($) (map translateUnadic' xs) y
 
-- translate :: Function ->[Int] -> Bool
translateUnadic Not  = not
translateUnadic' Even = not . odd
translateUnadic' Odd = odd

translateUnadic'' Prime = isPrime
translateUnadic'' Composite = not . isPrime

translate And  = (&&)
translate Or  = (||)
-- translate Xor  = xor

translate' Fold = \x y -> foldr x 0 y

translate'' Subtract = (-)
translate'' Add = (+)
-- translate'' = 

xor :: Bool -> Bool -> Bool
xor x y = (x &&not y) || (y ||not x)

-}