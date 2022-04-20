module Problem005 where

import Lib.PrimeNumber

solve :: Integer -> Integer
solve n = head [x | x <- [n,2*n..], all (\a -> x `mod` a == 0) [(n-1),(n-2)..1]]

test :: Bool
test = (solve 10) == 2520

answer :: Integer
answer = solve2 20

{-
(1) 1..N までの各整数を素因数分解する。
(2) 素数の個数の最大値を調べる。
(3) 素数 * 素数の最大個数 の総積を求める。
-}
solve2 :: Integer -> Integer
solve2 n = let fss = map factorisation [1..n]
               ps  = concat $ filter (\fs -> length fs == 1) fss
               count v xs = sum [1 | x <- xs, v == x]
               f a p = a * p ^ (maximum $ map (count p) fss)
           in  foldl f 1 ps
