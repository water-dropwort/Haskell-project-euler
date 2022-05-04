module Problem030 where

import Data.Char

answer :: (Integral a, Show a) => a
answer = solve 5

solve :: (Integral a, Show a) => a -> a
solve x = sum [n | n <- [2..maxN x], n == (sumOfXPows n x)]

sumOfXPows :: (Integral a, Show a) => a -> a -> a
sumOfXPows n x = toEnum $ sum $ map (\c -> digitToInt c ^ x) $ show n

maxN :: (Integral a, Show a) => a -> a
maxN x = head [10^k-1 | k <- [1..], 10^k > sumOfXPows (10^(k+1)-1) x]
