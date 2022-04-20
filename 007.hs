module Problem007 where

import Lib.PrimeNumber

solve :: Int -> Integer
solve n = primeNums !! (n - 1)

test :: Bool
test = (solve 6) == 13

answer :: Integer
answer = solve 10001
