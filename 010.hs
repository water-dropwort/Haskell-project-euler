module Problem010 where

import Lib.PrimeNumber

solve :: Int -> Int
solve n_max = sum $ primeNums' $ 2:[3,5..n_max]

solve2 :: Int -> Int
solve2 = sum . eratosthenes

test :: Bool
test = (solve2 10) == 17

answer :: Int
answer = solve2 2000000
