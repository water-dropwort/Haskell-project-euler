module Problem003 where

import Lib.PrimeNumber

solve :: Integer -> Integer
solve n
  | n == 1    = n
  | otherwise =
      let p = head [x | x <- [2] ++ [3,5..n], n `mod` x == 0]
      in  if p == n then n else solve $ n `div` p

test :: Bool
test = (solve 13195) == 29

answer :: Integer
answer = solve 600851475143
