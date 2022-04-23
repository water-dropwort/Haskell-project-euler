module Problem012 where

import Lib.PrimeNumber

solve :: Int -> Int
solve n = solve' 1 1
  where
    solve' i x
      | divisorCount x > n = x
      | otherwise           = solve' (i+1) (x+i+1)

test :: Bool
test = (solve 5) == 28

answer :: Int
answer = solve 500

divisorCount :: Int -> Int
divisorCount x = product $ map (\n -> 1 + snd n) $ factorisation' x
