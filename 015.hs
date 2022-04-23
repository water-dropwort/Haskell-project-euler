module Problem015 where

solve :: Integer -> Integer
solve n = binCoef (2*n) n

answer :: Integer
answer = solve 20

-- 二項係数
binCoef :: (Integral a) => a -> a -> a
binCoef n k
  | k == 0 || n == k = 1
  | otherwise        = n * (binCoef (n-1) (k-1)) `div` k
