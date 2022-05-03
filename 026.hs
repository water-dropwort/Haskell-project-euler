module Problem026 where

answer :: Integer
answer = solve 999

solve :: Integer -> Integer
solve n = fst $ foldl max' (0,0) [1..n]
  where
    max' (i,len) j = let j_len = recurringCycleLen j
                     in  if len < j_len then (j,j_len) else (i,len)

recurringCycleLen :: Integer -> Integer
recurringCycleLen x
  | x       == 1 = 1
  | mod x 2 == 0 = recurringCycleLen (x`div`2)
  | mod x 5 == 0 = recurringCycleLen (x`div`5)
  | otherwise    = head [l | l <- [1..], 10^l `mod` x == 1]
