module Problem003 where

isPrimeNum :: Integer -> Bool
isPrimeNum n = isPrimeNum' 2
  where
    sqrtN = floor $ sqrt $ fromInteger n
    isPrimeNum' x = if x > sqrtN then
                      True
                    else
                      if n `mod` x == 0 then False else isPrimeNum' $ x + 1

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
