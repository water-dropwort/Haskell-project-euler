module Problem003 where

isPrimeNum :: Integer -> Bool
isPrimeNum n = isPrimeNum' 2
  where
    sqrtN = floor $ sqrt $ fromInteger n
    isPrimeNum' x = if x > sqrtN then
                      True
                    else
                      if n `mod` x == 0 then False else isPrimeNum' $ x+1
