module Problem028 where

answer :: (Integral a) => a
answer = solve 1001

solve :: (Integral a) => a -> a
solve n
  | n < 1 || even n = error "invalid parameter"
  | otherwise       = let n2 = fromEnum $ (n-1) `div` 2 + 1
                          s1 = sum $ take n2 $ makeSeq 1 9
                          s2 = sum $ take n2 $ makeSeq 1 3
                          s3 = sum $ take n2 $ makeSeq 1 5
                          s4 = sum $ take n2 $ makeSeq 1 7
                      in  s1+s2+s3+s4-3

makeSeq :: (Integral a) => a -> a -> [a]
makeSeq x0 x1 = [x0,x1] ++ [x1+dX*i+8*(sum1toN i)| i <- [1..]]
  where dX = x1 - x0
        sum1toN n = (1+n) * n `div` 2
