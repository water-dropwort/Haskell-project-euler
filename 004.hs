module Problem004 where

solve :: Int -> Int
solve digit =
  let (minv,maxv) = digit2MaxMin digit
      set = [maxv,maxv-1..minv]
  in  maximum [a*b | a <- set, b <- set, isPalindromic (a*b)]

{-
solveよりも処理が高速。
以下のような順番で行と列の値の積をチェックしていく。---は無視する。
|     | 999 | 998 | 997 | 996 |
|-----+-----+-----+-----+-----|
| 999 |   1 | --- | --- | --- |
| 998 |   2 |   3 | --- | --- |
| 997 |   4 |   5 |   7 | --- |
| 996 |   6 |   8 |  10 |     |
| 995 |   9 |  11 |     |     |
-}
solve2 :: Int -> Int
solve2 digit = solve' (maxv,maxv) (maxv,maxv)
  where
    maxv = snd $ digit2MaxMin digit
    nextCenter (a,b) = if a == b then ((a-1),b) else (a,(b-1))
    solve' (a,b) center
      | isPalindromic (a * b) = a * b
      | b == maxv             = let center' = nextCenter center in solve' center' center'
      | otherwise             = solve' ((a - 1),(b + 1)) center

isPalindromic :: Int -> Bool
isPalindromic x = (show x) == (reverse $ show x)

digit2MaxMin :: Int -> (Int,Int)
digit2MaxMin digit = (10^(digit-1), sum $ flip map [0..(digit-1)] $ \x -> (10^x) * 9)

test :: Bool
test = (solve 2) == 9009

answer :: Int
answer = solve 3
