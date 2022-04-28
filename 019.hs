module Problem019 where

type Year  = Int
type Month = Int
type DateDiff = Int

answer :: Int
answer = sum [1 | y <- [1901..2000], m <- [1..12], isSunday $ dateDiff y m]

isSunday :: DateDiff -> Bool
isSunday diff = (diff `mod` 7) == 6

-- 1900/1/1 から引数で与えたyyyy/MM/01との差を求める
dateDiff :: Year -> Month -> DateDiff
dateDiff year month = let yearDiff   = year - 1900
                          yearDiff'  = yearDiff * 365 + (yearDiff `div` 4)
                          monthDiff  = calcMonthDiff (month-1)
                      in  monthDiff + yearDiff'
  where calcMonthDiff x
          | x <= 0 = 0
          | x == 1 = 31
          | x == 2 = 28 + calcMonthDiff (x-1) + if year `mod` 4  == 0 && year `mod` 400 /= 0 then 1 else 0
          | x == 4 || x == 6 || x == 9 || x == 11 = 30 + calcMonthDiff (x-1)
          | otherwise = 31 + calcMonthDiff (x-1)
