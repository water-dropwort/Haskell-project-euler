module Problem020 where
import Data.Char (digitToInt)

solve :: Integer -> Int
solve n = sum $ map digitToInt $ show $ factorial n

answer :: Int
answer = solve 100

factorial :: Integer -> Integer
factorial 1 = 1
factorial x = x * factorial (x-1)
