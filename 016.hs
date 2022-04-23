module Problem016 where
import Data.Char (digitToInt)

solve :: Integer -> Int
solve n = sum $ map digitToInt $ show $ 2^n

answer :: Int
answer = solve 1000
