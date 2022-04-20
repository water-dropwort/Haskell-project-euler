module Problem001 where

solve :: Int -> Int
solve upper = sum [x | x <- [1..(upper-1)], x `mod` 3 == 0 || x `mod` 5 == 0]

test :: Bool
test = (solve 10) == 23

answer :: Int
answer = solve 1000
