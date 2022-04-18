module Problem006 where

solve :: Int -> Int
solve n = 2 * sum [x * y | x <- [1..n], y <- [(x+1)..n]]

test :: Bool
test = (solve 10) == 2640

answer :: Int
answer = solve 100
