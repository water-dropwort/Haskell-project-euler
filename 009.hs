module Problem009 where

solve :: Int -> Int
solve total = head [a*b*c | a <- [1..total], b <- [(a+1)..total], let c = total-a-b, b < c && a^2 + b^2 == c^2]

answer :: Int
answer = solve 1000
