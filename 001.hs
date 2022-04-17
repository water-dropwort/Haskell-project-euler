module Problem001 where

solve :: Int -> Int
solve upper = foldr f 0 [1..(upper-1)]
  where
    isMultiple3Or5 x = isDivisible x 3 || isDivisible x 5
    f x total = if isMultiple3Or5 x then x + total else total

isDivisible :: Int -> Int -> Bool
isDivisible x y = (x `mod` y) == 0

test :: Bool
test = (solve 10) == 23

answer :: Int
answer = solve 1000
