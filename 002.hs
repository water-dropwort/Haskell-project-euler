module Problem002 where

import Lib.Fibonacci

solve :: Int -> Int
solve x_max = solve' (1,1) 0
  where
    -- タプルはフィボナッチ数列の2項を格納するキューをイメージ。
    solve' (x_im2,x_im1) total =
      let x_i = x_im1 + x_im2
      in  if x_i <= x_max then
            if even x_i then
              solve' (x_im1,x_i) (total + x_i)
            else
              solve' (x_im1,x_i) total
          else
            total

solve2 :: Int -> Int
solve2 x_max = sum [x | x <- takeWhile (\a -> a <= x_max) fibonacciNums, mod x 2 == 0]

test :: Bool
test = (solve 89) == 44

answer :: Int
answer = solve 4000000
