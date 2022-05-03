module Problem025 where

import Lib.Fibonacci

digit :: Integer -> Int
digit = length . show

solve :: Int -> Int
solve digitNum = head [i | (i,x) <- zip [1..] fibonacciNums, digit x >= digitNum]

answer = solve 1000
