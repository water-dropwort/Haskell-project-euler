module Lib.Fibonacci (fibonacciNums) where

-- フィボナッチ数列 (1,1,2,3,5,8...)
fibonacciNums :: (Integral a) => [a]
fibonacciNums = [1,1] ++ (addItem [1,1] (0,1))
  where
    addItem xs (im2,im1) = let x = (xs !! im2) + (xs !! im1)
                               xs' = xs ++ [x]
                           in  x : (addItem xs' (im1,im1+1))
