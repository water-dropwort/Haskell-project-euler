module Lib.PrimeNumber ( isPrimeNum
                       , factorisation
                       , primeNums
                       , primeNums'
                       , eratosthenes) where

import GHC.Arr
import Control.Monad
import Control.Monad.ST

-- 素数判定
isPrimeNum :: (Integral a) => a -> Bool
isPrimeNum n = isPrimeNum' 2
  where
    sqrtN = floor $ sqrt $ fromIntegral n
    isPrimeNum' x = if x > sqrtN then
                      True
                    else
                      if n `mod` x == 0 then False else isPrimeNum' $ x + 1

-- 素因数分解
factorisation :: (Integral a) => a -> [a]
factorisation x
  | x <= 1    = []
  | otherwise = let p = head [v | v <- [2..], x `mod` v == 0]
                in  p : factorisation (x `div` p)

-- 素数のリスト
primeNums :: (Integral a) => [a]
primeNums = primeNums' $ 2:[3,5..]

primeNums' []     = []
primeNums' (x:xs) = x : primeNums' [y | y <- xs, y `mod` x /= 0]

-- エラトステネスの篩に基づいて素数列を生成する。(Project Euler #10で回答後に得られるPDFを参考)
eratosthenes :: (Integral a, Ix a) => a -> [a]
eratosthenes n = 2 : [2 * i + 1 | (i,flag) <- assocs sieve, not flag]
  where
    sieve = runST $ do
      let sievebound = (n - 1) `div` 2
      let crosslimit = (((floor.sqrt.fromIntegral) n) - 1) `div` 2
      sieveary <- newSTArray (1,sievebound) False
      forM_ [1..crosslimit] $ \i -> do
        flag <- readSTArray sieveary i
        when (not flag) $ forM_ [2*i*(i+1),2*i*(i+1)+(2*i+1) .. sievebound] $ \j -> writeSTArray sieveary j True
      freezeSTArray sieveary
