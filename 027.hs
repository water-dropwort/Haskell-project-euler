module Problem027 where

import Lib.PrimeNumber
import GHC.Arr
import Control.Monad
import Control.Monad.ST

aMax = 999
bMax = 1000
nMax = aMax + bMax - 1
qMax = quadratic (-aMax) bMax nMax

answer :: Integer
answer = let b_range = eratosthenes bMax
             lens = [(a,b,len) | a <- [-aMax..aMax], b <- filter (>a) b_range, let len = length $ primes a b]
             max' x@(_,_,len1) y@(_,_,len2) = if len1 < len2 then y else x
         in  (\(a,b,_) -> a * b) $ foldl1 max' lens

quadratic a b n = n^2 + a*n + b

primes :: Integer -> Integer -> [Integer]
primes a b = takeWhile isPrime' $ map (quadratic a b) [0..(b-a-1)]

isPrimeLut :: Array Integer Bool
isPrimeLut = runST $ do
  let ps = eratosthenes qMax
  lut <- newSTArray (1, qMax) False
  forM_ ps $ \p -> writeSTArray lut p True
  freezeSTArray lut

isPrime' :: Integer -> Bool
isPrime' x = if x <= 1 || qMax < x then False else isPrimeLut ! x
