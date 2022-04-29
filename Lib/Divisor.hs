module Lib.Divisor (proofDivisor
                   ,sumOfProofDivisor) where

-- 真の約数
proofDivisor :: Integral a => a -> [a]
proofDivisor n = [x | x <- [1..n`div`2], mod n x == 0]

-- 真の約数の和
sumOfProofDivisor :: Integral a => a -> a
sumOfProofDivisor = sum . proofDivisor
