module Lib.Divisor where

-- 真の約数
properDivisor :: Integral a => a -> [a]
properDivisor n = [x | x <- [1..n`div`2], mod n x == 0]

-- 真の約数の和
sumOfProperDivisor :: Integral a => a -> a
sumOfProperDivisor 1 = 0
sumOfProperDivisor n= let r = (floor . sqrt . fromIntegral) n
                          (r',totalIni) = if r*r==n then (r-1,1+r) else (r,1)
                          (fIni,step) = if odd n then (3,2) else (2,1)
                          process total f = if mod n f == 0 then total+f+(n`div`f) else total
                      in  foldl process totalIni [fIni,fIni+step..r']

data NumType = Abundant | Perfect | Deficient deriving (Show,Eq,Ord)

numType :: (Integral a) => a -> NumType
numType x = let y = sumOfProperDivisor x
                check
                  | x < y  = Abundant
                  | x == y = Perfect
                  | x > y  = Deficient
            in  check
