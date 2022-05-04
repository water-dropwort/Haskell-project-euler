module Problem032 where

import Data.List
import Lib.List

answer :: Integer
answer = sum $ rmvDuplicate $ sort [res | num <- numbers, pos1 <- [1..7]
                                        , res <- pandigitalProducts num pos1]
numbers :: [Integer]
numbers = map read $ permutations "123456789"

splitNumbers :: Integer -> Integer -> Integer -> (Integer,Integer,Integer)
splitNumbers num pos1 pos2 = let (x,xs) = divMod num (10^(9-pos1))
                                 (y,z)  = divMod xs  (10^(9-pos1-pos2))
                             in  (x,y,z)

pandigitalProducts :: Integer -> Integer -> [Integer]
pandigitalProducts pan pos1 = pandigitalProducts' (pos1+1)
  where
    pandigitalProducts' pos2
      | pos1 + pos2 > 8 = []
      | otherwise          = let (x,y,z) = splitNumbers pan pos1 pos2
                                 res = if x < y && x*y==z then [z] else []
                             in  if x > y || y > z then
                                   res
                                 else
                                   res ++ pandigitalProducts' (pos2+1)
