module Problem029 where

import Data.List
import Lib.List

solve :: (Integral a) => a -> Int
solve n = length $ rmvDuplicate $ sort $ [x^y | x <- [2..n], y <- [2..n]]

answer :: Int
answer = solve (100::Integer)
