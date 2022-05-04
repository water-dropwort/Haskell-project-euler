module Lib.List where

-- 引数のリストは並べ替え済みとする
rmvDuplicate :: (Eq a) => [a] -> [a]
rmvDuplicate [] = []
rmvDuplicate (x:xs) = x : (rmvDuplicate $ dropWhile (==x) xs)
