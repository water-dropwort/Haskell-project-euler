module Problem022 where

import System.IO
import Data.Char
import Data.List


answer :: IO Int
answer = solve "p022_names.txt"

solve :: FilePath -> IO Int
solve path = do
  names <- readFile path
  return $ sumOfNameScore names

sumOfNameScore :: String -> Int
sumOfNameScore names = let nameList = sort $ split ',' names
                       in  foldl (\total (i,name) -> total + i * nameToScore name) 0 $ zip [1..] nameList

split :: Char -> String -> [String]
split delim str = split' delim str []
  where
    split' _ [] ss = ss
    split' d s  ss = case span (/= d) s of
                       (sL,[])      -> ss ++ [sL]
                       (sL,_:sR')   -> split' d sR' $ ss ++ [sL]

alphaToInt :: Char -> Int
alphaToInt c
  | 'A' <= c && c <= 'Z' = (ord c) - (ord 'A') + 1
  | otherwise            = 0

nameToScore :: String -> Int
nameToScore = sum . (map alphaToInt)
