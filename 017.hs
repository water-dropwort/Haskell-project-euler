module Problem017 where

solve :: Int -> Int
solve n = sum $ map (length . toWord) [1..n]

answer :: Int
answer = solve 1000

toWord :: Int -> String
toWord  0 = ""
toWord  1 = "one"
toWord  2 = "two"
toWord  3 = "three"
toWord  4 = "four"
toWord  5 = "five"
toWord  6 = "six"
toWord  7 = "seven"
toWord  8 = "eight"
toWord  9 = "nine"
toWord 10 = "ten"
toWord 11 = "eleven"
toWord 12 = "twelve"
toWord 13 = "thirteen"
toWord 14 = "fourteen"
toWord 15 = "fifteen"
toWord 16 = "sixteen"
toWord 17 = "seventeen"
toWord 18 = "eighteen"
toWord 19 = "nineteen"
toWord 20 = "twenty"
toWord 30 = "thirty"
toWord 40 = "forty"
toWord 50 = "fifty"
toWord 60 = "sixty"
toWord 70 = "seventy"
toWord 80 = "eighty"
toWord 90 = "ninety"
toWord x
  | x < 100 = let (d,m) = divMod x 10 in toWord (d*10) ++ toWord m
  | x < 1000 = let (d,m) = divMod x 100 in toWord d ++ "hundred" ++ if m /= 0 then "and" ++ toWord m else ""
  | x == 1000 = "onethousand"
  | otherwise = error "not support"
