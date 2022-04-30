module Problem023 where

import Lib.Divisor
import GHC.Arr
import Control.Monad
import Control.Monad.ST

answer :: Integer
answer = let lut = isAbundantLut
             abundants = [x|(x,t)<-assocs lut,t]
             isSumOfTwoAbundants n = any (\x -> lut!(n-x)) $ filter (< n) abundants
         in  sum $ filter (not.isSumOfTwoAbundants) [1..maximum abundants]

isAbundantLut :: Array Integer Bool
isAbundantLut = runST $ do
  let lo = 1
  let up = 28123
  flags <- newSTArray (lo,up) False
  forM_ [lo..up] $ \i -> do
    flag <- readSTArray flags i
    when (flag == False) $ do
      let nt = numType i
      when (nt == Abundant || nt == Perfect) $ 
        forM_ [(if nt == Abundant then 1 else 2)..(up`div`i)] $ \j -> writeSTArray flags (i*j) True
  freezeSTArray flags
