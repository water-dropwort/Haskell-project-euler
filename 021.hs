module Problem021 where

import Data.Maybe
import Data.STRef
import GHC.Arr
import Control.Monad
import Control.Monad.ST
import Lib.Divisor

solve :: Int -> Int
solve n_max = foldl addAmicableNum 0 [1..n_max]
  where
    addAmicableNum total n = let b = sumOfProperDivisor n
                             in  if n < b && n == sumOfProperDivisor b then total + n + b else total

answer :: Int
answer = solve 9999

-- 別パターン
solve2 :: Int -> Int
solve2 n_max = runST $ do
  memo <- newSTArray (1, n_max) Nothing
  srTotal <- newSTRef 0
  let getMemo i x = case x of
                      Just x' -> return x'
                      Nothing -> do let b = sumOfProperDivisor i
                                    writeSTArray memo i (Just b)
                                    return b
  forM_ [1..n_max] $ \a -> do
    b <- (readSTArray memo a) >>= (getMemo a)
    when (a < b && b <= n_max) $ do
      a' <- (readSTArray memo b) >>= (getMemo b)
      when (a' == a) $ do
        modifySTRef srTotal (\total -> total + a + b)
  readSTRef srTotal
