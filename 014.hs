module Problem014 where

import GHC.Arr
import Control.Monad
import Control.Monad.ST

solve :: Int -> Int
solve max_n = let (_,maxidx,_) = foldlElems' f (0,0,0) $ collatzSeqLens max_n in maxidx
  where
    f :: (Int,Int,Int) -> Int -> (Int,Int,Int)
    f (idx,maxidx,maxLen) len = if maxLen < len then (idx+1,idx+1,len) else (idx+1,maxidx,maxLen)

answer :: Int
answer = solve (10^6 - 1)

collatz :: (Integral a) => a -> a
collatz x = if even x then x `div` 2 else 3 * x + 1

collatzSeqLens :: Int -> Array Int Int
collatzSeqLens max_n = runST $ do
  seqLenMemo <- newSTArray (1,max_n) 0
  forM_ [1..(floor $ logBase 2 $ fromIntegral max_n)] $ \i -> writeSTArray seqLenMemo (2^i) (i+1)
  forM_ [1..max_n] $ \n -> do
    slTpls <- collatzSeqLens' seqLenMemo n []
    forM_ slTpls $ \(i,seqLen_i) -> when (i <= max_n) $ writeSTArray seqLenMemo i seqLen_i
  freezeSTArray seqLenMemo

collatzSeqLens' :: STArray s Int Int -> Int -> [(Int,Int)] -> ST s [(Int,Int)]
collatzSeqLens' seqLenMemo n seqLenTuples = do
  len <- if n <= (snd $ boundsSTArray seqLenMemo) then readSTArray seqLenMemo n else return 0
  if len > 0 then
    return [(a,b + len) | (a,b) <- seqLenTuples]
  else do
    let seqLenTuples' = (n,1) : [(a,b + 1)|(a,b) <- seqLenTuples]
    if n == 1 then return seqLenTuples' else collatzSeqLens' seqLenMemo (collatz n) seqLenTuples'

solve2 :: Int -> Int
solve2 1 = 1
solve2 max_n = runST $ do
  memo <- newSTArray (1,max_n) 0
  writeSTArray memo 1 1
  let max' (i1,v1) (i2,v2) = if v1 < v2 then (i2,v2) else (i1,v1)
  let f (i,v) n = (pure $ max' (i,v)) <*> (countChain' memo n)
  (pure fst) <*> foldM f (1,1) [(max_n `div` 2)..max_n]

collatz2 :: (Integral a) => a -> a
collatz2 x = if even x then x `div` 2 else (3*x+1) `div` 2

countChain :: STArray s Int Int -> Int -> ST s Int
countChain memo n
  | inRange (boundsSTArray memo) n = do
      v <- readSTArray memo n
      if v > 0 then
        return v
      else do
        cnt <- pure succ' <*> countChain memo (collatz2 n)
        writeSTArray memo n cnt
        return cnt
  | otherwise                      = pure succ' <*> countChain memo (collatz2 n)
  where succ' = (if even n then succ else succ.succ)

countChain' :: STArray s Int Int -> Int -> ST s (Int,Int)
countChain' memo n = (pure $ \cnt -> (n,cnt)) <*> countChain memo n
