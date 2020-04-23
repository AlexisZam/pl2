{-# LANGUAGE BangPatterns #-}
-- {-# OPTIONS_GHC -O2 -Weverything -Wno-implicit-prelude -Wno-all-missed-specialisations -Wno-missing-safe-haskell-mode -Wno-unsafe #-}
{-# OPTIONS_GHC -O2 #-}

import Control.Monad (forM_, replicateM_)
import Control.Monad.ST (ST)
import Data.Array.ST (STUArray, newArray_, readArray, runSTUArray, writeArray)
import Data.Array.Unboxed ((!), UArray)
import qualified Data.ByteString.Char8 as B
import Data.Maybe (fromJust)

main :: IO ()
main = do
  line <- B.getLine
  let [n, m] = (map (fst . fromJust . B.readInt) . B.words) line

  let a = solve m

  replicateM_ n $ do
    line <- B.getLine
    let q = (map (fst . fromJust . B.readInt) . B.words) line
    print $ answer a m q

solve :: Int -> UArray Int Int
solve m = runSTUArray $ do
  let maxB = 1000000
  let streaks = takeWhile (<= maxB) $ map (subtract 1) $ iterate (* 2) 2
  a <- newArray_ (0, maxB) :: ST s (STUArray s Int Int)

  writeArray a 0 1
  forM_ [1 .. maxB] $ \i ->
    forM_ (filter (<= i) streaks) $ \streak -> do
      current <- readArray a i
      streak' <- readArray a (i - streak)
      writeArray a i ((current + streak') `mod` m)

  forM_ [1 .. maxB] $ \i -> do
    previous <- readArray a (i - 1)
    current <- readArray a i
    writeArray a i ((previous + current * 2 `mod` m) `mod` m)
  return a

answer :: UArray Int Int -> Int -> [Int] -> Int
answer a _ [0, j] = a ! j
answer a m [i, j] = (a ! j - a ! (i - 1)) `mod` m
answer _ _ _ = undefined
