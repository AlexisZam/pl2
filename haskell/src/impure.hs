{-# OPTIONS_GHC -O2 -Weverything -Wno-all-missed-specialisations -Wno-implicit-prelude -Wno-incomplete-uni-patterns -Wno-missing-safe-haskell-mode -Wno-prepositive-qualified-module -Wno-unsafe #-}

import Control.Monad (forM_, replicateM_)
import Control.Monad.ST (ST)
import Data.Array.ST (STUArray, newArray, readArray, runSTUArray, writeArray)
import Data.Array.Unboxed ((!), UArray)
import qualified Data.ByteString.Char8 as C
import Data.Maybe (fromJust)

max' :: Int
max' = 1000000

streaks :: [Int]
streaks = takeWhile (<= max') $ map (subtract 1) $ iterate (* 2) 2

solve :: Int -> UArray Int Int
solve m = runSTUArray $ do
  a <- newArray (0, max') 0 :: ST s (STUArray s Int Int)
  writeArray a 0 1
  forM_ [1 .. max'] $ \i ->
    forM_ (takeWhile (<= i) streaks) $ \streak -> do
      previous <- readArray a (i - streak)
      current <- readArray a i
      writeArray a i ((previous + current) `mod` m)
  forM_ [1 .. max'] $ \i -> do
    previous <- readArray a (i - 1)
    current <- readArray a i
    writeArray a i ((previous + current) `mod` m)
  return a

answer :: UArray Int Int -> Int -> Int -> Int -> Int
answer a m i j = case i of
  0 -> (2 * a ! j - 1) `mod` m
  _ -> 2 * (a ! j - a ! (i - 1)) `mod` m

main :: IO ()
main = do
  line <- C.getLine
  let [n, m] = map (fst . fromJust . C.readInt) $ C.words line
  let a = solve m
  replicateM_ n $ do
    line' <- C.getLine
    let [i, j] = map (fst . fromJust . C.readInt) $ C.words line'
    print $ answer a m i j
