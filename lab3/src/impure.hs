{-# OPTIONS_GHC -O2 -Weverything -Wno-implicit-prelude -Wno-all-missed-specialisations -Wno-missing-safe-haskell-mode -Wno-unsafe #-}

import Control.Monad (forM_)
import Data.Array.IO (IOUArray, newArray, readArray, writeArray)

main :: IO ()
main = do
  line <- getLine
  let m = (read $ last $ words line) :: Int
  contents <- getContents
  let queries = map (map read . words) (lines contents) :: [[Int]]

  let max' = maximum $ map last queries
  let streaks = takeWhile (<= max') $ map (subtract 1) $ iterate (* 2) 2

  a <- newArray_ (0, max') 0 :: IO (IOUArray Int Int)
  writeArray a 0 1
  forM_ [1 .. max'] (foo a m streaks)
  forM_ [1 .. max'] (foo' a m)
  forM_ queries (bar a m)

foo :: IOUArray Int Int -> Int -> [Int] -> Int -> IO ()
foo a m streaks i = do
  let streaks' = map (i -) $ filter (<= i) streaks
  forM_ streaks' $ \streak -> do
    e1 <- readArray a i
    e2 <- readArray a streak
    writeArray a i $ (e1 + e2) `mod` m

foo' :: IOUArray Int Int -> Int -> Int -> IO ()
foo' a m i = do
  e1 <- readArray a (i - 1)
  e2 <- readArray a i
  writeArray a i $ (e1 + (e2 * 2 `mod` m)) `mod` m

bar :: IOUArray Int Int -> Int -> [Int] -> IO ()
bar a _ [0, j] = readArray a j >>= print
bar a m [i, j] = do
  e1 <- readArray a (i - 1)
  e2 <- readArray a j
  print $ (e2 - e1) `mod` m
bar _ _ _ = undefined
