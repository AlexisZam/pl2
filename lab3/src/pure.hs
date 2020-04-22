{-# LANGUAGE Safe #-}
{-# OPTIONS_GHC -O2 -Weverything -Wno-implicit-prelude -Wno-all-missed-specialisations #-}

import Data.Array.IArray ((!), Array, listArray)
import Data.List (foldl1')

main :: IO ()
main = do
  line <- getLine
  let m = (read $ last $ words line) :: Int
  contents <- getContents
  let queries = map (map read . words) (lines contents) :: [[Int]]

  mapM_ print $ answer m queries

answer :: Int -> [[Int]] -> [Int]
answer m queries = map answer' queries
  where
    max' = maximum $! map last queries
    streaks = takeWhile (<= max') $! map (subtract 1) $! iterate (* 2) 2
    arr :: Array Int Int
    arr = listArray (0, max') $ 1 : [aux i | i <- [1 .. max']]
    aux i = foldl1' (\x y -> (x + y) `mod` m) $ map ((arr !) . (i -)) $ filter (<= i) streaks
    -- arr' :: UArray Int Int
    -- arr' = listArray (0, max') $ 1 : (tail $ scanl1 (\x y -> (x + (y * 2 `mod` m)) `mod` m) $ elems arr)

    arr' :: Array Int Int
    arr' = listArray (0, max') $ 1 : [aux' i | i <- [1 .. max']]
    aux' i = (arr' ! (i - 1) + (aux i * 2 `mod` m)) `mod` m
    answer' [0, b] = arr' ! b
    answer' [a, b] = (arr' ! b - arr' ! (a - 1)) `mod` m
    answer' _ = undefined
