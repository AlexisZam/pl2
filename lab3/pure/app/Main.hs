-- {-# OPTIONS_GHC -static -rtsopts -O2 -optc-O2 -fprof-auto -fexcess-precision -optc-march=native #-}
{-# OPTIONS_GHC -Wall -fwarn-tabs -fwarn-incomplete-record-updates -fwarn-monomorphism-restriction -fwarn-unused-do-bind #-}
{-# OPTIONS_GHC -O2 #-}
{-# OPTIONS_GHC -threaded -rtsopts -with-rtsopts=-N #-}
{-# LANGUAGE BangPatterns #-}

import qualified Data.IntMap.Strict as IntMap -- HashMap
import Data.Array.IArray
import Data.Array.Unboxed
import Data.List (foldl1', scanl1)

main :: IO ()
main = do
    line <- getLine
    let m = (read $ last $ words line) :: Int
    contents <- getContents
    let queries = (map (map read) $ map words $ lines contents) :: [[Int]]

    mapM_ print $ answer m queries

answer :: Int -> [[Int]] -> [Int]
answer m queries = map answer' queries
    where
        max' = maximum $ map last queries
        streaks = takeWhile (<= max') $ map (subtract 1) $ iterate (* 2) 2

        arr :: Array Int Int
        arr = array (0, max') ((0, 1) : [(i, foo i) | i <- [1..max']])

        foo i = sumMod $ map (arr !) streaks'
            where
                streaks' = map (i -) $ filter (<= i) streaks
                sumMod = foldl1' (\x y -> (x + y) `mod` m)

        arr' :: UArray Int Int
        arr' = listArray (0, max') $ 1 : (tail $ scanl1 (\x y -> ((x + (y * 2 `mod` m)) `mod` m)) (elems arr))

        answer' [0, b] = arr' ! b
        answer' [a, b] = (arr' ! b - arr' ! (a - 1)) `mod` m
        answer' _ = undefined
