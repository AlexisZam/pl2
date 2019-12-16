-- {-# OPTIONS_GHC -static -rtsopts -O2 -optc-O2 -fprof-auto -fexcess-precision -optc-march=native #-}
{-# OPTIONS_GHC -Wall -fwarn-tabs -fwarn-incomplete-record-updates -fwarn-monomorphism-restriction -fwarn-unused-do-bind #-}
{-# OPTIONS_GHC -O2 #-}
{-# OPTIONS_GHC -threaded -rtsopts -with-rtsopts=-N #-}

import qualified Data.IntMap.Strict as IntMap -- HashMap
-- import Data.Array.IArray
-- import Data.Array.Unboxed
import Data.List (foldl1')

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

        aux k acc
            | k <= max' =
                let ks = map (k -) $ filter (<= k) streaks
                    sumMod xs = foldl1' (\x y -> (x + y) `mod` m) xs
                in aux (k + 1) $ IntMap.insert k (sumMod (map (acc IntMap.!) ks)) acc
            | otherwise = acc

        aux' k acc
            | k <= max' = aux' (k + 1) $ IntMap.adjust (\x -> ((acc IntMap.! (k - 1)) + (x * 2 `mod` m)) `mod` m) k acc
            | otherwise = acc

        acc' = aux' 1 $ aux 1 $ IntMap.singleton 0 1

        answer' [0, b] = acc' IntMap.! b
        answer' [a, b] = (acc' IntMap.! b - acc' IntMap.! (a - 1)) `mod` m
        answer' _ = undefined
        