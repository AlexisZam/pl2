-- {-# OPTIONS_GHC -static -rtsopts -O2 -optc-O2 -fprof-auto -fexcess-precision -optc-march=native #-}
{-# OPTIONS_GHC -Wall -fwarn-tabs -fwarn-incomplete-record-updates -fwarn-monomorphism-restriction -fwarn-unused-do-bind #-}
{-# OPTIONS_GHC -O2 #-}
{-# OPTIONS_GHC -threaded -rtsopts -with-rtsopts=-N #-}

import qualified Data.IntMap.Strict as IntMap -- HashMap
-- import Data.Array.IArray
-- import Data.Array.Unboxed

main :: IO ()
main = do
    -- parse input
    line <- getLine
    let m = (read $ last $ words line) :: Int
    contents <- getContents
    let queries = (map (map read) $ map words $ lines contents) :: [[Int]]

    -- answer queries
    mapM_ print $ answer m queries

-- probably running sum should be lazy
answer :: Int -> [[Int]] -> [Int]
answer m queries = map answer' queries
    where
        max' = maximum $ map last queries
        streaks = takeWhile (<= max') $ map (subtract 1) $ iterate (* 2) 2

        aux k acc
            | k <= max' =
                let ks = map (k -) $ filter (<= k) streaks
                in aux (k + 1) $ IntMap.insert k (sum (map (acc IntMap.!) ks) `mod` m) acc
            | otherwise = acc

        runningKv = IntMap.foldlWithKey' (\kv' k v -> IntMap.insert k ((v + if k == 0 then 0 else kv' IntMap.! (k -1)) `mod` m) kv) IntMap.empty kv

        kv = IntMap.insert 0 1 $ IntMap.map (\a -> a * 2 `mod` m) $ aux 1 $ IntMap.singleton 0 1
        
        -- runningKv = snd $ IntMap.mapAccum (\x y -> ((x + y) `mod` m, (x + y) `mod` m)) 0 kv
        
        -- runningKv = IntMap.fromAscList $ scanl1 (\(_, v1) (k2, v2) -> (k2, (v1 + v2) `mod` m)) $ IntMap.toAscList kv
        answer' [0, b] = runningKv IntMap.! b
        answer' [a, b] = (runningKv IntMap.! b - runningKv IntMap.! (a - 1)) `mod` m
        answer' _ = undefined
        
        -- arr = listArray (0, max') $ scanl1 (\x y -> (x + y) `mod` m) $ IntMap.elems kv :: UArray Int Int
        -- answer' [0, b] = arr ! b
        -- answer' [a, b] = (arr ! b - arr ! (a - 1)) `mod` m
        -- answer' _ = undefined
