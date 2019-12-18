-- {-# OPTIONS_GHC -static -rtsopts -O2 -optc-O2 -fprof-auto -fexcess-precision -optc-march=native #-}
{-# OPTIONS_GHC -Wall -fwarn-tabs -fwarn-incomplete-record-updates -fwarn-monomorphism-restriction -fwarn-unused-do-bind #-}
{-# OPTIONS_GHC -O2 #-}
{-# OPTIONS_GHC -threaded -rtsopts -with-rtsopts=-N #-}
{-# LANGUAGE BangPatterns #-}

import Control.Monad
import Data.Array.Base
import Data.Array.IO

main :: IO ()
main = do
    line <- getLine
    let m = (read $ last $ words line) :: Int
    contents <- getContents
    let queries = (map (map read) $ map words $ lines contents) :: [[Int]]

    let max' = maximum $ map last queries
    let streaks = takeWhile (<= max') $ map (subtract 1) $ iterate (* 2) 2
    a <- newArray (0, max') 0 :: IO (IOUArray Int Int)
    unsafeWrite a 0 1
    forM_ [1..max'] $ \i -> do
        let streaks' = map (i -) $ filter (<= i) streaks
        forM_ streaks' $ \streak -> do
            e1 <- unsafeRead a i
            e2 <- unsafeRead a streak
            unsafeWrite a i $ (e1 + e2) `mod` m

    forM_ [1..max'] $ \i -> do
        e1 <- unsafeRead a (i - 1)
        e2 <- unsafeRead a i
        unsafeWrite a i $ (e1 + (e2 * 2 `mod` m)) `mod` m

    forM_ queries $ \[i, j] -> case i of
        0 -> unsafeRead a j >>= print
        _ -> do
            e1 <- unsafeRead a (i - 1)
            e2 <- unsafeRead a j
            print $ (e2 - e1) `mod` m
