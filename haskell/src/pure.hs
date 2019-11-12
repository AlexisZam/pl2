{-# LANGUAGE Safe #-}
{-# OPTIONS_GHC -O2 -Weverything -Wno-implicit-prelude -Wno-incomplete-uni-patterns -Wno-prepositive-qualified-module #-}

import Control.Monad (replicateM_)
import Data.Array.IArray ((!), Array, elems, listArray)
import Data.Array.Unboxed (UArray)
import qualified Data.ByteString.Char8 as C
import Data.List (foldl1')
import Data.Maybe (fromJust)

max' :: Int
max' = 1000000

streaks :: [Int]
streaks = takeWhile (<= max') $ map (subtract 1) $ iterate (* 2) 2

sumMod :: Int -> Int -> Int -> Int
sumMod m x y = (x + y) `mod` m

solve :: Int -> UArray Int Int
solve m = listArray (0, max') $ scanl1 (sumMod m) $ elems array
  where
    array = listArray (0, max') (1 : [go i | i <- [1 .. max']]) :: Array Int Int
    go i = foldl1' (sumMod m) $ map ((array !) . (i -)) $ takeWhile (<= i) streaks

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
