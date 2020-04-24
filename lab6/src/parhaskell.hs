{-# OPTIONS_GHC -O2 -Weverything -Wno-implicit-prelude -Wno-missing-safe-haskell-mode -Wno-prepositive-qualified-module -Wno-unsafe #-}

import Control.Concurrent.Async (forConcurrently_, replicateConcurrently, replicateConcurrently_)
import Control.Concurrent.STM (atomically)
import Control.Concurrent.STM.TVar
import Control.Monad ((>=>), forM_, replicateM, replicateM_)
-- import Control.Concurrent
-- import Control.Monad.Par (get, runPar, spawnP)
import Control.Parallel.Strategies (parMap, rdeepseq)
import qualified Data.ByteString.Char8 as C
import qualified Data.IntMap.Strict as IntMap
import Data.List (foldl')
import Data.Maybe (fromJust)

-- Fermat's little theorem

expMod :: Int -> Int -> Int -> Int
expMod p = go
  where
    go :: Int -> Int -> Int
    go _ 0 = 1
    go b e
      | odd e = b * go b (e - 1) `mod` p
      | otherwise = go (b * b `mod` p) (e `div` 2)

prodMod :: Int -> Int -> Int -> Int
prodMod p n1 n2 = (n1 * n2) `mod` p

prodMod' :: Int -> [Int] -> Int
prodMod' p = foldl' (prodMod p) 1

factMod :: Int -> Int -> Int
factMod p n = prodMod' p [1 .. n]

invMod :: Int -> Int -> Int
invMod p n = expMod p n (p - 2)

chooseMod :: Int -> Int -> Int -> Int
chooseMod p n k = prodMod' p [n - k + 1 .. n] * invMod p (factMod p k) `mod` p

-- Lucas's theorem (and Fermat's little theorem)

digits :: Int -> Int -> [Int]
digits p = go
  where
    go 0 = []
    go n = let (d, m) = divMod n p in m : go d

chooseMod' :: Int -> Int -> Int -> Int
chooseMod' p n k = prodMod' p (zipWith (chooseMod p) (digits p n) (digits p k))

-- Main

-- main :: IO ()
-- main = do
--   line <- C.getLine
--   let t = fst $ fromJust $ C.readInt line
--   replicateM_ t $ do
--     line' <- C.getLine
--     let [n, k, p] = map (fst . fromJust . C.readInt) $ C.words line'
--     print $ chooseMod' p n k

main :: IO ()
main = do
  line <- C.getLine
  let t = fst $ fromJust $ C.readInt line
  tvar <- atomically $ newTVar IntMap.empty
  forConcurrently_ [1 .. t] $ \i -> do
    line' <- C.getLine
    let [n, k, p] = map (fst . fromJust . C.readInt) $ C.words line'
    let c = chooseMod' p n k
    atomically $ modifyTVar' tvar (IntMap.insert i c)
  m <- readTVarIO tvar
  mapM_ print $ IntMap.elems m

-- main :: IO ()
-- main = do
--   line <- C.getLine
--   let t = fst $ fromJust $ C.readInt line
--   lines <- replicateM t C.getLine
--   let lines' = map (map (fst . fromJust . C.readInt) . C.words) lines
--   print $ runPar $ do
--     ps <- mapM (\[n, k, p] -> spawnP $ chooseMod' p n k) lines'
--     mapM get ps

-- main :: IO ()
-- main = do
--   line <- C.getLine
--   let t = fst $ fromJust $ C.readInt line
--   lines' <- replicateConcurrently t C.getLine
--   let cs = parMap rdeepseq ((\[n, k, p] -> chooseMod' p n k) . (map (fst . fromJust . C.readInt) . C.words)) lines'
--   mapM_ print cs

-- https://downloads.haskell.org/~ghc/latest/docs/html/users_guide/using-concurrent.html#using-smp
-- https://downloads.haskell.org/~ghc/latest/docs/html/users_guide/parallel.html#lang-parallel
