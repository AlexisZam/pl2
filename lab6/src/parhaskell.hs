{-# OPTIONS_GHC -O2 -Weverything -Wno-implicit-prelude -Wno-incomplete-uni-patterns -Wno-unsafe #-}

import Control.DeepSeq (NFData)
import Control.Monad.Par (get, runPar, spawnP)
import Control.Monad.Par.Combinator (parMap)
import Control.Parallel.Strategies (parMap, parTuple2, rdeepseq, using)
import Data.List (foldl')
import Data.Maybe (fromMaybe)
import System.Environment (lookupEnv)

-- Fermat's little theorem

expMod :: Word -> Word -> Word -> Word
expMod p b e = go (b `mod` p) e `mod` p
  where
    go :: Word -> Word -> Word
    go _ 0 = 1
    go b' e'
      | odd e' = b' * go b' (e' - 1) `mod` p
      | otherwise = go (b' * b' `mod` p) (e' `div` 2)

invMod :: Word -> Word -> Word
invMod p n = expMod p n (p - 2)

-- utils

prodMod :: Word -> Word -> Word -> Word
prodMod p n1 n2 = (n1 * n2) `mod` p
{-# INLINE prodMod #-}

prodMod' :: Word -> Word -> Word -> Word
prodMod' p from to = foldl' (prodMod p) 1 [from .. to]

-- seq

chooseMod :: Word -> Word -> Word -> Word
chooseMod p n k = go (min k (n - k))
  where
    go k' = prodMod' p (n - k' + 1) n * invMod p (prodMod' p 1 k') `mod` p

-- monad-par

parTuple2' :: (NFData a, NFData b) => (a, b) -> (a, b)
parTuple2' (n1, n2) = runPar $ do
  n1' <- spawnP n1
  n2' <- spawnP n2
  n1'' <- get n1'
  n2'' <- get n2'
  return (n1'', n2'')

prodModMonadPar :: Word -> Word -> Word -> Word
prodModMonadPar p = go 8
  where
    go :: Int -> Word -> Word -> Word
    go 0 from to = prodMod' p from to
    go n from to =
      let half = (from + to) `div` 2
       in uncurry (*) (parTuple2' (go (n - 1) from (half - 1), go (n - 1) half to)) `mod` p

chooseModMonadPar :: Word -> Word -> Word -> Word
chooseModMonadPar p n k = go (min k (n - k))
  where
    go k' = uncurry (*) (parTuple2' (prodModMonadPar p (n - k' + 1) n, invMod p (prodModMonadPar p 1 k'))) `mod` p

mapMonadPar :: NFData b => (a -> b) -> [a] -> [b]
mapMonadPar f = runPar . Control.Monad.Par.Combinator.parMap f

-- parallel

prodModParallel :: Word -> Word -> Word -> Word
prodModParallel p = go 8
  where
    go :: Int -> Word -> Word -> Word
    go 0 from to = prodMod' p from to
    go n from to =
      let half = (from + to) `div` 2
       in uncurry (*) ((go (n - 1) from (half - 1), go (n - 1) half to) `using` parTuple2 rdeepseq rdeepseq) `mod` p

chooseModParallel :: Word -> Word -> Word -> Word
chooseModParallel p n k = go (min k (n - k))
  where
    go k' = uncurry (*) ((prodModParallel p (n - k' + 1) n, invMod p (prodModParallel p 1 k')) `using` parTuple2 rdeepseq rdeepseq) `mod` p

mapParallel :: NFData b => (a -> b) -> [a] -> [b]
mapParallel = Control.Parallel.Strategies.parMap rdeepseq

-- main

enviroment :: NFData b => IO (Word -> Word -> Word -> Word, (a -> b) -> [a] -> [b])
enviroment = do
  package <- lookupEnv "PACKAGE"
  return $ case fromMaybe "seq" package of
    "seq" -> (chooseMod, map)
    "parallel" -> (chooseModParallel, mapParallel)
    "monad-par" -> (chooseModMonadPar, mapMonadPar)
    _ -> undefined

main :: IO ()
main = do
  (chooseMod', map') <- enviroment
  line <- getLine
  let t = read line :: Int
  contents <- getContents
  mapM_ print $ map' ((\[n, k, p] -> chooseMod' p n k) . map read . words) $ take t $ lines contents
