{-# OPTIONS_GHC -O2 -Weverything -Wno-implicit-prelude -Wno-incomplete-uni-patterns -Wno-unsafe -Wno-unused-top-binds #-}

import Control.DeepSeq (NFData)
import Control.Monad.Par (runPar)
import Control.Monad.Par.Combinator (parMap)
import Control.Parallel.Strategies (parMap, rdeepseq)
import qualified Data.ByteString.Char8 as C
import Data.List (foldl')
import Data.Maybe (fromJust, fromMaybe)
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

prodMod :: Word -> Word -> Word -> Word
prodMod p n1 n2 = (n1 * n2) `mod` p
{-# INLINE prodMod #-}

prodMod' :: Word -> [Word] -> Word
prodMod' p = foldl' (prodMod p) 1

factMod :: Word -> Word -> Word
factMod p n = prodMod' p [1 .. n]

invMod :: Word -> Word -> Word
invMod p n = expMod p n (p - 2)

chooseMod :: Word -> Word -> Word -> Word
chooseMod p n k = go (min k (n - k))
  where
    go k' = prodMod' p [n - k' + 1 .. n] * invMod p (factMod p k') `mod` p

-- Lucas's theorem (and Fermat's little theorem)

digits :: Word -> Word -> [Word]
digits p = go
  where
    go 0 = []
    go n = let (d, m) = divMod n p in m : go d

chooseMod' :: Word -> Word -> Word -> Word
chooseMod' p n k = go (min k (n - k))
  where
    go k' = prodMod' p (zipWith (chooseMod p) (digits p n) (digits p k'))

-- parallel

mapParallel :: NFData b => (a -> b) -> [a] -> [b]
mapParallel = Control.Parallel.Strategies.parMap rdeepseq

-- monad-par

mapMonadPar :: NFData b => (a -> b) -> [a] -> [b]
mapMonadPar f = runPar . Control.Monad.Par.Combinator.parMap f

main :: IO ()
main = do
  package <- lookupEnv "PACKAGE"
  let map' = case fromMaybe "monad-par" package of
        "parallel" -> mapParallel
        "monad-par" -> mapMonadPar
  print package
  line <- C.getLine
  let t = fst $ fromJust $ C.readInt line
  contents <- C.getContents
  mapM_ print $ map' ((\[n, k, p] -> chooseMod' p n k) . (map (fromIntegral . fst . fromJust . C.readInt) . C.words)) $ take t $ C.lines contents
