{-# OPTIONS_GHC -Wall -fwarn-tabs -fwarn-incomplete-record-updates -fwarn-monomorphism-restriction -fwarn-unused-do-bind #-}
{-# OPTIONS_GHC -O2 #-}
{-# OPTIONS_GHC -threaded -rtsopts -with-rtsopts=-N #-}

import Control.Monad.Par

-- main :: IO [()]
main = do
  line <- getLine
  let t = read line :: Int
  sequence $ take t $ repeat $ do
    line' <- getLine
    let [n, k, p] = map read $ words line' :: [Int]
    -- return (n, k, p)
    print $ choosePar p n k
  -- print $ runPar $  p n k

-- Pascal's triangle

choose :: Int -> Int -> Int -> Int
choose p = go
  where _ `go` 0 = 1
        0 `go` _ = 0
        n `go` k = (((n - 1) `go` (k - 1)) `mod` p + ((n - 1) `go` k) `mod` p) `mod` p

-- Fermat's little theorem

modExp :: Int -> Int -> Int -> Int
modExp p = go
  where _ `go` 0 = 1
        x `go` n = let t = x `go` (n `div` 2) ^ (2 :: Int) `mod` p
                   in if odd n then t * x `mod` p else t

modProd :: Int -> [Int] -> Int
modProd p = go
  where go [] = 1
        go (x : xs) = (x * go xs) `mod` p

modFact :: Int -> Int -> Int
modFact p n = modProd p [1 .. n]

modInv :: Int -> Int -> Int
modInv p x = modExp p x (p - 2)

choose' :: Int -> Int -> Int -> Int
choose' p n k = let num = modProd p [n - k + 1 .. n]
                    den = modFact p k
                in num * modInv p den `mod` p

-- Lucas's theorem (and Fermat's little theorem)

digitize :: Int -> Int -> [Int]
digitize p = go
  where go 0 = []
        go n = (n `mod` p) : go (n `div` p)

choose'' :: Int -> Int -> Int -> Int
choose'' p n k = let ns = digitize p n
                     ks = digitize p k
                 in modProd p (zipWith (choose' p) ns ks)

choosePar :: Int -> Int -> Int -> Int
choosePar p n k = let ns = digitize p n
                      ks = digitize p k
                  in runPar $ do
                    i <- new
                    j <- new
                    let (ns1, ns2) = splitAt (length ns `div` 2) ns
                    let (ks1, ks2) = splitAt (length ks `div` 2) ks
                    fork (put i (modProd p (zipWith (choose' p) ns1 ks1)))
                    fork (put j (modProd p (zipWith (choose' p) ns2 ks2)))
                    a <- get i
                    b <- get j
                    return (a * b `mod` p)
