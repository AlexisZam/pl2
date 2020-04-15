{-# OPTIONS_GHC -Wall -fwarn-tabs -fwarn-incomplete-record-updates -fwarn-monomorphism-restriction -fwarn-unused-do-bind #-}
{-# OPTIONS_GHC -O2 #-}
{-# OPTIONS_GHC -threaded -rtsopts -with-rtsopts=-N #-}

main :: IO [()]
main = do
  line <- getLine
  let t = read line :: Int
  sequence $ take t $ repeat $ do
    line' <- getLine
    let [n, k, p] = map read $ words line' :: [Int]
    print $ chooseMod'' p n k

-- Pascal's triangle

chooseMod :: Int -> Int -> Int -> Int
chooseMod p = go
  where _ `go` 0 = 1
        0 `go` _ = 0
        n `go` k = (((n - 1) `go` (k - 1)) `mod` p + ((n - 1) `go` k) `mod` p) `mod` p

-- Fermat's little theorem

expMod :: Int -> Int -> Int -> Int
expMod p = go
  where _ `go` 0 = 1
        x `go` n = let t = x `go` (n `div` 2) ^ (2 :: Int) `mod` p
                   in if odd n then t * x `mod` p else t

prodMod :: Int -> [Int] -> Int
prodMod p = go
  where go [] = 1
        go (x : xs) = (x * go xs) `mod` p

factMod :: Int -> Int -> Int
factMod p n = prodMod p [1 .. n]

invMod :: Int -> Int -> Int
invMod p x = expMod p x (p - 2)

chooseMod' :: Int -> Int -> Int -> Int
chooseMod' p n k = let num = prodMod p [n - k + 1 .. n]
                       den = factMod p k
                   in num * invMod p den `mod` p

-- Lucas's theorem (and Fermat's little theorem)

digitize :: Int -> Int -> [Int]
digitize p = go
  where go 0 = []
        go n = (n `mod` p) : go (n `div` p)

chooseMod'' :: Int -> Int -> Int -> Int
chooseMod'' p n k = let ns = digitize p n
                        ks = digitize p k
                    in prodMod p (zipWith (chooseMod' p) ns ks)

-- chooseModPar :: Int -> Int -> Int -> Int
-- chooseModPar p n k = let ns = digitize p n
--                          ks = digitize p k
--                      in runPar $ do
--                        i <- new
--                        j <- new
--                        let (ns1, ns2) = splitAt (length ns `div` 2) ns
--                        let (ks1, ks2) = splitAt (length ks `div` 2) ks
--                        fork (put i (prodMod p (zipWith (chooseMod' p) ns1 ks1)))
--                        fork (put j (prodMod p (zipWith (chooseMod' p) ns2 ks2)))
--                        a <- get i
--                        b <- get j
--                        return (a * b `mod` p)
