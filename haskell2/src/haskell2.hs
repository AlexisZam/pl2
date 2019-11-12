{-# LANGUAGE Safe #-}
{-# OPTIONS_GHC -O2 -Weverything -Wno-all-missed-specialisations -Wno-implicit-prelude -Wno-incomplete-uni-patterns -Wno-orphans #-}

import Control.Monad.State (State, evalState, get, modify', put, runState)
import Data.Tree (Tree (Node), flatten, foldTree, levels, rootLabel)
import Test.QuickCheck (Arbitrary, Gen, arbitrary, chooseInt, genericShrink, quickCheck, shrink, sized, vectorOf)

-- tree

dfn :: Tree a -> Tree (a, Int)
dfn t = evalState (go t) 1
  where
    go :: Tree a -> State Int (Tree (a, Int))
    go (Node x ts) = do
      i <- get
      put $ i + 1
      Node (x, i) <$> mapM go ts

bfn :: Tree a -> Tree (a, Int)
bfn t = let (t', is) = runState (go t) (1 : is) in t'
  where
    go :: Tree a -> State [Int] (Tree (a, Int))
    go (Node x ts) = do
      is <- get
      let i : is' = is
      put is'
      ts' <- mapM go ts
      modify' (i + 1 :)
      return $ Node (x, i) ts'

wrong :: (a -> a -> a) -> Tree a -> Tree a -> Tree a
wrong f (Node x tsx) (Node y tsy) = Node (f x y) $ zipWith (wrong f) tsx tsy

zipWith' :: (a -> a -> a) -> [a] -> [a] -> [a]
zipWith' f = go
  where
    go [] ys = ys
    go xs [] = xs
    go (x : xs) (y : ys) = f x y : go xs ys

merge :: (a -> a -> a) -> Tree a -> Tree a -> Tree a
merge f = go
  where
    go (Node x1 ts1) (Node x2 ts2) = Node (f x1 x2) (zipWith' go ts1 ts2)

size :: Tree a -> Int
size = foldTree (\_ xs -> 1 + sum xs)

-- quickcheck

instance Arbitrary a => Arbitrary (Tree a) where
  arbitrary = sized go
    where
      go :: Arbitrary a => Int -> Gen (Tree a)
      go n = do
        x <- arbitrary
        n' <- chooseInt (0, n `div` 2)
        Node x <$> vectorOf n' (go (n `div` 4))
  shrink = genericShrink

prop_SizeEq :: (Tree a -> Tree (a, Int)) -> Tree a -> Bool
prop_SizeEq f t = size (f t) == size t

prop_RootLabelSnd :: (Tree a -> Tree (a, Int)) -> Tree a -> Bool
prop_RootLabelSnd f t = snd (rootLabel (f t)) == 1

prop_Fst :: Eq a => (Tree a -> Tree (a, Int)) -> Tree a -> Bool
prop_Fst f t = fmap fst (f t) == t

prop_SndFlatten :: Tree a -> Bool
prop_SndFlatten t = flatten (fmap snd (dfn t)) == [1 .. size t]

prop_SndLevels :: Tree a -> Bool
prop_SndLevels t = concat (levels (fmap snd (bfn t))) == [1 .. size t]

prop_SizeGe :: (Tree a -> Tree a -> Tree a) -> Tree a -> Tree a -> Bool
prop_SizeGe f t1 t2 = size (f t1 t2) >= max (size t1) (size t2)

-- main

main :: IO ()
main = do
  putStrLn "Ckecking dfn"
  quickCheck (prop_SizeEq dfn :: Tree Int -> Bool)
  quickCheck (prop_RootLabelSnd dfn :: Tree Int -> Bool)
  quickCheck (prop_Fst dfn :: Tree Int -> Bool)
  quickCheck (prop_SndFlatten :: Tree Int -> Bool)
  putStrLn "\nCkecking bfn"
  quickCheck (prop_SizeEq bfn :: Tree Int -> Bool)
  quickCheck (prop_RootLabelSnd bfn :: Tree Int -> Bool)
  quickCheck (prop_Fst bfn :: Tree Int -> Bool)
  quickCheck (prop_SndLevels :: Tree Int -> Bool)
  putStrLn "\nCkecking wrong"
  quickCheck (prop_SizeGe (wrong (+)) :: Tree Int -> Tree Int -> Bool)
  putStrLn "\nCkecking merge"
  quickCheck (prop_SizeGe (merge (+)) :: Tree Int -> Tree Int -> Bool)
