{-# LANGUAGE Safe #-}
{-# OPTIONS_GHC -O2 -Weverything -Wno-all-missed-specialisations -Wno-implicit-prelude #-}

import Control.Monad.State (State, evalState, get, put, runState)
import Data.Tree (Tree (Node), drawTree, foldTree, rootLabel)
import Test.QuickCheck (Arbitrary, Gen, arbitrary, chooseInt, generate, quickCheck, sized, vectorOf)

-- tree

dfn :: Tree a -> Tree (a, Int)
dfn t = evalState (go t) 1
  where
    go :: Tree a -> State Int (Tree (a, Int))
    go (Node x ts) = do
      i <- get
      put $ i + 1
      ts' <- mapM go ts
      return $ Node (x, i) ts'

bfn :: Tree a -> Tree (a, Int)
bfn t =
  let (t', is) = runState (go t) (1 : is)
   in t'
  where
    go :: Tree a -> State [Int] (Tree (a, Int))
    go (Node x ts) = do
      is <- get
      let i : is' = is
      put is'
      ts' <- mapM go ts
      is'' <- get
      put $ i + 1 : is''
      return $ Node (x, i) ts'

size :: Tree a -> Int
size = foldTree (\_ ts -> 1 + sum ts)

wrong :: (a -> a -> a) -> Tree a -> Tree a -> Tree a
wrong f (Node x tsx) (Node y tsy) = Node (f x y) $ zipWith (wrong f) tsx tsy

-- zipWith :: (a->b->c) -> [a]->[b]->[c]
-- zipWith f = go
--   where
--     go [] _ = []
--     go _ [] = []
--     go (x:xs) (y:ys) = f x y : go xs ys

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

-- quickcheck

-- instance Arbitrary a => Arbitrary (Tree a) where
--   arbitrary =
--     sized arbitrarySizedTree

-- arbitrarySizedTree :: Arbitrary a => Int -> Gen (Tree a)
-- arbitrarySizedTree m = do
--   t <- arbitrary
--   n <- choose (0, m `div` 2)
--   ts <- vectorOf n (arbitrarySizedTree (m `div` 4))
--   return (Tree t ts)

instance Arbitrary a => Arbitrary (Tree a) where
  arbitrary = sized go
    where
      go :: Arbitrary a => Int -> Gen (Tree a)
      go n = do
        x <- arbitrary
        m <- chooseInt (0, n `div` 4)
        ts <- vectorOf m (go (n `div` 4))
        return $ Node x ts

prop_SizeEq :: (Tree a -> Tree (a, Int)) -> Tree a -> Bool
prop_SizeEq f t = size (f t) == size t

prop_SizeGe :: (Tree a -> Tree a -> Tree a) -> Tree a -> Tree a -> Bool
prop_SizeGe f t1 t2 = size (f t1 t2) >= max (size t1) (size t2)

prop_RootLabel :: (Tree a -> Tree (a, Int)) -> Tree a -> Bool
prop_RootLabel f t = snd (rootLabel (f t)) == 1

-- main

main :: IO ()
main = do
  t1 <- generate arbitrary :: IO (Tree Int)
  t2 <- generate arbitrary :: IO (Tree Int)
  -- putStrLn $ drawTree $ show <$> dfn t
  -- putStrLn $ drawTree $ show <$> bfn t
  putStrLn $ drawTree $ show <$> t1
  putStrLn $ drawTree $ show <$> t2
  putStrLn $ drawTree $ show <$> merge (+) t1 t2
  putStrLn "Ckecking dfn"
  quickCheck (prop_SizeEq dfn :: Tree Int -> Bool)
  quickCheck (prop_RootLabel dfn :: Tree Int -> Bool)
  putStrLn "Ckecking bfn"
  quickCheck (prop_SizeEq bfn :: Tree Int -> Bool)
  quickCheck (prop_RootLabel dfn :: Tree Int -> Bool)
  putStrLn "Ckecking wrong"
  quickCheck (prop_SizeGe (wrong (+)) :: Tree Int -> Tree Int -> Bool)
  putStrLn "Ckecking merge"
  quickCheck (prop_SizeGe (merge (+)) :: Tree Int -> Tree Int -> Bool)
