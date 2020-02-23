{-# OPTIONS_GHC -Wall -fwarn-tabs -fwarn-incomplete-record-updates -fwarn-monomorphism-restriction -fwarn-unused-do-bind #-}
{-# OPTIONS_GHC -O2 #-}
{-# OPTIONS_GHC -threaded -rtsopts -with-rtsopts=-N #-}

import Control.Monad.State
import qualified Data.Map.Strict as Map
import Text.Read (Lexeme(..), lexP, readPrec, (<++))

data Type = Tvar Int | Tfun Type Type deriving Eq
data Expr = Evar String | Eabs String Expr | Eapp Expr Expr deriving (Eq, Ord)

-- Parsing of expressions

instance Read Expr where
  readPrec = (do Ident x <- lexP
                 return (Evar x)) <++
             (do Punc "(" <- lexP
                 Punc "\\" <- lexP
                 Ident x <- lexP
                 Symbol "." <- lexP
                 e <- readPrec
                 Punc ")" <- lexP
                 return (Eabs x e)) <++
             (do Punc "(" <- lexP
                 e1 <- readPrec
                 e2 <- readPrec
                 Punc ")" <- lexP
                 return (Eapp e1 e2))

-- Pretty printing of types

instance Show Type where
  showsPrec _ (Tvar alpha) = ("@" ++) . showsPrec 0 alpha
  showsPrec p (Tfun sigma tau) =
    showParen (p > 0) (showsPrec 1 sigma . (" -> " ++) . showsPrec 0 tau)

-- Type inference

data Sub = Sub Type Type

instance Show Sub where
  show (Sub t1 t2) = show t1 ++ " = " ++ show t2

infer :: Expr -> (Type, [Sub])
infer e = evalState (go Map.empty e) 0
  where go :: Map.Map Expr Type -> Expr -> State Int (Type, [Sub])
        go gamma e' @ (Evar _) = do
          let tau = gamma Map.! e'
          return (tau, [])
        go gamma (Eabs x e') = do
          i <- get
          let alpha = Tvar i
          let gamma' = Map.insert (Evar x) alpha gamma
          modify' (+ 1)
          (tau, c) <- go gamma' e'
          return (Tfun alpha tau, c)
        go gamma (Eapp e1 e2) = do
          (sigma, c1) <- go gamma e1
          (tau, c2) <- go gamma e2
          i <- get
          let alpha = Tvar i
          modify' (+ 1)
          return (alpha, (Sub sigma (Tfun tau alpha)) : c1 ++ c2)

unify :: [Sub] -> Maybe [Sub]
unify = go []
  where go mgu [] = Just mgu
        go mgu ((Sub tau1 tau2) : subs) | tau1 == tau2 = go mgu subs
        go mgu ((sub @ (Sub alpha @ (Tvar _) tau2)) : subs) | not (contains alpha tau2) =
          go (sub : mgu) (replace sub subs)
        go mgu (Sub tau1 alpha @ (Tvar _) : subs) = go mgu ((Sub alpha tau1) : subs)
        go mgu ((Sub (Tfun tau11 tau12) (Tfun tau21 tau22)) : subs) =
          go mgu ((Sub tau11 tau21) : (Sub tau12 tau22) : subs)
        go _ _ = Nothing

contains :: Type -> Type -> Bool
contains alpha = go
  where go tau @ (Tvar _) = alpha == tau
        go (Tfun tau1 tau2) = go tau1 || go tau2

replace :: Sub -> [Sub] -> [Sub]
replace sub c = map (\(Sub x y) -> Sub (substitute sub x) (substitute sub y)) c

properSubstitute :: [Sub] -> Type -> Type
properSubstitute subs tau =
  let tau' = substituteAll subs tau
  in if tau == tau' then tau else properSubstitute subs tau'

substituteAll :: [Sub] -> Type -> Type
substituteAll [] tau = tau
substituteAll (mgu : mgus) tau = substituteAll mgus (substitute mgu tau)

substitute :: Sub -> Type -> Type
substitute (Sub alpha tau) = go
  where go tau' @ (Tvar _) = if alpha == tau' then tau else tau'
        go (Tfun tau1 tau2) = Tfun (go tau1) (go tau2)

lexicographize :: Type -> Type
lexicographize t = evalState (go t) (0, Map.empty)
  where go :: Type -> State (Int, Map.Map Int Int) Type
        go (Tvar x) = do
          (i, m) <- get
          case Map.lookup x m of
            Just x' -> return (Tvar x')
            Nothing -> do
              put (i + 1, Map.insert x i m)
              return (Tvar i)
        go (Tfun t1 t2) = do
          t1' <- go t1
          t2' <- go t2
          return (Tfun t1' t2')

-- Main program

readOne :: IO ()
readOne = do
  s <- getLine
  let e = read s :: Expr
  let (tau, substitutions) = infer e
  case unify substitutions of
    Just mgu -> print $ lexicographize $ properSubstitute mgu tau
    Nothing -> putStrLn "type error"

count :: Int -> IO a -> IO [a]
count n m = sequence $ take n $ repeat m

main :: IO [()]
main = do
  n <- readLn
  count n readOne
