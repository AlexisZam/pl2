{-# LANGUAGE Safe #-}
{-# OPTIONS_GHC -O2 -Weverything -Wno-implicit-prelude -Wno-missing-deriving-strategies -Wno-prepositive-qualified-module #-}

import Control.Monad.State (State, evalState, get, modify', put, replicateM)
import Data.List (foldl')
import qualified Data.Map.Strict as Map
import Text.Read ((<++), Lexeme (Ident, Punc, Symbol), lexP, readPrec)

data Type = Tvar Int | Tfun Type Type deriving (Eq)

data Expr = Evar String | Eabs String Expr | Eapp Expr Expr deriving (Eq, Ord)

-- Parsing of expressions

instance Read Expr where
  readPrec =
    ( do
        Ident x <- lexP
        return (Evar x)
    )
      <++ ( do
              Punc "(" <- lexP
              Punc "\\" <- lexP
              Ident x <- lexP
              Symbol "." <- lexP
              e <- readPrec
              Punc ")" <- lexP
              return (Eabs x e)
          )
      <++ ( do
              Punc "(" <- lexP
              e1 <- readPrec
              e2 <- readPrec
              Punc ")" <- lexP
              return (Eapp e1 e2)
          )

-- Pretty printing of types

instance Show Type where
  showsPrec _ (Tvar a) = ("@" ++) . shows a
  showsPrec p (Tfun t1 t2) =
    showParen (p > 0) (showsPrec 1 t1 . (" -> " ++) . shows t2)

-- Type inference

data Sub = Sub Type Type

type Env = Map.Map Expr Type

infer :: Expr -> (Type, [Sub])
infer e = evalState (go Map.empty e) 0
  where
    go :: Env -> Expr -> State Int (Type, [Sub])
    go env x@(Evar _) = return (env Map.! x, [])
    go env (Eabs x e') = do
      i <- get
      modify' (+ 1)
      let a = Tvar i
      (t, c) <- go (Map.insert (Evar x) a env) e'
      return (Tfun a t, c)
    go env (Eapp e1 e2) = do
      (t1, c1) <- go env e1
      (t2, c2) <- go env e2
      i <- get
      let a = Tvar i
      modify' (+ 1)
      return (a, Sub t1 (Tfun t2 a) : c1 ++ c2)

unify :: [Sub] -> Maybe [Sub]
unify = go []
  where
    go mgu [] = Just mgu
    go mgu (Sub t1 t2 : subs)
      | t1 == t2 = go mgu subs
    go mgu (sub@(Sub a@(Tvar _) t2) : subs)
      | not (contains a t2) = go (sub : mgu) (replace sub subs)
    go mgu (Sub t1 a@(Tvar _) : subs) =
      go mgu (Sub a t1 : subs)
    go mgu (Sub (Tfun t1 t2) (Tfun t1' t2') : subs) =
      go mgu (Sub t1 t1' : Sub t2 t2' : subs)
    go _ _ = Nothing

contains :: Type -> Type -> Bool
contains a = go
  where
    go a'@(Tvar _) = a == a'
    go (Tfun t1 t2) = go t1 || go t2

replace :: Sub -> [Sub] -> [Sub]
replace sub = map (\(Sub x y) -> Sub (substitute sub x) (substitute sub y))

substitute :: Sub -> Type -> Type
substitute (Sub a t) = go
  where
    go a'@(Tvar _) = if a == a' then t else a'
    go (Tfun t1 t2) = Tfun (go t1) (go t2)

properSubstitute :: [Sub] -> Type -> Type
properSubstitute subs t =
  let t' = foldl' (flip substitute) t subs
   in if t == t' then t else properSubstitute subs t'

enumerate :: Type -> Type
enumerate t = evalState (go t) (0, Map.empty)
  where
    go :: Type -> State (Int, Map.Map Int Int) Type
    go (Tvar a) = do
      (i, m) <- get
      case Map.lookup a m of
        Just a' -> return (Tvar a')
        Nothing -> do
          put (i + 1, Map.insert a i m)
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
  let (t, subs) = infer e
  case unify subs of
    Just mgu -> print $ enumerate $ properSubstitute mgu t
    Nothing -> putStrLn "type error"

main :: IO [()]
main = do
  n <- readLn
  replicateM n readOne
