{-# LANGUAGE Safe #-}
{-# OPTIONS_GHC -O2 -Weverything -Wno-implicit-prelude -Wno-missing-deriving-strategies -Wno-prepositive-qualified-module #-}

import Control.Monad.State (State, evalState, get, put, replicateM_)
import Data.List (foldl')
import qualified Data.Map.Strict as Map
import Text.Read ((<++), Lexeme (Ident, Punc, Symbol), lexP, readPrec)

data Type = Tvar Int | Tfun Type Type deriving (Eq)

data Expr = Evar String | Eabs String Expr | Eapp Expr Expr deriving (Eq)

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
  showsPrec _ (Tvar alpha) = ("@" ++) . shows alpha
  showsPrec p (Tfun sigma tau) =
    showParen (p > 0) (showsPrec 1 sigma . (" -> " ++) . shows tau)

-- Type inference

data Sub = Sub Type Type

type Env = Map.Map String Type

constraints :: Expr -> (Type, [Sub])
constraints e = evalState (go Map.empty e) 0
  where
    go :: Env -> Expr -> State Int (Type, [Sub])
    go env (Evar x) = return (env Map.! x, [])
    go env (Eabs x e') = do
      a <- fresh
      (t, c) <- go (Map.insert x a env) e'
      return (Tfun a t, c)
    go env (Eapp e1 e2) = do
      (t1, c1) <- go env e1
      (t2, c2) <- go env e2
      a <- fresh
      return (a, Sub t1 (Tfun t2 a) : c1 ++ c2)

fresh :: State Int Type
fresh = do
  i <- get
  put (i + 1)
  return (Tvar i)

mgu :: [Sub] -> Maybe [Sub]
mgu [] = Just []
mgu (Sub t1 t2 : subs) | t1 == t2 = mgu subs
mgu (sub@(Sub a@(Tvar _) t) : subs)
  | a `notElem'` t = (sub :) <$> mgu (map (substitute' sub) subs)
mgu (Sub t a@(Tvar _) : subs) = mgu (Sub a t : subs)
mgu (Sub (Tfun t1 t2) (Tfun t1' t2') : subs) = mgu (Sub t1 t1' : Sub t2 t2' : subs)
mgu _ = Nothing

notElem' :: Type -> Type -> Bool
notElem' a@(Tvar _) = go
  where
    go a'@(Tvar _) = a /= a'
    go (Tfun t1 t2) = go t1 && go t2
notElem' _ = undefined

substitute :: Sub -> Type -> Type
substitute (Sub a@(Tvar _) t) = go
  where
    go a'@(Tvar _) = if a == a' then t else a'
    go (Tfun t1 t2) = Tfun (go t1) (go t2)
substitute _ = undefined

substitute' :: Sub -> Sub -> Sub
substitute' sub (Sub t1 t2) = Sub (substitute sub t1) (substitute sub t2)

substitute'' :: [Sub] -> Type -> Type
substitute'' subs t = if t == t' then t else substitute'' subs t'
  where
    t' = foldl' (flip substitute) t subs

enumerate :: Type -> Type
enumerate t = evalState (go t) (0, Map.empty)
  where
    go :: Type -> State (Int, Map.Map Int Int) Type
    go (Tvar a) = do
      (i, m) <- get
      maybe
        (put (i + 1, Map.insert a i m) >> return (Tvar i))
        (return . Tvar)
        (Map.lookup a m)
    go (Tfun t1 t2) = do
      t1' <- go t1
      t2' <- go t2
      return (Tfun t1' t2')

inferType :: Expr -> Maybe Type
inferType e = do
  let (t, subs) = constraints e
  enumerate . flip substitute'' t <$> mgu subs

-- Main program

readOne :: IO ()
readOne = do
  s <- getLine
  let e = read s :: Expr
  maybe (putStrLn "type error") print (inferType e)

main :: IO ()
main = do
  n <- readLn
  replicateM_ n readOne
