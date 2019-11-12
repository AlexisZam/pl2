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

infer :: Expr -> Maybe Type
infer e = do
  let (t, subs) = derive e
  sort . flip substitute'' t <$> unify subs

derive :: Expr -> (Type, [Sub])
derive e = evalState (go Map.empty e) 0
  where
    go :: Map.Map String Type -> Expr -> State Int (Type, [Sub])
    go m (Evar x) = return (m Map.! x, [])
    go m (Eabs x e') = do
      a <- newTvar
      (t, c) <- go (Map.insert x a m) e'
      return (Tfun a t, c)
    go m (Eapp e1 e2) = do
      (t1, c1) <- go m e1
      (t2, c2) <- go m e2
      a <- newTvar
      return (a, Sub t1 (Tfun t2 a) : c1 ++ c2)

newTvar :: State Int Type
newTvar = do
  i <- get
  put (i + 1)
  return (Tvar i)

unify :: [Sub] -> Maybe [Sub]
unify [] = Just []
unify (Sub t1 t2 : subs) | t1 == t2 = unify subs
unify (sub@(Sub a@(Tvar _) t) : subs)
  | a `notElem'` t = (sub :) <$> unify (substitute' sub subs)
unify (Sub t a@(Tvar _) : subs) = unify (Sub a t : subs)
unify (Sub (Tfun t1 t2) (Tfun t1' t2') : subs) = unify (Sub t1 t1' : Sub t2 t2' : subs)
unify _ = Nothing

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

substitute' :: Sub -> [Sub] -> [Sub]
substitute' sub = map go
  where
    go (Sub t1 t2) = Sub (substitute sub t1) (substitute sub t2)

substitute'' :: [Sub] -> Type -> Type
substitute'' subs t = if t == t' then t else substitute'' subs t'
  where
    t' = foldl' (flip substitute) t subs

sort :: Type -> Type
sort t = evalState (go t) (0, Map.empty)
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

-- Main program

readOne :: IO ()
readOne = do
  s <- getLine
  let e = read s :: Expr
  maybe (putStrLn "type error") print (infer e)

main :: IO ()
main = do
  n <- readLn
  replicateM_ n readOne
