import Control.Monad.State
import Data.Char
import Data.Map.Strict (Map, empty, (!), insert, lookup)
import Data.Maybe
import System.IO
import Text.Read (Lexeme(..), lexP, (<++), readPrec)

data Type = Tvar Int | Tfun Type Type
    deriving Eq
data Expr = Evar String | Eabs String Expr | Eapp Expr Expr
    deriving (Eq, Ord)

-- Pretty printing of expressions

always = True -- False omits parentheses whenever possible

instance Show Expr where
    showsPrec p (Evar x) = (x ++)
    showsPrec p (Eabs x e) =
        showParen (always || p > 0) ((("\\" ++ x ++ ". ") ++) . showsPrec 0 e)
    showsPrec p (Eapp e1 e2) =
        showParen (always || p > 1) (showsPrec 1 e1 . (" " ++) . showsPrec 2 e2)

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
    showsPrec p (Tvar alpha) = ("@" ++) . showsPrec 0 alpha
    showsPrec p (Tfun sigma tau) =
        showParen (p > 0) (showsPrec 1 sigma . (" -> " ++) . showsPrec 0 tau)

-- Main program

readOne = do
    s <- getLine
    let e = read s :: Expr
    print e
    let (tau, substitutions) = foo e
    print tau
    print substitutions
    case unify substitutions of
        Just mgu -> do
            print $ mgu
            print $ lexicographize $ properSubstitute mgu tau
        Nothing -> putStrLn "type error"
    -- TODO: OPTIMIZATIONS

count n m = sequence $ take n $ repeat m

main = do
    n <- readLn
    count n readOne

data Sub = Sub Type Type
    deriving Show

foo :: Expr -> (Type, [Sub])
foo e = evalState (aux empty e) 0
    where
        aux :: Map Expr Type -> Expr -> State Int (Type, [Sub])
        aux gamma e = case e of
            Evar _ -> do
                let tau = gamma ! e
                return (tau, [])
            Eabs x e' -> do
                i <- get
                let alpha = Tvar i
                let gamma' = insert (Evar x) alpha gamma
                modify (+ 1)
                (tau, c) <- aux gamma' e'
                return (Tfun alpha tau, c)
            Eapp e1 e2 -> do
                (sigma, c1) <- aux gamma e1
                (tau, c2) <- aux gamma e2
                i <- get
                let alpha = Tvar i
                return (alpha, (Sub sigma (Tfun tau alpha)) : c1 ++ c2)

unify :: [Sub] -> Maybe [Sub]
unify [] = Just []
unify (c : cs) =
    case c of
        Sub tau1 tau2 | tau1 == tau2 -> unify cs
        Sub (alpha @ (Tvar _)) tau2 | not (contains alpha tau2) -> do
            let sub = Sub alpha tau2
            foobar <- unify (bar sub cs)
            return (compose foobar sub)
        Sub tau1 (alpha @ (Tvar _)) | not (contains alpha tau1) -> do
            let sub = Sub alpha tau1
            foobar <- unify (bar sub cs)
            return (compose foobar sub)
        Sub (Tfun tau11 tau12) (Tfun tau21 tau22) ->
            unify ((Sub tau11 tau21) : (Sub tau12 tau22) : cs)
        otherwise -> Nothing
    where
        contains :: Type -> Type -> Bool
        contains alpha tau = go tau
            where go tau = case tau of
                    Tvar _ -> alpha == tau
                    Tfun tau1 tau2 -> go tau1 || go tau2

        compose :: [Sub] -> Sub -> [Sub]
        compose [] c = [c]
        compose (c @ (Sub a _) : cs) (c' @ (Sub alpha tau))
            | contains a tau = compose cs (Sub alpha (substitute c tau))
            | otherwise = c : compose cs c'

        bar :: Sub -> [Sub] -> [Sub]
        bar sub c = map (\(Sub x y) -> Sub (substitute sub x) (substitute sub y)) c

properSubstitute :: [Sub] -> Type -> Type
properSubstitute [] tau = tau
properSubstitute (mgu : mgus) tau = properSubstitute mgus (substitute mgu tau)

substitute :: Sub -> Type -> Type
substitute (Sub alpha tau1) tau2 = go tau2
    where go (tau' @ (Tvar _)) = if alpha == tau' then tau1 else tau'
          go (Tfun tau1 tau2) = Tfun (go tau1) (go tau2)

lexicographize :: Type -> Type
lexicographize sigma = evalState (aux sigma) (0, empty)
    where
        aux :: Type -> State (Int, Map Int Int) Type
        aux (Tvar x) = do
            (i, m) <- get
            case Data.Map.Strict.lookup x m of
                Just x' -> return (Tvar x')
                Nothing -> do
                    put (i + 1, insert x i m)
                    return (Tvar i)
        aux (Tfun sigma tau) = do
            sigma' <- aux sigma
            tau' <- aux tau
            return (Tfun sigma' tau')
