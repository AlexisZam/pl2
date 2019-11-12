{-# LANGUAGE Safe #-}
{-# OPTIONS_GHC -O2 -Weverything -Wno-implicit-prelude -Wno-all-missed-specialisations #-}

import Control.Monad.Fix (fix)

-- Syntax

data P
  = Pint Integer
  | Ptrue
  | Pfalse
  | Padd
  | Pneg
  | Pmul
  | Pdiv
  | Plt
  | Peq
  | Pand
  | Pnot
  | Pnop
  | Pdup
  | Ppop
  | Pswap
  | Pswap2
  | Pseq P P
  | Pcond P P
  | Ploop P

-- Denotational semantics

data V = VI Integer | VB Bool

type S = [V]

sem :: P -> S -> S
sem (Pint n) s = VI n : s
sem Ptrue s = VB True : s
sem Pfalse s = VB False : s
sem Padd (VI n2 : VI n1 : s) = VI (n1 + n2) : s
sem Pneg (VI n : s) = VI (- n) : s
sem Pmul (VI n2 : VI n1 : s) = VI (n1 * n2) : s
sem Pdiv (VI n2 : VI n1 : s) | n2 /= 0 = VI r : VI q : s
  where
    (q, r) = quotRem n1 n2
sem Plt (VI n2 : VI n1 : s) = VB (n1 < n2) : s
sem Peq (VI n2 : VI n1 : s) = VB (n1 == n2) : s
sem Pand (VB b2 : VB b1 : s) = VB (b1 && b2) : s
sem Pnot (VB b : s) = VB (not b) : s
sem Pnop s = s
sem Pdup (u : s) = u : u : s
sem Ppop (_ : s) = s
sem Pswap (u2 : u1 : s) = u1 : u2 : s
sem Pswap2 (u3 : u2 : u1 : s) = u2 : u1 : u3 : s
sem (Pseq p1 p2) s = sem p2 (sem p1 s)
sem (Pcond p1 _) (VB True : s) = sem p1 s
sem (Pcond _ p2) (VB False : s) = sem p2 s
sem (Ploop p) s = fix f s
  where
    f g (VB True : s') = g (sem p s')
    f _ (VB False : s') = s'
    f _ _ = undefined
sem _ _ = undefined

-- Main function: interpreter

main :: IO ()
main = do
  input <- getContents
  mapM_ print $ sem (read input) []

-- Pretty-printing

instance Show P where
  showsPrec _ (Pint n) = shows n
  showsPrec _ Ptrue = ("true" ++)
  showsPrec _ Pfalse = ("false" ++)
  showsPrec _ Padd = ("+" ++)
  showsPrec _ Pneg = ("-" ++)
  showsPrec _ Pmul = ("*" ++)
  showsPrec _ Pdiv = ("/" ++)
  showsPrec _ Plt = ("<" ++)
  showsPrec _ Peq = ("=" ++)
  showsPrec _ Pand = ("and" ++)
  showsPrec _ Pnot = ("not" ++)
  showsPrec _ Pnop = ("nop" ++)
  showsPrec _ Pdup = ("dup" ++)
  showsPrec _ Ppop = ("pop" ++)
  showsPrec _ Pswap = ("swap" ++)
  showsPrec _ Pswap2 = ("swap2" ++)
  showsPrec d (Pseq p1 p2) = showParen (d > 0) $ showsPrec 1 p1 . (" " ++) . shows p2
  showsPrec _ (Pcond p1 p2) = ("cond [" ++) . shows p1 . (" | " ++) . shows p2 . ("]" ++)
  showsPrec _ (Ploop p) = ("loop [" ++) . shows p . ("]" ++)

instance Show V where
  showsPrec d (VI n) = showsPrec d n
  showsPrec _ (VB True) = ("true" ++)
  showsPrec _ (VB False) = ("false" ++)

-- Parsing

instance Read P where
  readsPrec d s =
    readParen False (\s' -> [(Pint n, r) | (n, r) <- reads s']) s
      ++ readParen False (\s' -> [(Ptrue, r) | ("true", r) <- lex s']) s
      ++ readParen False (\s' -> [(Pfalse, r) | ("false", r) <- lex s']) s
      ++ readParen False (\s' -> [(Padd, r) | ("+", r) <- lex s']) s
      ++ readParen False (\s' -> [(Pneg, r) | ("-", r) <- lex s']) s
      ++ readParen False (\s' -> [(Pmul, r) | ("*", r) <- lex s']) s
      ++ readParen False (\s' -> [(Pdiv, r) | ("/", r) <- lex s']) s
      ++ readParen False (\s' -> [(Plt, r) | ("<", r) <- lex s']) s
      ++ readParen False (\s' -> [(Peq, r) | ("=", r) <- lex s']) s
      ++ readParen False (\s' -> [(Pand, r) | ("and", r) <- lex s']) s
      ++ readParen False (\s' -> [(Pnot, r) | ("not", r) <- lex s']) s
      ++ readParen False (\s' -> [(Pnop, r) | ("nop", r) <- lex s']) s
      ++ readParen False (\s' -> [(Pdup, r) | ("dup", r) <- lex s']) s
      ++ readParen False (\s' -> [(Ppop, r) | ("pop", r) <- lex s']) s
      ++ readParen False (\s' -> [(Pswap, r) | ("swap", r) <- lex s']) s
      ++ readParen False (\s' -> [(Pswap2, r) | ("swap2", r) <- lex s']) s
      ++ readParen (d > 0) (\s' -> [(Pseq p1 p2, r) | (p1, t) <- readsPrec 1 s', (p2, r) <- reads t]) s
      ++ readParen False (\s' -> [(Pcond p1 p2, r) | ("cond", t1) <- lex s', ("[", t2) <- lex t1, (p1, t3) <- reads t2, ("|", t4) <- lex t3, (p2, t5) <- reads t4, ("]", r) <- lex t5]) s
      ++ readParen False (\s' -> [(Ploop p, r) | ("loop", t1) <- lex s', ("[", t2) <- lex t1, (p, t3) <- reads t2, ("]", r) <- lex t3]) s
