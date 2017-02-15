module Syntax where

import Support

data Term =
    TmVar Info Int Int
  | TmAbs Info String Term
  | TmApp Info Term Term
  deriving (Show, Eq)

termShift d t = walk 0 t
  where walk c t = case t of
          TmVar fi x n -> if x >= c
                          then TmVar fi (x+d) (n+d)
                          else TmVar fi x (n+d)
          TmAbs fi x t1 -> TmAbs fi x (walk (c+1) t1)
          TmApp fi t1 t2 -> TmApp fi (walk c t1) (walk c t2)

termSubst j s t = walk 0 t
  where walk c t = case t of
          TmVar fi x n -> if x == j + c
                          then termShift c s
                          else TmVar fi x n
          TmAbs fi x t1 -> TmAbs fi x (walk (c+1) t1)
          TmApp fi t1 t2 -> TmApp fi (walk c t1) (walk c t2)

termSubstTop s t = termShift (negate 1) (termSubst 0 (termShift 1 s) t)

-- todo implement pretty printer
printTm ctx t = show t
