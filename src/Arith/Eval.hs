module Arith.Eval where

import Arith.Ast

isNumericVal t = case t of
  TmZero -> True
  TmSucc t1 -> isNumericVal t1
  _ -> False

isVal t = case t of
  TmTrue -> True
  TmFalse -> True
  t -> isNumericVal t

eval1 (TmIf TmTrue t2 t3) = t2
eval1 (TmIf TmFalse t2 t3) = t3
eval1 (TmIf t1 t2 t3) = TmIf t1' t2 t3
  where t1' = eval1 t1
eval1 (TmSucc t1) = TmSucc t1'
  where t1' = eval1 t1
eval1 (TmPred TmZero) = TmZero
eval1 (TmPred (TmSucc nv1))
  | isNumericVal nv1 = nv1
eval1 (TmPred t1) = TmPred t1'
  where t1' = eval1 t1
eval1 (TmIsZero TmZero) = TmTrue
eval1 (TmIsZero (TmSucc nv1))
  | isNumericVal nv1 = TmFalse
eval1 (TmIsZero t1) = TmIsZero t1'
  where t1' = eval1 t1
eval1 _ = error "No rule applies"

