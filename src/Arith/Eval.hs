module Arith.Eval where

import Arith.Ast

isNumericVal t = case t of
  TmZero _ -> True
  TmSucc _ t1 -> isNumericVal t1
  _ -> False

isVal t = case t of
  TmTrue _ -> True
  TmFalse _ -> True
  t -> isNumericVal t

eval1 (TmIf _ (TmTrue _) t2 t3) = t2
eval1 (TmIf _ (TmFalse _) t2 t3) = t3
eval1 (TmIf fi t1 t2 t3) = TmIf fi t1' t2 t3
  where t1' = eval1 t1
eval1 (TmSucc fi t1) = TmSucc fi t1'
  where t1' = eval1 t1
eval1 (TmPred _ (TmZero _)) = TmZero dummyInfo
eval1 (TmPred _ (TmSucc _ nv1))
  | isNumericVal nv1 = nv1
eval1 (TmPred fi t1) = TmPred fi t1'
  where t1' = eval1 t1
eval1 (TmIsZero _ (TmZero _)) = TmTrue dummyInfo
eval1 (TmIsZero _ (TmSucc _ nv1))
  | isNumericVal nv1 = TmFalse dummyInfo
eval1 (TmIsZero fi t1) = TmIsZero fi t1'
  where t1' = eval1 t1
eval1 _ = error "No rule applies"

