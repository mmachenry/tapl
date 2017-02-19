module Core where

import Syntax

isNumericVal t = case t of
  TmZero -> True
  TmSucc t1 -> isNumericVal t1
  _ -> False

isVal t = case t of
  TmTrue -> True
  TmFalse -> True
  t -> isNumericVal t

eval1 (TmIf TmTrue t2 t3) = return t2
eval1 (TmIf TmFalse t2 t3) = return t3
eval1 (TmIf t1 t2 t3) = do
  t1' <- eval1 t1
  return $ TmIf t1' t2 t3
eval1 (TmSucc t1) = fmap TmSucc t1'
  where t1' = eval1 t1
eval1 (TmPred TmZero) = return TmZero
eval1 (TmPred (TmSucc nv1))
  | isNumericVal nv1 = return nv1
eval1 (TmPred t1) = fmap TmPred t1'
  where t1' = eval1 t1
eval1 (TmIsZero TmZero) = return TmTrue
eval1 (TmIsZero (TmSucc nv1))
  | isNumericVal nv1 = return TmFalse
eval1 (TmIsZero t1) = fmap TmIsZero t1'
  where t1' = eval1 t1
eval1 _ = Nothing

eval t = case eval1 t of
  -- I'm not sure but it seems as though this is actually broken
  -- in Pierce's original implementation so I've added "if t' == t .."
  Just t' -> if t' == t then t else eval t'
  Nothing -> t

