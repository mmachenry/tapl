module Core (eval) where

import Syntax

-- Fully evaluate the term by recursively applying the single step evaluator.
eval :: Term -> Term
eval t = case eval1 t of
  -- I'm not sure but it seems as though this is actually broken
  -- in Pierce's original implementation so I've added "if t' == t .."
  Just t' -> if t' == t then t else eval t'
  Nothing -> t

-- True if the given term is a numerical value. In short a Peano natural number
-- consisting of a chain of calls to succ terminating at a zero. Anything else
-- is either a type error or needs to be evaluated.
isNumericVal :: Term -> Bool
isNumericVal t = case t of
  TmZero -> True
  TmSucc t1 -> isNumericVal t1
  _ -> False

-- True if the given term represents a fully evaluated value.
isVal :: Term -> Bool
isVal t = case t of
  TmTrue -> True
  TmFalse -> True
  t -> isNumericVal t

-- Evaluate the given term one step.
eval1 :: Term -> Maybe Term
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

