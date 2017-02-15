module Core where

import Syntax

isVal ctx t = case t of
  TmAbs _ _ _ -> True
  _ -> False

eval1 ctx (TmApp fi (TmAbs _ x t12) v2) | isVal ctx v2 =
  return $ termSubstTop v2 t12
eval1 ctx (TmApp fi v1 t2) | isVal ctx v1 = do
  t2' <- eval1 ctx t2
  return $ TmApp fi v1 t2'
eval1 ctx (TmApp fi t1 t2) = do
  t1' <- eval1 ctx t1
  return $ TmApp fi t1' t2
eval1 _ _ = Nothing

eval ctx t = case eval1 ctx t of
  Just t1 -> if t == t1 then t else eval ctx t1
  Nothing -> t
