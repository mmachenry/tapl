module Arith.Ast where

data Term =
    TmTrue Info
  | TmFalse Info
  | TmIf Info Term Term Term
  | TmZero Info
  | TmSucc Info Term
  | TmPred Info Term
  | TmIsZero Info Term
  deriving (Show)

data Info = Info
  deriving (Show)

dummyInfo = Info
