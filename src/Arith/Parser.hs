module Arith.Parser where

import Arith.Ast
import Text.Parsec
import Text.Parsec.Char (char, string)
import Text.Parsec.ByteString as TPBS

parseFromFile = TPBS.parseFromFile pterms

pterms = sepBy pterm (char ';')

pterm =
      ptrue
  <|> pfalse
  <|> pifThenElse
  <|> pzero
  <|> psucc
  <|> ppred
  <|> piszero

ptrue = string "true" *> pure TmTrue
pfalse = string "false" *> pure TmFalse
pifThenElse =
  TmIf <$> (string "if" *> pterm)
       <*> (string "then" *> pterm)
       <*> (string "else" *> pterm)
pzero = string "0" *> pure TmZero
psucc = TmSucc <$> (string "succ" *> pterm)
ppred = TmPred <$> (string "pred" *> pterm)
piszero = TmIsZero <$> (string "iszero" *> pterm)

