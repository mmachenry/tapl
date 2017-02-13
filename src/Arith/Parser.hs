module Arith.Parser where

import Arith.Ast
import Text.Parsec
import Text.Parsec.Language
import Text.Parsec.Char (char, string)
import Text.Parsec.String as TPS
import qualified Text.ParserCombinators.Parsec.Token as Token

parseFromFile = TPS.parseFromFile program

languageDef = emptyDef {
  Token.reservedNames = [
    "true", "fale", "if", "then", "else", "z", "succ", "pred", "iszero" ]
  }

lexer = Token.makeTokenParser languageDef
reserved = Token.reserved lexer
whiteSpace = Token.whiteSpace lexer
semi = Token.semi lexer
parens = Token.parens lexer

program = whiteSpace *> terms
terms = sepBy1 term semi

term =
      parens term
  <|> (reserved "true" *> pure TmTrue)
  <|> (reserved "false" *> pure TmFalse)
  <|> ifThenElse
  <|> (reserved "z" *> pure TmZero)
  <|> (TmSucc <$> (reserved "succ" *> term))
  <|> (TmPred <$> (reserved "pred" *> term))
  <|> (TmIsZero <$> (reserved "iszero" *> term))

ifThenElse =
  TmIf <$> (reserved "if" *> term)
       <*> (reserved "then" *> term)
       <*> (reserved "else" *> term)

