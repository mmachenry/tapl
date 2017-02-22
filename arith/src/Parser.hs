module Parser where

import Syntax
import Text.Parsec
import Text.Parsec.Language
import Text.Parsec.String as TPS
import qualified Text.ParserCombinators.Parsec.Token as Token

parseFromFile = TPS.parseFromFile program
parseFromString = parse program ""

languageDef = emptyDef {
  Token.reservedNames = [
    "true", "fale", "if", "then", "else", "succ", "pred", "iszero" ]
  }

lexer = Token.makeTokenParser languageDef
reserved = Token.reserved lexer
whiteSpace = Token.whiteSpace lexer
semi = Token.semi lexer
parens = Token.parens lexer
symbol = Token.symbol lexer

program = whiteSpace *> terms
terms = sepBy1 term semi

term =
      parens term
  <|> (reserved "true" *> pure TmTrue)
  <|> (reserved "false" *> pure TmFalse)
  <|> ifThenElse
  <|> (symbol "0" *> pure TmZero)
  <|> (TmSucc <$> (reserved "succ" *> term))
  <|> (TmPred <$> (reserved "pred" *> term))
  <|> (TmIsZero <$> (reserved "iszero" *> term))

ifThenElse =
  TmIf <$> (reserved "if" *> term)
       <*> (reserved "then" *> term)
       <*> (reserved "else" *> term)

