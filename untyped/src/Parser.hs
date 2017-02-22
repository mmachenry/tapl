module Parser where

import Syntax
import Text.Parsec
import Text.Parsec.Language
import qualified Text.Parsec.String as TPS
import qualified Text.ParserCombinators.Parsec.Token as Token

parseFromFile = TPS.parseFromFile program

languageDef = emptyDef {
  Token.reservedNames = ["lambda"],
  Token.reservedOpNames = ["."]
  }

lexer = Token.makeTokenParser languageDef
reserved = Token.reserved lexer
whiteSpace = Token.whiteSpace lexer
semi = Token.semi lexer
parens = Token.parens lexer
symbol = Token.symbol lexer
identifier = Token.identifier lexer

program = whiteSpace *> terms
terms = sepBy1 term semi

term = do
  reserved "lambda"
  i1 <- identifier
  reserved "."
  i2 <- identifier  
  return (i1,i2)
