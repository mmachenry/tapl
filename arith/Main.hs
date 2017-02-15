module Main where

import System.Environment (getArgs)
import Arith.Eval (eval)
import Arith.Parser (parseFromFile)

main :: IO ()
main = do
  [filename] <- getArgs
  result <- parseFromFile filename
  case result of
    Left e -> print e
    Right ts -> mapM_ (print . eval) ts

