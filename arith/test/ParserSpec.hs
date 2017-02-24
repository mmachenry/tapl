module ParserSpec (spec) where

import Test.Hspec
import Text.Parsec (parse)
import Syntax
import Parser

spec :: Spec
spec = do
  describe "parser" $ do
    it "parses a zero" $
      parseFromString "0" `shouldBe` Right [TmZero]
    it "parses a true" $
      parseFromString "true" `shouldBe` Right [TmTrue]
    it "parses a false" $
      parseFromString "false" `shouldBe` Right [TmFalse]
    it "parses succ and pred" $
      parseFromString "succ (pred (succ 0))" `shouldBe`
        Right [TmSucc (TmPred (TmSucc TmZero))]
    it "parses iszero" $
      parseFromString "iszero 0" `shouldBe` Right [TmIsZero TmZero]
    it "parses if-then-else" $
      parseFromString "if iszero (succ 0) then succ 0 else succ (succ 0)"
        `shouldBe` Right [TmIf (TmIsZero (TmSucc TmZero))
                               (TmSucc TmZero)
                               (TmSucc (TmSucc TmZero))]
    it "parses multiple terms" $
      parseFromString "0;succ 0" `shouldBe` Right [TmZero, TmSucc TmZero]
