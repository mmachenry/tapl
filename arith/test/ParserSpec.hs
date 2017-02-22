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
