module CoreSpec (spec) where

import Test.Hspec
import Syntax
import Core

spec :: Spec
spec = do
  describe "eval" $ do
    it "evaluates zero" $ eval TmZero `shouldBe` TmZero
    it "evaluates true" $ eval TmTrue `shouldBe` TmTrue
    it "evaluates an if" $
      eval (TmIf (TmIsZero (TmPred (TmSucc TmZero)))
                 (TmSucc (TmPred (TmSucc TmZero)))
                 TmZero) `shouldBe` TmSucc TmZero
    it "evaluates (succ (pred 0))" $
      eval (TmSucc (TmPred TmZero)) `shouldBe` TmSucc TmZero
