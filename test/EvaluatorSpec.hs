module EvaluatorSpec where

import Test.Hspec

import AST
import Evaluator
import Parser

spec :: Spec
spec = do
    describe "evaluating basic types (bools, strings, numbers)" $ do
        it "should evaluate to itself" $ do
            Bool True `shouldEvalAs` Bool True
            Bool False `shouldEvalAs` Bool False
            String "foobar" `shouldEvalAs` String "foobar"
            Number 42 `shouldEvalAs` Number 42
            Float 3.1415 `shouldEvalAs` Float 3.1415

    describe "evaluating quotes" $ do
        it "should evaluate to the quoted value" $ do
            List [Atom "quote", Atom "foo"] `shouldEvalAs` Atom "foo"
        it "should not evaluate the quoted value" $ do
            readExpr "''foo" `shouldEvalAs` readExpr "'foo"


-- Test helpers

shouldEvalAs :: LispVal -> LispVal -> Expectation
input `shouldEvalAs` result = 
    eval input `shouldBe` result

