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
            Integer 42 `shouldEvalAs` Integer 42
            Real 3.1415 `shouldEvalAs` Real 3.1415

    describe "evaluating quotes" $ do
        it "should evaluate to the quoted value" $ do
            List [Atom "quote", Atom "foo"] `shouldEvalAs` Atom "foo"
        it "should not evaluate the quoted value" $ do
            readExpr "''foo" `shouldEvalAs` readExpr "'foo"

    describe "evaluating calls to primitive functions" $ do
        it "should do addition" $ do
            readExpr "(+ 2 2)" `shouldEvalAs` Integer 4
            readExpr "(+ 2 (- 4 1))" `shouldEvalAs` Integer 5
            readExpr "(- (+ 4 6 3) 3 5 2)" `shouldEvalAs` Integer 3

-- Test helpers

shouldEvalAs :: LispVal -> LispVal -> Expectation
input `shouldEvalAs` result = 
    eval input `shouldBe` result

