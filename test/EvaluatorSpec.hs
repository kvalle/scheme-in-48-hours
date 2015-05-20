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
            Character '(' `shouldEvalAs` Character '('

    describe "evaluating quotes" $ do
        it "should evaluate to the quoted value" $ do
            List [Atom "quote", Atom "foo"] `shouldEvalAs` Atom "foo"
        it "should not evaluate the quoted value" $ do
            readExpr "''foo" `shouldEvalAs` readExpr "'foo"

    describe "evaluating calls to primitive functions" $ do
        it "should work with integers" $ do
            readExpr "(+ 2 2)" `shouldEvalAs` Integer 4
            readExpr "(+ 2 (- 4 1))" `shouldEvalAs` Integer 5
            readExpr "(- (+ 4 6 3) 3 5 2)" `shouldEvalAs` Integer 3
        it "should work with radixes" $ do
            readExpr "(+ #b101 #xA)" `shouldEvalAs` readExpr "15"

    describe "evaluating boolean type-check primitive" $ do
        it "should return #t if input evaluates to either #t or #f" $ do
            readExpr "(boolean? #t)" `shouldEvalAs` Bool True
            readExpr "(boolean? #f)" `shouldEvalAs` Bool True
            readExpr "(boolean? '#f)" `shouldEvalAs` Bool True
        it "should return #f otherwise" $ do
            readExpr "(boolean? 'nil)" `shouldEvalAs` Bool False
            readExpr "(boolean? '())" `shouldEvalAs` Bool False
            readExpr "(boolean? 123)" `shouldEvalAs` Bool False

    describe "evaluating numeric type-check primitives" $ do
        it "should be number if its either an integer or a real" $ do
            readExpr "(number? 3.14)" `shouldEvalAs` Bool True
            readExpr "(number? 3)" `shouldEvalAs` Bool True
            readExpr "(number? '#t)" `shouldEvalAs` Bool False
            readExpr "(number? 'foo)" `shouldEvalAs` Bool False
        it "should be a real number if its either an integer or a real" $ do
            readExpr "(real? 3.14)" `shouldEvalAs` Bool True
            readExpr "(real? 3)" `shouldEvalAs` Bool True
            readExpr "(real? '#t)" `shouldEvalAs` Bool False
            readExpr "(real? 'foo)" `shouldEvalAs` Bool False
        it "should be integer only if it's an integer" $ do
            readExpr "(integer? 3)" `shouldEvalAs` Bool True
            readExpr "(integer? 3.14)" `shouldEvalAs` Bool False
            readExpr "(integer? '#t)" `shouldEvalAs` Bool False
            readExpr "(integer? 'foo)" `shouldEvalAs` Bool False

    describe "evaluating list type check primitive" $ do
        it "should be #t if given a list (duh)" $ do
            readExpr "(list? '())" `shouldEvalAs` readExpr "#t"
            readExpr "(list? '(1 2 3))" `shouldEvalAs` readExpr "#t"
        it "should evaluate to #f if its an improper (dotted) list" $ do
            readExpr "(list? '(foo . bar))" `shouldEvalAs` readExpr "#f"
            readExpr "(list? '(. unf))" `shouldEvalAs` readExpr "#f"
        it "should also evaluate to #f for other stuff" $ do
            readExpr "(list? 'foo)" `shouldEvalAs` readExpr "#f"
            readExpr "(list? 123)" `shouldEvalAs` readExpr "#f"
            readExpr "(list? #\\space)" `shouldEvalAs` readExpr "#f"

    describe "evaluating pair type check primitive" $ do
        it "should not be a pair if its an empty list" $ do
            readExpr "(pair? '())" `shouldEvalAs` readExpr "#f"
        it "should be a pair if it is a list of at least one element" $ do
            readExpr "(pair? '(1))" `shouldEvalAs` readExpr "#t"
            readExpr "(pair? '(1 2 3))" `shouldEvalAs` readExpr "#t"
            readExpr "(pair? '(foo . bar))" `shouldEvalAs` readExpr "#t"
            readExpr "(pair? '(. bar))" `shouldEvalAs` readExpr "#t"
        it "should evaluate to #f for other stuff" $ do
            readExpr "(pair? 'foo)" `shouldEvalAs` readExpr "#f"
            readExpr "(pair? 123)" `shouldEvalAs` readExpr "#f"
            readExpr "(pair? #\\space)" `shouldEvalAs` readExpr "#f"

    describe "evaluating vector type check primitive" $ do
        it "should only be #t if given a vector" $ do
            readExpr "(vector? #())" `shouldEvalAs` readExpr "#t"
            readExpr "(vector? #(1 2 3))" `shouldEvalAs` readExpr "#t"
            readExpr "(vector? '#(1 2 3))" `shouldEvalAs` readExpr "#t"
            readExpr "(vector? '())" `shouldEvalAs` readExpr "#f"
            readExpr "(vector? '(1 2 3))" `shouldEvalAs` readExpr "#f"
            readExpr "(vector? '(foo . bar))" `shouldEvalAs` readExpr "#f"
            readExpr "(vector? '(. unf))" `shouldEvalAs` readExpr "#f"
            readExpr "(vector? 'foo)" `shouldEvalAs` readExpr "#f"
            readExpr "(vector? 123)" `shouldEvalAs` readExpr "#f"
            readExpr "(vector? #\\space)" `shouldEvalAs` readExpr "#f"

    describe "evaluating string type check primitive" $ do
        it "should should only return #t for strings" $ do
            readExpr "(string? \"herro!\")" `shouldEvalAs` readExpr "#t"
            readExpr "(string? '(\"hi\" \"there\"))" `shouldEvalAs` readExpr "#f"
            readExpr "(string? 'hello)" `shouldEvalAs` readExpr "#f"
            readExpr "(string? 12345)" `shouldEvalAs` readExpr "#f"

    describe "evaluating char type check primitive" $ do
        it "should should only return #t for characters" $ do
            readExpr "(char? #\\x)" `shouldEvalAs` readExpr "#t"
            readExpr "(char? #\\space)" `shouldEvalAs` readExpr "#t"
            readExpr "(char? \"herro!\")" `shouldEvalAs` readExpr "#f"
            readExpr "(char? '(1 2 3))" `shouldEvalAs` readExpr "#f"
            readExpr "(char? 'hello)" `shouldEvalAs` readExpr "#f"
            readExpr "(char? 12345)" `shouldEvalAs` readExpr "#f"


-- Test helpers

shouldEvalAs :: LispVal -> LispVal -> Expectation
input `shouldEvalAs` result = 
    eval input `shouldBe` result

