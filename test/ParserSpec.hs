module ParserSpec where

import Test.Hspec
import Text.ParserCombinators.Parsec hiding (spaces)
import Parser

input `shouldParseAs` result = 
    (parse parseExpr "test" input) `shouldBe` Right result

spec :: Spec
spec = do
    describe "parsing booleans" $ do
        it "should parse #t as true" $
            "#t" `shouldParseAs` Bool True
        it "should parse #f as false" $
            "#f" `shouldParseAs` Bool False
    
    describe "parsing atoms" $ do
        it "should parse letters as atoms" $
            "foobar" `shouldParseAs` Atom "foobar"
        it "should parse atoms starting with symbols" $
            "@lpha" `shouldParseAs` Atom "@lpha"
        it "should parse atoms containing digits" $ 
            "h3ll0" `shouldParseAs` Atom "h3ll0"

    describe "parsing quotes" $ do
        it "should parse quoted atoms" $
            "'foo" `shouldParseAs` List [Atom "quote", Atom "foo"]
        it "should parse quoted nested expressions" $
            "'(foo 'bar)" `shouldParseAs` 
                List [Atom "quote", List [Atom "foo", List [Atom "quote", Atom "bar"]]]

    describe "parsing unquotes" $ do
        it "should parse unquoted atoms" $
            ",foo" `shouldParseAs` List [Atom "unquote", Atom "foo"]

    describe "parsing quasiquotes" $ do
        it "should parse quasiquoted atoms" $
            "`foo" `shouldParseAs` List [Atom "quasiquote", Atom "foo"]

    describe "parsing strings" $ do
        it "should parse strings started and ended with double quotes" $
            "\"hey\"" `shouldParseAs` String "hey"
        it "should handle strings with escaped quotes" $
            -- parsing: "say \"hey\""
            "\"say \\\"hey\\\"\"" `shouldParseAs` String "say \"hey\""

    describe "parsing floats" $ do
        it "should parse numbers with decimal point as floats" $ do
            "42.0" `shouldParseAs` Float 42.0
            "3.1415" `shouldParseAs` Float 3.1415
