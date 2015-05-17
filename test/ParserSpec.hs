module ParserSpec where

import Test.Hspec
import Test.Hspec.QuickCheck (prop)
import Text.ParserCombinators.Parsec hiding (spaces)
import Parser

p :: String -> Either ParseError LispVal
p = parse parseExpr "test"

shouldParseAs :: String -> LispVal -> Expectation
input `shouldParseAs` result = 
    (p input) `shouldBe` Right result

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

    describe "parsing numbers" $ do
        it "should parse regular numbers as decimals" $ do
            "42" `shouldParseAs` Number 42
            "3" `shouldParseAs` Number 3
        it "should parse numbers with explicit decimal radix" $ do
            "#d42" `shouldParseAs` Number 42
            "#d3" `shouldParseAs` Number 3
        it "should parse binary numbers" $ do
            "#b101010" `shouldParseAs` Number 42
            "#b11" `shouldParseAs` Number 3
        it "should parse octal numbers" $ do
            "#o52" `shouldParseAs` Number 42
            "#o3" `shouldParseAs` Number 3
        it "should parse hexadecimal numbers" $ do
            "#x2A" `shouldParseAs` Number 42
            "#x3" `shouldParseAs` Number 3
        it "should parse negative numbers" $ do
            "-42" `shouldParseAs` Number (-42)
            "#b-101010" `shouldParseAs` Number (-42)
            "#o-52" `shouldParseAs` Number (-42)
            "#d-42" `shouldParseAs` Number (-42)
            "#x-2A" `shouldParseAs` Number (-42)
        prop "numbers without radix are treated as decimals" $ 
            ((\n -> 
                p (show n) == p ("#d" ++ (show n))) :: Integer -> Bool)

    -- TODO
    --describe "parsing vectors" $ do
    --describe "parsing characters" $ do
    --describe "parsing lists" $ do
