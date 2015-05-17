module ParserSpec where

import Data.Array
import Test.Hspec
import Test.Hspec.QuickCheck (prop)
import Text.ParserCombinators.Parsec hiding (spaces, parse)
import qualified Text.ParserCombinators.Parsec as P

import Parser

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
                parse (show n) == parse ("#d" ++ (show n))) :: Integer -> Bool)

    describe "parsing vectors" $ do
        it "should parse vectors" $
            "#(foo #b101)" `shouldParseAs` Vector (listArray (0,1) [Atom "foo", Number 5])
        it "should parse an empty vector" $
            "#()" `shouldParseAs` Vector (listArray (0,-1) [])
        it "should ignore spaces at beginning and end" $
            "#(  1 2 3  )" `shouldParseAs` Vector (listArray (0,2) [Number 1, Number 2, Number 3])

    describe "parsing characters" $ do
        it "should parse hash+backslash followed by char as Character" $ do
            "#\\x" `shouldParseAs` Character 'x'
            "#\\λ" `shouldParseAs` Character 'λ'
            "#\\ " `shouldParseAs` Character ' '
        it "should parse named characters" $ do
            "#\\space" `shouldParseAs` Character ' '
            "#\\newline" `shouldParseAs` Character '\n'

    describe "parsing lists" $ do
        it "should parse empty lists" $
            "()" `shouldParseAs` List []
        it "should parse proper lists with items" $ do
            "(1 2 3)" `shouldParseAs` List [Number 1, Number 2, Number 3]
            "(1 2 3)" `shouldParseAs` List [Number 1, Number 2, Number 3]
            "(foo 'bar)" `shouldParseAs` List [Atom "foo", List [Atom "quote", Atom "bar"]]
        it "should parse dotted lists" $ do
            "(1 2 . 3)" `shouldParseAs` DottedList [Number 1, Number 2] (Number 3)
            "(foo . 'bar)" `shouldParseAs` DottedList [Atom "foo"] (List [Atom "quote", Atom "bar"])
            "(. baz)" `shouldParseAs` DottedList [] (Atom "baz")
        it "should parse nested lists" $
            "(1 (42 . (foo bar)) #f)" `shouldParseAs`
                List [Number 1, DottedList [Number 42] (List [Atom "foo", Atom "bar"]), Bool False]
        it "should ignore spaces at beginning and end" $ do
            "(  1 2 3  )" `shouldParseAs` List [Number 1, Number 2, Number 3]
            "(  1 2 . 3  )" `shouldParseAs` DottedList [Number 1, Number 2] (Number 3)


-- Test helpers

parse :: String -> Either ParseError LispVal
parse = P.parse parseExpr "test"

shouldParseAs :: String -> LispVal -> Expectation
input `shouldParseAs` result = 
    (parse input) `shouldBe` Right result
