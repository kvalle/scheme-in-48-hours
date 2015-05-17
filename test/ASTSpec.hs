module ASTSpec where

import Data.Array
import Test.Hspec

import AST

spec :: Spec
spec = do
    describe "showing parsed values" $ do
        it "should show booleans" $ do
            show (Bool True) `shouldBe` "#t"
            show (Bool False) `shouldBe` "#f"
        it "should show atoms" $ do
            show (Atom "foo") `shouldBe` "foo"
        it "should show integers" $ do
            show (Integer 42) `shouldBe` "42"
            show (Integer (-42)) `shouldBe` "-42"
        it "should show strings" $ do
            show (String "foobar") `shouldBe` "\"foobar\""
            show (String "foo\"bar") `shouldBe` "\"foo\"bar\""
        it "should show reals" $ do
            show (Real 3.1415) `shouldBe` "3.1415"
            show (Real 42.0) `shouldBe` "42.0"
        it "should show characters" $ do
            show (Character ' ') `shouldBe` "#\\ "
            show (Character '\n') `shouldBe` "#\\\n"
            show (Character '(') `shouldBe` "#\\("
            show (Character 'x') `shouldBe` "#\\x"
        it "should show quoted values" $ do
            show (List [Atom "quote", Atom "foo"]) `shouldBe` "'foo"
            show (List [Atom "quote", List [Integer 1, Integer 2]]) `shouldBe` "'(1 2)"
        it "should show unquoted values" $ do
            show (List [Atom "unquote", Atom "foo"]) `shouldBe` ",foo"
            show (List [Atom "unquote", List [Integer 1, Integer 2]]) `shouldBe` ",(1 2)"
        it "should show quasiquoted values" $ do
            show (List [Atom "quasiquote", Atom "foo"]) `shouldBe` "`foo"
            show (List [Atom "quasiquote", List [Integer 1, Integer 2]]) `shouldBe` "`(1 2)"
        it "should show lists" $ do
            show (List [Atom "foo", Atom "bar"]) `shouldBe` "(foo bar)"
            show (List [Atom "foo", List [Atom "bar", Atom "baz"]]) `shouldBe` "(foo (bar baz))"
        it "should show dottedLists" $ do
            show (DottedList [Atom "foo"] (Atom "bar")) `shouldBe` "(foo . bar)"
            show (DottedList [Atom "foo"] (DottedList [Atom "bar"] (Atom "baz"))) `shouldBe` "(foo . (bar . baz))"
        it "should show vectors" $ do
            show (Vector (listArray (0,1) [Atom "foo", Atom "bar"])) `shouldBe` "#(foo bar)"
            show (Vector (listArray (0,1) [Atom "foo", List [Atom "bar", Atom "baz"]])) `shouldBe` "#(foo (bar baz))"
