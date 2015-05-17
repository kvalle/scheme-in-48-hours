module Main where

import Text.ParserCombinators.Parsec hiding (spaces)
import System.Environment

import Parser

main :: IO ()
main = getArgs >>= putStrLn . readExpr . (!! 0)

readExpr :: String -> String
readExpr input = case parse parseExpr "lisp" input of
    Left err  -> "Error: " ++ show err
    Right val -> show val
