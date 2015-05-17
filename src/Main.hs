module Main where

import System.Environment

import AST
import Evaluator
import Parser

main :: IO ()
main = getArgs >>= print . eval . readExpr . head 
