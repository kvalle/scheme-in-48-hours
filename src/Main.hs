module Main where

import System.Environment
import Parser

main :: IO ()
main = getArgs >>= putStrLn . readExpr . (!! 0)
