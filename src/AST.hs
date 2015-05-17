module AST where

import Data.Array

data LispVal = Atom String
             | Integer Integer
             | Real Float
             | String String
             | Bool Bool
             | Character Char
             | List [LispVal]
             | DottedList [LispVal] LispVal
             | Vector (Array Int LispVal)
            deriving (Eq)

instance Show LispVal where
    show (String contents) = "\"" ++ contents ++ "\""
    show (Atom name) = name
    show (Integer contents) = show contents
    show (Real contents) = show contents
    show (Character c) = "#\\" ++ [c]
    show (Bool True) = "#t"
    show (Bool False) = "#f"
    show (List [Atom "quote", val]) = "'" ++ show val
    show (List [Atom "unquote", val]) = "," ++ show val
    show (List [Atom "quasiquote", val]) = "`" ++ show val
    show (List contents) = "(" ++ unwordsList contents ++ ")"
    show (DottedList head tail) = "(" ++ unwordsList head ++ " . " ++ show tail ++ ")"
    show (Vector contents) = "#(" ++ unwordsList (elems contents) ++ ")"

unwordsList :: [LispVal] -> String
unwordsList = unwords . map show
