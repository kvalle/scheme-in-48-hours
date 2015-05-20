module Evaluator where

import Debug.Trace

import AST

eval :: LispVal -> LispVal
eval val@(String _) = val
eval val@(Integer _) = val
eval val@(Real _) = val
eval val@(Bool _) = val
eval val@(Vector _) = val
eval val@(Character _) = val
eval (List [Atom "quote", val]) = val
eval (List (Atom func : args)) = apply func $ map eval args

apply :: String -> [LispVal] -> LispVal
apply func args = maybe (Atom "error") ($ args) $ lookup func primitives

primitives :: [(String, [LispVal] -> LispVal)]
primitives = [("+", numericBinop (+)),
              ("-", numericBinop (-)),
              ("*", numericBinop (*)),
              ("/", numericBinop div),
              ("mod", numericBinop mod),
              ("quotient", numericBinop quot),
              ("remainder", numericBinop rem),
              ("list?", isList),
              ("pair?", isPair),
              ("vector?", isVector),
              ("string?", isString),
              ("char?", isChar),
              ("number?", isNumber),
              ("real?", isReal),
              ("integer?", isInteger),
              ("boolean?", isBoolean)]

numericBinop :: (Integer -> Integer -> Integer) -> [LispVal] -> LispVal
numericBinop op params = Integer $ foldl1 op $ map unpackNum params
 
unpackNum :: LispVal -> Integer
unpackNum (Integer n) = n
unpackNum _ = 0

isList :: [LispVal] -> LispVal
isList [List _] = Bool True
isList _        = Bool False

isVector :: [LispVal] -> LispVal
isVector [Vector _] = Bool True
isVector _          = Bool False

isPair :: [LispVal] -> LispVal
isPair [List []]        = Bool False
isPair [List _]         = Bool True
isPair [DottedList _ _] = Bool True
isPair _                = Bool False

isBoolean :: [LispVal] -> LispVal
isBoolean [Bool _] = Bool True
isBoolean _        = Bool False

isString :: [LispVal] -> LispVal
isString [String _] = Bool True
isString _          = Bool False

isChar :: [LispVal] -> LispVal
isChar [Character _] = Bool True
isChar _             = Bool False


isInteger :: [LispVal] -> LispVal
isInteger [Integer _] = Bool True
isInteger _           = Bool False

isReal :: [LispVal] -> LispVal
isReal [Real _]    = Bool True
isReal [Integer _] = Bool True
isReal _           = Bool False

isNumber :: [LispVal] -> LispVal
-- Without 'complex' and 'fractional' being a number and a real is the same thing 
isNumber = isReal
