module Parser where

import Text.ParserCombinators.Parsec hiding (spaces)
import Data.Array
import Control.Monad
import Numeric

import Debug.Trace

data LispVal = Atom String
             | Number Integer
             | Float Float
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
    show (Number contents) = show contents
    show (Float contents) = show contents
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

readExpr :: String -> String
readExpr input = case parse parseExpr "lisp" input of
    Left err  -> "Error: " ++ show err
    Right val -> show val

parseExpr :: Parser LispVal
parseExpr = try parseBool
         <|> try parseQuote
         <|> try parseUnquote
         <|> try parseQuasiquote
         <|> try parseString
         <|> try parseFloat
         <|> try parseNumber
         <|> try parseAtom
         <|> try parseVector
         <|> try parseCharacter
         <|> parseList

parseList :: Parser LispVal
parseList = do
        char '('
        optionalSpaces
        exps <- sepEndBy parseExpr spaces
        end <- parseListEnd
        optionalSpaces
        char ')'
        return $ case end of
            Just e  -> DottedList exps e
            Nothing -> List exps
    where
        parseListEnd = (char '.' >> spaces >> parseExpr >>= return . Just) 
                    <|> return Nothing

parseVector :: Parser LispVal
parseVector = do
    string "#("
    optionalSpaces
    arrayValues <- sepEndBy parseExpr spaces
    char ')'
    return $ Vector $ listArray (0, length arrayValues - 1) arrayValues

symbol :: Parser Char
symbol = oneOf "!$%&|*+-/:<=>?@^_~"

optionalSpaces :: Parser ()
optionalSpaces = skipMany space

spaces :: Parser ()
spaces = skipMany1 space

parseBool :: Parser LispVal
parseBool = true <|> false 
    where true  = try (string "#f") >> return (Bool False)
          false = try (string "#t") >> return (Bool True)

parseAtom :: Parser LispVal
parseAtom = do 
              first <- letter <|> symbol
              rest <- many (letter <|> digit <|> symbol)
              let atom = first:rest
              return $ case atom of 
                         "#t" -> Bool True
                         "#f" -> Bool False
                         _    -> Atom atom

parseString :: Parser LispVal
parseString = do
                char '"'
                x <- many parseStringChar
                char '"'
                return $ String x

parseStringChar :: Parser Char
parseStringChar = parseEscapedChar <|> (noneOf "\"")

parseEscapedChar :: Parser Char
parseEscapedChar = do
    char '\\'
    x <- oneOf "\\\"nrt"
    return $ case x of
                '"'  -> x
                '\\' -> x
                _    -> ' '

parseNumber :: Parser LispVal
parseNumber = do
    r <- parseNumberRadix
    sign <- parseSign
    let reader = case r of
            'b' -> readBin
            'o' -> fst . head . readOct
            'd' -> read
            'x' -> fst . head . readHex
    let parseDigits = case r of
            'b' -> many1 (oneOf "01")
            'o' -> many1 (oneOf "01234567")
            'd' -> many1 digit
            'x' -> many1 (oneOf $ ['0'..'9'] ++ ['A'..'F'] ++ ['a'..'f'])
    number <- parseDigits >>= return . reader
    return $ case sign of
        Just '-'  -> Number (-number)
        otherwise -> Number number

readBin :: String -> Integer
readBin = foldl1 (\acc digit -> acc * 2 + digit) . map toDigit
    where toDigit '1' = 1
          toDigit _   = 0

parseSign :: Parser (Maybe Char)
parseSign = try (oneOf "+-" >>= return . Just) 
         <|> return Nothing

parseNumberRadix :: Parser Char
parseNumberRadix = (try (char '#' >> oneOf "bodx")) <|> return 'd'

parseFloat :: Parser LispVal
parseFloat = do
    d1 <- many1 digit
    char '.'
    d2 <- many1 digit
    return $ Float $ fst . head . readFloat $ d1 ++ ['.'] ++ d2

parseCharacter :: Parser LispVal
parseCharacter = 
    string "#\\" >> (try parseCharacterSingle <|> parseCharacterNamed) >>= return . Character

parseCharacterNamed :: Parser Char
parseCharacterNamed = do
    s <- (string "newline" <|> string "space")
    case s of 
        "newline" -> return '\n'
        "space"   -> return ' '

parseCharacterSingle :: Parser Char
parseCharacterSingle = do 
    x <- anyChar
    notFollowedBy alphaNum
    return x

parseQuote :: Parser LispVal
parseQuote = do
    char '\''
    x <- parseExpr
    return $ List [Atom "quote", x]

parseUnquote :: Parser LispVal
parseUnquote = do
    char ','
    x <- parseExpr
    return $ List [Atom "unquote", x]

parseQuasiquote :: Parser LispVal
parseQuasiquote = do
    char '`'
    x <- parseExpr
    return $ List [Atom "quasiquote", x]
