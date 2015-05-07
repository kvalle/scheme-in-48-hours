import Text.ParserCombinators.Parsec hiding (spaces)
import System.Environment
import Control.Monad
import Numeric

import Debug.Trace

main :: IO ()
main = do 
    args <- getArgs
    let src = (args !! 0)
    putStrLn ("Source is:   >>>" ++ src ++ "<<<")
    putStrLn (readExpr src)

data LispVal = Atom String
             | List [LispVal]
             | DottedList [LispVal] LispVal
             | Number Integer
             | Float Float
             | String String
             | Bool Bool
             | Character Char
            deriving (Show)

symbol :: Parser Char
symbol = oneOf "!$%&|*+-/:<=>?@^_~"

readExpr :: String -> String
readExpr input = case parse parseExpr "lisp" input of
    Left err -> "No match: " ++ show err
    Right val -> "Found value: >>>" ++ show val ++ "<<<"

spaces :: Parser ()
spaces = skipMany1 space

parseExpr :: Parser LispVal
parseExpr = parseAtom
         <|> try parseBool
         <|> try parseString
         <|> try parseFloat
         <|> try parseNumber
         <|> try parseCharacter

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
    let reader = case r of
            'b' -> readBin
            'o' -> fst . head . readOct
            'd' -> read
            'x' -> fst . head . readHex
    many1 digit >>= return . Number . reader

readBin :: String -> Integer
readBin = foldl1 (\acc digit -> acc * 2 + digit) . map toDigit
    where toDigit '1' = 1
          toDigit _   = 0

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
