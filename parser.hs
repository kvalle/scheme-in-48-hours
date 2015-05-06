import Text.ParserCombinators.Parsec hiding (spaces)
import System.Environment
import Control.Monad

main :: IO ()
main = do 
    args <- getArgs
    let src = (args !! 0)
    putStrLn ("Source is:   " ++ src)
    putStrLn (readExpr src)

data LispVal = Atom String
             | List [LispVal]
             | DottedList [LispVal] LispVal
             | Number Integer
             | String String
             | Bool Bool
            deriving (Show)

symbol :: Parser Char
symbol = oneOf "!#$%&|*+-/:<=>?@^_~"

readExpr :: String -> String
readExpr input = case parse parseExpr "lisp" input of
    Left err -> "No match: " ++ show err
    Right val -> "Found value: " ++ show val

spaces :: Parser ()
spaces = skipMany1 space

parseExpr :: Parser LispVal
parseExpr = parseAtom
         <|> parseString
         <|> parseNumber

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

parseEscapedChar :: Parser Char
parseEscapedChar = char '\\' >> char '"' >> return '"'

parseStringChar :: Parser Char
parseStringChar = parseEscapedChar <|> (noneOf "\"")

parseNumber' :: Parser LispVal
parseNumber' = liftM (Number . read) $ many1 digit

parseNumber'' :: Parser LispVal
parseNumber'' = do
    digits <- many1 digit
    return $ (Number . read) digits     

parseNumber :: Parser LispVal
parseNumber = many1 digit >>= return . Number . read
