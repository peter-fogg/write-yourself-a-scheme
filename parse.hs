import Text.ParserCombinators.Parsec hiding (spaces)
import System.Environment
import Control.Monad
import Numeric

data LispVal = Atom String
             | List [LispVal]
             | DottedList [LispVal] LispVal
             | Number Integer
             | Float Double
             | Complex Double Double
             | String String
             | Character Char
             | Bool Bool
               deriving Show

parseEscape :: Parser Char
parseEscape = char '\\' >> choice (zipWith escapedChar codes replacements)

escapedChar :: Char -> Char -> Parser Char
escapedChar code replacement = char code >> return replacement

codes = ['b', 'n', 'f', 'r', 't', '\\', '\"', '/']
replacements = ['\b', '\n', '\f', '\r', '\t', '\\', '\"', '/']

parseString :: Parser LispVal
parseString = do char '"'
                 x <- many (choice [parseEscape, noneOf "\""])
                 char '"'
                 return $ String x

parseCharacter :: Parser LispVal
parseCharacter = do char '\''
                    x <- choice [parseEscape, noneOf "\'"]
                    char '\''
                    return $ Character x

parseAtom :: Parser LispVal
parseAtom = do first <- letter <|> symbol
               rest <- many (letter <|> digit <|> symbol)
               let atom = first:rest
               return $ case atom of
                 "#t" -> Bool True
                 "#f" -> Bool False
                 _    -> Atom atom

parseOct :: Parser LispVal
parseOct = do string "#o"
              x <- many octDigit
              return $ Number (fst ((readOct x) !! 0))

parseHex :: Parser LispVal
parseHex = do string "#h"
              x <- many hexDigit
              return $ Number (fst ((readHex x) !! 0))

parseDecimal :: Parser LispVal
parseDecimal = do x <- many digit
                  return $ Number (read x)

parseNumber :: Parser LispVal
parseNumber =  (try parseHex) <|> (try parseOct) <|> parseDecimal
-- parseNumber = liftM (Number . read) $ many1 digit
-- parseNumber = do digits <- many1 digit
--                  return $ Number $ read digits
-- parseNumber = (many1 digit) >>= \s -> return $ (Number . read) $ s

parseFloatingString :: Parser String
parseFloatingString = do integral <- many1 digit
                         char '.'
                         fractional <- many1 digit
                         return (integral ++ "." ++ fractional)

parseFloat :: Parser LispVal
parseFloat = do x <- parseFloatingString
                return $ Float (getValue (readFloat x))
                  where getValue ((x, y):xs) = x

parseComplex :: Parser LispVal
parseComplex = do real <- choice [try parseFloatingString, many1 digit]
                  char '+'
                  imaginary <- choice [try parseFloatingString, many1 digit]
                  char 'i'
                  return $ Complex (getValue (readFloat real)) (getValue (readFloat imaginary))
                    where getValue ((x, y):xs) = x

parseExpr :: Parser LispVal
parseExpr = (try parseComplex) <|> (try parseFloat) <|> parseNumber <|> parseAtom <|> parseString <|> parseCharacter
               
symbol :: Parser Char
symbol = oneOf "!#$%&|*+-/:<=>?@^_~"
         
readExpr :: String -> String
readExpr input = case parse parseExpr "lisp" input of
  Left err -> "No match: " ++ show err
  Right val -> "Found value " ++ show val

spaces :: Parser ()
spaces = skipMany1 space

main :: IO ()
main = do
  args <- getArgs
  putStrLn $ readExpr $ args !! 0