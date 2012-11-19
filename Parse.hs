-- module Parse
--        ( LispVal
--        , String
--        , parseExpr
--        , readExpr
--        ) where

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

showVal :: LispVal -> String
showVal (String contents) = "\"" ++ contents ++ "\""
showVal (Atom name) = name
showVal (Number value) = show value
showVal (Float value) = show value
showVal (Complex real imaginary) = (show real) ++ "+" ++ (show imaginary) ++ "i"
showVal (Character char) = "'" ++ show char ++ "'"
showVal (Bool True) = "#t"
showVal (Bool False) = "#f"
showVal (List contents) = "(" ++ unwordsList contents ++ ")"
showVal (DottedList head tail) = "(" ++ unwordsList head ++ " . " ++ showVal tail ++ ")"

instance Show LispVal where show = showVal

unwordsList :: [LispVal] -> String
unwordsList = unwords . map showVal

eval :: LispVal -> LispVal
eval val@(String _) = val
eval val@(Character _) = val
eval val@(Bool _) = val
eval val@(Number _) = val
eval val@(Float _) = val
eval val@(Complex _ _) = val
eval val@(Atom _) = val
eval (List [Atom "quote", val]) = val
eval (List (Atom func : args)) = apply func $ map eval args

apply :: String -> [LispVal] -> LispVal
apply func args = maybe (Bool False) ($ args) $ lookup func primitives

numericBinop :: (Integer -> Integer -> Integer) -> [LispVal] -> LispVal
numericBinop op params = Number $ foldl1 op $ map unpackNum params

unpackNum :: LispVal -> Integer
unpackNum (Number n) = n
unpackNum _ = 0

isNumber :: [LispVal] -> LispVal
isNumber [(Number _)] = Bool True
isNumber _ = Bool False

isBool :: [LispVal] -> LispVal
isBool [(Bool _)] = Bool True
isBool _ = Bool False

isString :: [LispVal] -> LispVal
isString [(String _)] = Bool True
isString _ = Bool False

isAtom :: [LispVal] -> LispVal
isAtom [(Atom _)] = Bool True
isAtom _ = Bool False

isCharacter :: [LispVal] -> LispVal
isCharacter [(Character _)] = Bool True
isCharacter _ = Bool False

isComplex :: [LispVal] -> LispVal
isComplex [(Complex _ _)] = Bool True
isComplex _ = Bool False

isFloat :: [LispVal] -> LispVal
isFloat [(Float _)] = Bool True
isFloat _ = Bool False

isList :: [LispVal] -> LispVal
isList [(List _)] = Bool True
isList _ = Bool False

symbolToString :: [LispVal] -> LispVal
symbolToString [(Atom val)] = String val
symbolToString _ = Bool False

stringToSymbol :: [LispVal] -> LispVal
stringToSymbol [(String contents)] = Atom contents
stringToSymbol _ = Bool False

primitives :: [(String, [LispVal] -> LispVal)]
primitives = [("+", numericBinop (+)),
              ("-", numericBinop (-)),
              ("*", numericBinop (*)),
              ("/", numericBinop div),
              ("mod", numericBinop mod),
              ("quotient", numericBinop quot),
              ("remainder", numericBinop rem),
              ("bool?", isBool),
              ("atom?", isAtom),
              ("string?", isString),
              ("character?", isCharacter),
              ("complex?", isComplex),
              ("float?", isFloat),
              ("list?", isList),
              ("number?", isNumber),
              ("string->symbol", stringToSymbol),
              ("symbol->string", symbolToString)]

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
parseComplex = do real <- choice [try parseFloatingString, many digit]
                  char '+'
                  imaginary <- choice [try parseFloatingString, many1 digit]
                  char 'i'
                  return $ Complex (getValue (readFloat real)) (getValue (readFloat imaginary))
                    where getValue ((x, y):xs) = x

parseList :: Parser LispVal
parseList = liftM List $ sepBy parseExpr spaces

parseDottedList :: Parser LispVal
parseDottedList = do
  head <- endBy parseExpr spaces
  tail <- char '.' >> spaces >> parseExpr
  return $ DottedList head tail

parseQuoted :: Parser LispVal
parseQuoted = do
  char '\''
  x <- parseExpr
  return $ List [Atom "quote", x]

parseExpr :: Parser LispVal
parseExpr = try parseQuoted
            <|> do char '('
                   x <- try parseDottedList <|> parseList
                   char ')'
                   return x
            <|> try parseAtom
            <|> try parseString
            <|> try parseCharacter
            <|> try parseComplex
            <|> try parseFloat
            <|> try parseNumber
            
               
symbol :: Parser Char
symbol = oneOf "!#$%&|*+-/:<=>?@^_~"
         
readExpr :: String -> LispVal
readExpr input = case parse parseExpr "lisp" input of
  Left err -> String $ "No match: " ++ show err
  Right val -> val

spaces :: Parser ()
spaces = skipMany1 space

main :: IO ()
main = do
  getArgs >>= print . eval . readExpr . head