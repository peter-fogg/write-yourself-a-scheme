module Parse
       where

import Text.ParserCombinators.Parsec hiding (spaces)
import System.Environment
import Control.Monad
import Control.Monad.Error
import Numeric

data LispError = NumArgs Integer [LispVal]
               | TypeMismatch String LispVal
               | Parser ParseError
               | BadSpecialForm String LispVal
               | NotFunction String String
               | UnboundVar String String
               | Default String

showError :: LispError -> String
showError (UnboundVar message varname) = message ++ ": " ++ varname
showError (BadSpecialForm message form) = message ++ ": " ++ show form
showError (NotFunction message func) = message ++ ": " ++ show func
showError (NumArgs expected found) = "Expected " ++ show expected ++ "args; found values " ++ unwordsList found
showError (Parser parseErr) = "Parse error at " ++ show parseErr
showError (TypeMismatch expected found) = "Expected " ++ show expected ++ ", found " ++ show found

instance Show LispError where show = showError

instance Error LispError where
  noMsg = Default "Shit, bro!"
  strMsg = Default

type ThrowsError = Either LispError

trapError action = catchError action (return . show)

extractValue :: ThrowsError a -> a
extractValue (Right val) = val

data LispVal = Atom String
             | List [LispVal]
             | DottedList [LispVal] LispVal
             | Number Integer
             | Float Double
             | Complex Double Double
             | String String
             | Bool Bool

showVal :: LispVal -> String
showVal (String contents) = "\"" ++ contents ++ "\""
showVal (Atom name) = name
showVal (Number value) = show value
showVal (Float value) = show value
showVal (Complex real imaginary) = (show real) ++ "+" ++ (show imaginary) ++ "i"
showVal (Bool True) = "#t"
showVal (Bool False) = "#f"
showVal (List contents) = "(" ++ unwordsList contents ++ ")"
showVal (DottedList head tail) = "(" ++ unwordsList head ++ " . " ++ showVal tail ++ ")"

instance Show LispVal where show = showVal

unwordsList :: [LispVal] -> String
unwordsList = unwords . map showVal

eval :: LispVal -> ThrowsError LispVal
eval val@(String _) = return val
eval val@(Bool _) = return val
eval val@(Number _) = return val
eval val@(Float _) = return val
eval val@(Complex _ _) = return val
eval val@(Atom _) = return val
eval (List [Atom "quote", val]) = return val
eval (List [Atom "quasiquote", val]) = evalQuasiQuoted val
eval (List [Atom "if", pred, conseq, alt]) = -- because sometimes weak typing is chill
  do result <- eval pred
     case result of
       Bool False -> eval alt
       otherwise -> eval conseq
eval (List (Atom func : args)) = mapM eval args >>= apply func
eval badForm = throwError $ BadSpecialForm "Unrecognized special form" badForm

evalQuasiQuoted :: LispVal -> ThrowsError LispVal
evalQuasiQuoted (List [(List [Atom "unquote", val])]) =
  case (eval val) of
    Right val -> return $ List [val]
    Left err -> Left err
evalQuasiQuoted (List [Atom "unquote", val]) = eval val
evalQuasiQuoted val = return val

apply :: String -> [LispVal] -> ThrowsError LispVal
apply func args = maybe (throwError $ NotFunction "Unrecognized primitive function args" func)
                  ($ args)
                  (lookup func primitives)

eqv :: [LispVal] -> ThrowsError LispVal
eqv [(Bool arg1), (Bool arg2)] = return $ Bool $ arg1 == arg2
eqv [(Number arg1), (Number arg2)] = return $ Bool $ arg1 == arg2
eqv [(String arg1), (String arg2)] = return $ Bool $ arg1 == arg2
eqv [(Float arg1), (Float arg2)] = return $ Bool $ arg1 == arg2
eqv [(Atom arg1), (Atom arg2)] = return $ Bool $ arg1 == arg2
eqv [(DottedList xs x), (DottedList ys y)] = eqv [List $ xs ++ [x], List $ ys ++ [y]]
eqv [(List arg1), (List arg2)] = return $ Bool $ (length arg1 == length arg2) &&
                                 (all eqvPair $ zip arg1 arg2)
  where eqvPair (x, y) = case eqv [x, y] of
          Right (Bool val) -> val
          Left err -> False
eqv [_, _] = return $ Bool False
eqv bad = throwError $ NumArgs 2 bad

car :: [LispVal] -> ThrowsError LispVal
car [List (x:xs)] = return x
car [DottedList (x:xs) _] = return x
car [bad] = throwError $ TypeMismatch "pair" bad
car bad = throwError $ NumArgs 1 bad

cdr :: [LispVal] -> ThrowsError LispVal
cdr [List (x:xs)] = return $ List xs
cdr [DottedList (x:[]) val] = return val
cdr [DottedList (x:xs) val] = return $ DottedList xs val
cdr [bad] = throwError $ TypeMismatch "pair" bad
cdr bad = throwError $ NumArgs 1 bad

cons :: [LispVal] -> ThrowsError LispVal
cons [x, List []] = return $ List [x]
cons [x, List xs] = return $ List (x:xs)
cons [x, DottedList xs val] = return $ DottedList (x:xs) val
cons [x, y] = return $ DottedList [x] y
cons bad = throwError $ NumArgs 2 bad

boolBinop :: (LispVal -> ThrowsError a) -> (a -> a -> Bool) -> [LispVal] -> ThrowsError LispVal
boolBinop unpacker op args = if length args /= 2
                             then throwError $ NumArgs 2 args
                             else do left <- unpacker $ args !! 0
                                     right <- unpacker $ args !! 1
                                     return $ Bool (left `op` right)

numBoolBinop = boolBinop unpackNum
strBoolBinop = boolBinop unpackStr
boolBoolBinop = boolBinop unpackBool

unpackStr :: LispVal -> ThrowsError String
unpackStr (String contents) = return contents
unpackStr bad = throwError $ TypeMismatch "string" bad -- man, FUCK weak typing

unpackBool :: LispVal -> ThrowsError Bool
unpackBool (Bool b) = return b
unpackBool bad = throwError $ TypeMismatch "bool" bad

numericBinop :: (Integer -> Integer -> Integer) -> [LispVal] -> ThrowsError LispVal
numericBinop op singleVal@[_] = throwError $ NumArgs 2 singleVal
numericBinop op params = mapM unpackNum params >>= return . Number . foldl1 op
-- numericBinop op params = foldM (liftOp op) (getBaseCase op) params
--   where getBaseCase (+) = Number 0
--         getBaseCase (-) = Number 0
--         getBaseCase (*) = Number 1
--         getBaseCase (/) = Number 1
--  where resultType val@(Double _) = Float val
--        resultType val@(Number _) = 

--liftOp :: Num a => (a -> a -> a) -> LispVal -> LispVal -> ThrowsError LispVal
liftOp :: (Integer -> Integer -> Integer) -> LispVal -> LispVal -> ThrowsError LispVal
liftOp op (Number l) (Number r) = return . Number $ l `op` r
--liftOp op (Float l) (Number r) = return . Float $ op l (fromIntegral r)
--liftOp op (Number l) (Float r) = return . Float $ op l r
--liftOp op (Float l) (Float r) = return . Float $ op l r
liftOp _ badl badr  = throwError $ TypeMismatch ((show badl) ++ (show badr)) badl

unpackNum :: LispVal -> ThrowsError Integer
unpackNum (Number n) = return n
-- unpackNum (String n) = let parsed = reads n in
--   if null parsed
--   then throwError $ TypeMismatch "number" $ String n
--   else return $ fst $ parsed !! 0
-- unpackNum (List [n]) = unpackNum n
unpackNum notNum = throwError $ TypeMismatch "number" notNum

isNumber :: [LispVal] -> ThrowsError LispVal
isNumber [(Number _)] = return $ Bool True
isNumber [bad] = throwError $ TypeMismatch (show bad) bad

isBool :: [LispVal] -> ThrowsError LispVal
isBool [(Bool _)] = return $ Bool True
isBool [bad] = throwError $ TypeMismatch (show bad) bad

isString :: [LispVal] -> ThrowsError LispVal
isString [(String _)] = return $ Bool True
isString [bad] = throwError $ TypeMismatch (show bad) bad

isAtom :: [LispVal] -> ThrowsError LispVal
isAtom [(Atom _)] = return $ Bool True
isAtom [bad] = throwError $ TypeMismatch (show bad) bad

isComplex :: [LispVal] -> ThrowsError LispVal
isComplex [(Complex _ _)] = return $ Bool True
isComplex [bad] = throwError $ TypeMismatch (show bad) bad

isFloat :: [LispVal] -> ThrowsError LispVal
isFloat [(Float _)] = return $ Bool True
isFloat [bad] = throwError $ TypeMismatch (show bad) bad

isList :: [LispVal] -> ThrowsError LispVal
isList [(List _)] = return $ Bool True
isList [bad] = throwError $ TypeMismatch (show bad) bad

symbolToString :: [LispVal] -> ThrowsError LispVal
symbolToString [(Atom val)] = return $ String val
symbolToString [bad] = throwError $ TypeMismatch (show bad) bad

stringToSymbol :: [LispVal] -> ThrowsError LispVal
stringToSymbol [(String contents)] = return $ Atom contents
stringToSymbol [bad] = throwError $ TypeMismatch (show bad) bad

primitives :: [(String, [LispVal] -> ThrowsError LispVal)]
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
              ("complex?", isComplex),
              ("float?", isFloat),
              ("list?", isList),
              ("number?", isNumber),
              ("string->symbol", stringToSymbol),
              ("symbol->string", symbolToString),
              ("=", numBoolBinop (==)),
              ("<", numBoolBinop (<)),
              (">", numBoolBinop (>)),
              ("/=", numBoolBinop (/=)),
              (">=", numBoolBinop (>=)),
              ("<=", numBoolBinop (<=)),
              ("&&", boolBoolBinop (&&)),
              ("||", boolBoolBinop (||)),
              ("string=?", strBoolBinop (==)),
              ("string<?", strBoolBinop (<)),
              ("string>?", strBoolBinop (>)),
              ("string<=?", strBoolBinop (<=)),
              ("string>=?", strBoolBinop (>=)),
              ("car", car),
              ("cdr", cdr),
              ("cons", cons),
              ("eq?", eqv),
              ("eqv?", eqv)]

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

parseQuasiQuoted :: Parser LispVal
parseQuasiQuoted = do
  char '`'
  x <- parseExpr
  return $ List [Atom "quasiquote", x]

parseUnquoted :: Parser LispVal
parseUnquoted = do
  char ','
  x <- parseExpr
  return $ List [Atom "unquote", x]

parseExpr :: Parser LispVal
parseExpr = try parseQuoted
            <|> parseQuasiQuoted
            <|> parseUnquoted
            <|> do char '('
                   x <- try parseDottedList <|> try parseList
                   char ')'
                   return x
            <|> try parseAtom
            <|> try parseString
            <|> try parseComplex
            <|> try parseFloat
            <|> try parseNumber
            
               
symbol :: Parser Char
symbol = oneOf "!#$%&|*+-/:<=>?@^_~"
         
readExpr :: String -> ThrowsError LispVal
readExpr input = case parse parseExpr "lisp" input of
  Left err -> throwError $ Parser err
  Right val -> return val

spaces :: Parser ()
spaces = skipMany1 space

-- main :: IO ()
-- main = do
--   args <- getArgs
--   evaled <- return $ liftM show $ readExpr (args !! 0) >>= eval
--   putStrLn $ extractValue $ trapError evaled