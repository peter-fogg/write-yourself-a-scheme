module Parse
       where

import Text.ParserCombinators.Parsec hiding (spaces)
import System.IO
import System.Environment
import Data.IORef
import Control.Monad
import Control.Monad.Error
import Numeric

type Env = IORef [(String, IORef LispVal)]

nullEnv :: IO Env
nullEnv = newIORef []

type IOThrowsError = ErrorT LispError IO

liftThrows :: ThrowsError a -> IOThrowsError a
liftThrows (Left err) = throwError err
liftThrows (Right val) = return val

runIOThrows :: IOThrowsError String -> IO String
runIOThrows action = runErrorT (trapError action) >>= return . extractValue

isBound :: Env -> String -> IO Bool
isBound envRef var = readIORef envRef >>= return . maybe False (const True) . lookup var

getVar :: Env -> String -> IOThrowsError LispVal
getVar envRef var = do env <- liftIO $ readIORef envRef
                       maybe (throwError $ UnboundVar "Getting an unbound variable" var)
                         (liftIO . readIORef)
                         (lookup var env)

setVar :: Env -> String -> LispVal -> IOThrowsError LispVal
setVar envRef var value = do env <- liftIO $ readIORef envRef
                             maybe (throwError $ UnboundVar "Setting an unbound variable" var)
                               (liftIO . (flip writeIORef value))
                               (lookup var env)
                             return value

defineVar :: Env -> String -> LispVal -> IOThrowsError LispVal
defineVar envRef var value = do
  alreadyDefined <- liftIO $ isBound envRef var
  if alreadyDefined
    then setVar envRef var value >> return value
    else liftIO $ do valueRef <- newIORef value
                     env <- readIORef envRef
                     writeIORef envRef ((var, valueRef) : env)
                     return value

bindVars :: Env -> [(String, LispVal)] -> IO Env
bindVars envRef bindings = readIORef envRef >>= extendEnv bindings >>= newIORef
  where extendEnv bindings env = liftM (++ env) (mapM addBinding bindings)
        addBinding (var, value) = do ref <- newIORef value
                                     return (var, ref)

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
showError (NumArgs expected found) = "Expected " ++ show expected ++ " args; found values " ++ unwordsList found
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
             | PrimitiveFunc ([LispVal] -> ThrowsError LispVal)
             | Func {params :: [String], vararg :: (Maybe String),
                     body :: [LispVal], closure :: Env}

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
showVal (PrimitiveFunc _) = "<primitive>"
showVal (Func {params = args, vararg = varargs, body = body, closure = env}) =
  "(lambda (" ++ unwords (map show args) ++
  (case varargs of
      Nothing -> ""
      Just arg -> " . " ++ arg) ++ ") ...)"

instance Show LispVal where show = showVal

unwordsList :: [LispVal] -> String
unwordsList = unwords . map showVal

eval :: Env -> LispVal -> IOThrowsError LispVal
eval env val@(String _) = return val
eval env val@(Bool _) = return val
eval env val@(Number _) = return val
eval env val@(Float _) = return val
eval env val@(Complex _ _) = return val
eval env (Atom id) = getVar env id
eval env (List (Atom "define" : List (Atom var : params) : body)) =
  makeNormalFunc env params body >>= defineVar env var
eval env (List (Atom "define" : DottedList (Atom var : params) varargs : body)) =
  makeVarargs varargs env params body >>= defineVar env var
eval env (List (Atom "lambda" : List params : body)) =
  makeNormalFunc env params body
eval env (List (Atom "lambda" : DottedList params varargs : body)) =
  makeVarargs varargs env params body
eval env (List (Atom "lambda" : varargs@(Atom _) : body)) =
  makeVarargs varargs env [] body
eval env (List [Atom "quote", val]) = return val
eval env (List [Atom "unquote", val]) = eval env val
eval env (List [Atom "quasiquote", val]) = evalQuasiQuoted env val
eval env (List [Atom "if", pred, conseq, alt]) = -- because sometimes weak typing is chill
  do result <- eval env pred
     case result of
       Bool False -> eval env alt
       otherwise -> eval env conseq
eval env (List [Atom "set!", Atom var, form]) = eval env form >>= setVar env var
eval env (List [Atom "define", Atom var, form]) = eval env form >>= defineVar env var
eval env (List (function : args)) = do
  func <- eval env function
  argVals <- mapM (eval env) args
  apply func argVals
--eval env (List (Atom func : args)) = mapM (eval env) args >>= liftThrows . apply func
eval env badForm = throwError $ BadSpecialForm "Unrecognized special form" badForm

evalQuasiQuoted :: Env -> LispVal -> IOThrowsError LispVal
--evalQuasiQuoted env (List [(List [Atom "unquote", val])]) = 
--  case runErrorT $ eval env val of
--    Right val -> return $ List [val]
--    Left err -> liftThrows . Left err
evalQuasiQuoted env (List [Atom "unquote", val]) = eval env val
evalQuasiQuoted env val = return val

apply :: LispVal -> [LispVal] -> IOThrowsError LispVal
apply (PrimitiveFunc func) args = liftThrows $ func args
apply (Func params varargs body closure) args =
  if num params /= num args && varargs == Nothing
  then throwError $ NumArgs (num params) args
  else (liftIO $ bindVars closure $ zip params args) >>= bindVarArgs varargs >>= evalBody
  where remainingArgs = drop (length params) args
        num = toInteger . length
        evalBody env = liftM last $ mapM (eval env) body
        bindVarArgs arg env = case arg of
          Just argName -> liftIO $ bindVars env [(argName, List $ remainingArgs)]
          Nothing -> return env

primitiveBindings :: IO Env
primitiveBindings = nullEnv >>= (flip bindVars $ map makePrimitiveFunc primitives)
                    where makePrimitiveFunc (var, func) = (var, PrimitiveFunc func)

makeFunc varargs env params body = return $ Func (map showVal params) varargs body env
makeNormalFunc = makeFunc Nothing
makeVarargs = makeFunc . Just . showVal

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

stringLength :: [LispVal] -> ThrowsError LispVal
stringLength [(String contents)] = return $ Number $ fromIntegral $ length contents
stringLength [bad] = throwError $ TypeMismatch (show bad) bad
stringLength bad = throwError $ NumArgs 1 bad

stringRef :: [LispVal] -> ThrowsError LispVal
stringRef [(String contents), (Number val)] = return $ String $ [contents !! (fromIntegral val)]
stringRef [bad, bad2] = throwError $ TypeMismatch (show bad) bad
stringRef bad = throwError $ NumArgs 2 bad

substring :: [LispVal] -> ThrowsError LispVal
substring [(String contents), (Number start), (Number end)] = let intStart = fromIntegral start
                                                                  intEnd = fromIntegral end
                                                              in return $ String $ take (intEnd - intStart) . drop intStart $ contents
substring bad@[_, _, _] = throwError $ TypeMismatch (foldl (\acc x -> acc ++ ", " ++  (show x)) "" bad) $ bad !! 0
substring bad = throwError $ NumArgs 3 bad

stringAppend :: [LispVal] -> ThrowsError LispVal
stringAppend [(String first), (String second)] = return $ String $ first ++ second
stringAppend bad@[_, _] = throwError $ TypeMismatch (foldl (\acc x -> acc ++ ", " ++ (show x)) "" bad) $ bad !! 0
stringAppend bad = throwError $ NumArgs 2 bad

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
              ("string-length", stringLength),
              ("string-ref", stringRef),
              ("substring", substring),
              ("string-append", stringAppend),
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
                   many spaces
                   x <- try parseDottedList <|> try parseList
                   many spaces
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

flushStr :: String -> IO ()
flushStr str = putStr str >> hFlush stdout

readPrompt :: String -> IO String
readPrompt prompt = flushStr prompt >> getLine

evalString :: Env -> String -> IO String
evalString env expr = runIOThrows $ liftM show $ (liftThrows $ readExpr expr) >>= eval env

evalAndPrint :: Env -> String -> IO ()
evalAndPrint env expr = evalString env expr >>= putStrLn

runOne :: String -> IO ()
runOne expr = primitiveBindings >>= flip evalAndPrint expr

runRepl :: IO ()
runRepl = primitiveBindings >>= until_ (== "quit") (readPrompt "Lisp > ") . evalAndPrint

until_ :: Monad m => (a -> Bool) -> m a -> (a -> m ()) -> m ()
until_ pred prompt action = do
  result <- prompt
  if pred result
    then return ()
    else action result >> until_ pred prompt action

-- main :: IO ()
-- main = do
--   args <- getArgs
--   evaled <- return $ liftM show $ readExpr (args !! 0) >>= eval
--   putStrLn $ extractValue $ trapError evaled