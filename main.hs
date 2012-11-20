module Main where
import System.Environment
import Control.Monad
import Parse

main :: IO ()
main = do
  args <- getArgs
  evaled <- return $ liftM show $ readExpr (args !! 0) >>= eval
  putStrLn $ extractValue $ trapError evaled
-- main :: IO ()
-- main = do
--   putStrLn "Your name?"
--   line <- getLine
--   putStrLn $ "Hello, " ++ line ++ "!"