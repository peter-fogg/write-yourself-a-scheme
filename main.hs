module Main where
import System.Environment
import Control.Monad
import Parse

main :: IO ()
main = do
  args <- getArgs
  case length args of
    0 -> runRepl
    1 -> runOne $ args !! 1
    otherwise -> putStrLn "Foo!"
-- main :: IO ()
-- main = do
--   putStrLn "Your name?"
--   line <- getLine
--   putStrLn $ "Hello, " ++ line ++ "!"