module Main where
import System.Environment

main :: IO ()
main = do
  putStrLn "Your name?"
  line <- getLine
  putStrLn $ "Hello, " ++ line ++ "!"