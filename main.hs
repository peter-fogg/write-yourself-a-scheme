module Main where
import System.Environment
import Control.Monad
import Parse

main :: IO ()
main = do
  args <- getArgs
  if null args then runRepl else runOne $ args
