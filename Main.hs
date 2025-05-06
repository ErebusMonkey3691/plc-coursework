module Main where

import Lexer
import Parser
import Interpreter
import System.Environment (getArgs)
import System.IO

main :: IO ()
main = do
  args <- getArgs
  case args of
    [filename] -> do
      content <- readFile filename
      let tokens = alexScanTokens content
      let ast = parser tokens
      interpretProgram (Program ast)
    _ -> putStrLn "Usage: ./dslinterpreter <source.dsl>"
