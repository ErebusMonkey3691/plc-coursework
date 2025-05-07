module Main (
    main
  ) where

import Parser
import Lexer
import Interpreter
import System.Environment (getArgs)

main :: IO ()
main = do
  progArgs <- getArgs
  -- putStrLn introMessage
  -- let a = alexScanTokens "let A = B\noutput(A[1])"
  -- putStrLn $ show $ a
  -- let b = parser a
  -- putStrLn $ show $ b
  -- csvA <- readFile "test/A.csv"
  -- print $ show $ csvA
  program <- readFile (head progArgs)
  print (alexScanTokens program)
  print (parser $ alexScanTokens program)
  interpretProgram $ Program (parser $ alexScanTokens program)
  -- x <- getArgs
  -- print x
