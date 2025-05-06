module Main (
    main
  ) where

import Parser
import Lexer
import Interpreter

main :: IO ()
main = do
  -- putStrLn introMessage
  -- let a = alexScanTokens "let A = B\noutput(A[1])"
  -- putStrLn $ show $ a
  -- let b = parser a
  -- putStrLn $ show $ b
  -- csvA <- readFile "test/A.csv"
  -- print $ show $ csvA
  program <- readFile "test/Task4.cql"
  print (alexScanTokens program)
  print (parser $ alexScanTokens program)
  interpretProgram $ Program (parser $ alexScanTokens program)