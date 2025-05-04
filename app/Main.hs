module Main (
    main
  ) where

import Lib (introMessage)
import Parser
import Lexer

main :: IO ()
main = do
  -- putStrLn introMessage
  let a = alexScanTokens "let A = readFile('A.csv')"
  putStrLn $ show $ a
  let b = parser a
  putStrLn $ show $ b
