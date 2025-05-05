module Main (
    main
  ) where

import Lib (introMessage)
import Parser
import Lexer

main :: IO ()
main = do
  -- putStrLn introMessage
  let a = alexScanTokens "let A = B\nA[1]"
  putStrLn $ show $ a
  let b = parser a
  putStrLn $ show $ b
  -- csvA <- readFile "test/A.csv"
  -- print $ show $ csvA
