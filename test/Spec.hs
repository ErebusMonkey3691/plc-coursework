module Main (
  main
) where

import Test.HUnit
import Interpreter
import Lexer
import Parser
import System.IO
import Test.Silently (capture)


captureOutput :: IO a -> IO (a, String)
captureOutput action = do
  -- Save original stdout
  originalStdout <- hDuplicate stdout

  -- Create a temporary file
  (tempPath, tempHandle) <- openTempFile "." "hunit_capture.txt"  -- Creates in current directory
  hSetBuffering tempHandle NoBuffering  -- Avoid buffering issues

  -- Redirect stdout to the temporary file
  hDuplicateTo tempHandle stdout

  -- Run the action and capture result
  result <- action

  -- Restore stdout
  hFlush stdout
  hDuplicateTo originalStdout stdout

  -- Read captured output
  hClose tempHandle
  captured <- readFile tempPath

  -- Cleanup: Delete temporary file
  removeFile tempPath

  return (result, captured)

test4a :: Test
test4a = TestCase (do 
  program <- readFile "test/programs/t4/a.cql"
  (result, _) <- capture (interpretProgram $ Program (parser $ alexScanTokens program))
  _ <- assertEqual ("Task 4" result "Brenda,foo,Brenda\nCiara,foo,Ciara\nEric,foo,Eric")
  print "test 4a complete")

test4b :: Test
test4b = TestCase (do
  program <- readFile "test/programs/t4/b.cql"
  (result, _) <- capture (interpretProgram $ Program (parser $ alexScanTokens program))
  _ <- assertEqual ("Task 4" result ",foo,\nBrenda,foo,Brenda\nEric,foo,Eric")
  print "test 4b complete"
  )

tests :: Test
tests = TestList [TestLabel "test4a" test4a, TestLabel "test4b" test4b]

main :: IO ()
main = do
  putStrLn "Test suite starting."
  -- program1 <- readFile "test/Task4.cql"
  -- putStrLn "Task 4 Result:"
  -- interpretProgram $ Program (parser $ alexScanTokens program1)
  testResults <- runTestTT tests
  print testResults