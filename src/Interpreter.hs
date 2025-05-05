module Interpreter where

import qualified Data.Map as Map
import System.IO
import Parser
import Data.List.Split (splitOn)

-- Environment to hold variable bindings
type Env = Map.Map String Value

data Value
  = CSV [[String]]
  | Str String
  | IntVal Int
  deriving (Show, Eq)

interpretProgram :: Program -> IO ()
interpretProgram (Program stmts) = do
  _ <- evalStmts Map.empty stmts
  return ()

-- Evaluate a list of statements
evalStmts :: Env -> [Statement] -> IO Env
evalStmts env [] = return env
evalStmts env (stmt:rest) = do
  env' <- evalStmt env stmt
  evalStmts env' rest

-- Evaluate a single statement
evalStmt :: Env -> Statement -> IO Env
evalStmt env (Assignment name expr) = do
  val <- evalExpr env expr
  return $ Map.insert name val env

evalStmt env (Output expr) = do
  val <- evalExpr env expr
  print val
  return env

evalStmt env (If cond thenStmts) = do
  condVal <- evalExpr env cond
  case condVal of
    IntVal 0 -> return env
    _        -> evalStmts env thenStmts

evalStmt env (IfElse cond thenStmts elseStmts) = do
  condVal <- evalExpr env cond
  if condVal == IntVal 0
    then evalStmts env elseStmts
    else evalStmts env thenStmts

-- Evaluate an expression
evalExpr :: Env -> Expr -> IO Value
evalExpr _ (IntLiteral n) = return $ IntVal n
evalExpr _ (String s)     = return $ Str s
evalExpr _ (Filename f)   = return $ Str f

evalExpr env (Variable name) =
  case Map.lookup name env of
    Just v  -> return v
    Nothing -> error $ "Unbound variable: " ++ name

evalExpr env (ReadFile path) = do
  content <- readFile path
  return $ CSV (parseCSV content)

evalExpr env (ReadFileVar varName) = do
  val <- evalExpr env (Variable varName)
  case val of
    Str path -> evalExpr env (ReadFile path)
    _ -> error "Expected string in readFile variable"


parseCSV :: String -> [[String]]
parseCSV = map (splitOn ",") . lines

-- Function to extract a column of data from a table
indexColumn :: [[String]] -> Int -> [String]
indexColumn [] _ = []
indexColumn (x:xs) index
  | index + 1 > length x = error $ show $ "Index too big!"
  | otherwise = (x!!index) : indexColumn xs index

-- Function for building a table of data (for output). Adds a given column to the table
addColumn :: [[String]] -> [String] -> [[String]]
addColumn [] xs = map (: []) xs
addColumn xs ys = zipWith (\ xs' x  -> xs' ++ [x]) xs ys

-- use repeat to make a constant string into an infinite list which can be used to append columns and stuff
