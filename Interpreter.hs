-- Interpreter.hs
module Interpreter where

import qualified Data.Map as Map
import System.IO
import Grammar  -- From your parser (Grammar.y)
import Data.List.Split (splitOn)

-- Environment to hold variable bindings
type Env = Map.Map String Value

-- Values in our interpreted language
data Value
  = CSV [[String]]
  | Str String
  | IntVal Int
  deriving (Show, Eq)

-- Top-level interpreter
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

-- TODO: add other cases for Cartesian, Permutation, etc.

-- CSV parsing helper (very basic, update as needed)
parseCSV :: String -> [[String]]
parseCSV = map (splitOn ",") . lines
