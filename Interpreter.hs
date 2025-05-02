module Interpreter where

import qualified Data.Map as Map
import System.IO
import Grammar  
import Data.List.Split (splitOn)
import Data.List (sort)

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

-- Existence Check Expression
evalExpr env (ExistenceExpr expr) = do
  val <- evalExpr env expr
  case val of
    CSV rows -> do
      -- Filter out rows where the second column is an empty string
      let filtered = [ [a1, a2] | [a1, a2] <- rows, not (null a2) ]
          sorted = lexSort filtered
      return $ CSV sorted
    _ -> error "ExistenceExpr expects a CSV with 2 columns"

-- Cartesian Product Expression
evalExpr env (CartesianExpr exprs) = do
  -- Evaluate all expressions in the cartesian product
  vals <- mapM (evalExpr env) exprs
  case allCSV vals of
    Just csvs -> return $ CSV (cartesianProduct csvs)
    Nothing   -> error "CartesianExpr expects only CSVs"

-- Permutation Expression
evalExpr env (PermutationExpr expr) = do
  val <- evalExpr env expr
  case val of
    CSV rows -> do
      let permuted = [ [a3, a1] | [a1, a2, a3] <- rows, a1 == a2 ]
          sorted = lexSort permuted
      return $ CSV sorted
    _ -> error "PermutationExpr expects a CSV with 3 columns"

-- Helper function for sorting the result lexicographically
lexSort :: [[String]] -> [[String]]
lexSort = sort

-- Helper function to check if all values are CSVs
allCSV :: [Value] -> Maybe [[String]]
allCSV [] = Just []
allCSV (CSV rows:rest) = do
  restRows <- allCSV rest
  return (rows : restRows)
allCSV _ = Nothing

-- Cartesian product of N lists (foldr-based)
cartesianProduct :: [[[String]]] -> [[String]]
cartesianProduct = foldr cartesianProduct2 [[]]
  where
    cartesianProduct2 :: [[String]] -> [[String]] -> [[String]]
    cartesianProduct2 xs ys = [ x ++ y | x <- xs, y <- ys ]

-- Parsing CSV rows
parseCSV :: String -> [[String]]
parseCSV = map (splitOn ",") . lines
