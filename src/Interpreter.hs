-- {-# OPTIONS_GHC -Wno-incomplete-patterns #-}
{-# LANGUAGE BlockArguments #-}
module Interpreter where

import qualified Data.Map as Map
import System.IO
import Parser
import Data.List.Split (splitOn)
import Data.List
import GHC.ResponseFile (expandResponse)
import Control.Concurrent (rtsSupportsBoundThreads)
import Control.Monad (foldM)
import Foreign.C (e2BIG)
import GHC.IO.Encoding.CodePage.Table (CompactArray(encoderValues))

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
-- evalStmts env rest = foldM evalStmt env rest
evalStmts = foldM evalStmt
-- evalStmts env [] = return env
-- evalStmts env (stmt:rest) = do
--   env' <- evalStmt env stmt
--   evalStmts env' rest


-- Evaluate a single statement
evalStmt :: Env -> Statement -> IO Env
evalStmt env (Assignment name expr) = do
  val <- evalExpr env expr
  return $ Map.insert name val env

evalStmt env (Output expr) = do
  let val = evalOutput env expr
  putStrLn $ formatOutResult val
  print env
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
evalExpr _ (ReadFile csvName) = grabCSV csvName
-- evalExpr env expr = return $ CSV (evalOutput env expr)

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
evalExpr env (Existence expr) = do
  val <- evalExpr env expr
  case val of
    CSV rows -> do
      let filtered = [ [a1, a2] | [a1, a2] <- rows, not (null a2)]
      let sorted = sort filtered
      return $ CSV sorted
    _ -> error "ExistenceExpr expects a CSV with 2 columns"

-- Cartesian Product Expression
-- evalExpr env (Cartesian exprs) = do
--   -- Evaluate all expressions in the cartesian product
--   vals <- mapM (evalExpr env) exprs
--   case allCSV vals of
--     Just csvs -> return $ CSV (cartesianProduct [csvs])
--     Nothing -> error "CartesianExpr expects only CSVs"

evalExpr env (Cartesian exprs) = return $ CSV (cartesianProduct vals)
  where
    vals = map (evalOutput env) exprs


evalExpr env (Permutation expr) = do
  val <- evalExpr env expr
  case val of
    CSV rows -> do
      let permuted = [ [a3, a1] | [a1, a2, a3] <- rows, a1 == a2 ]
      let sorted = sort permuted
      return $ CSV sorted
    _ -> error "PermutationExpr expects a CSV with 3 columns"

evalExpr env expr@(LeftMerge _ _ _) = return $ handleLeftMerge env expr


handleLeftMerge :: Env -> Expr -> Value
handleLeftMerge env (LeftMerge (Variable v1) (Variable v2) boolExpr)
  | v1 < v2 = CSV $ mergeTables env v1 v2 boolExpr False -- don't reverse the tables
  | v2 < v1 = CSV $ mergeTables env v2 v1 boolExpr True -- reverse the tables
  | otherwise = error $ "Bindings not matching in left merge..."
handleLeftMerge _ e1 = error $ "Incorrect Arguments used for handleLeftMerge" ++ show e1

mergeTables :: Env -> String -> String -> BoolExpr -> Bool -> [[String]]
mergeTables env var1 var2 boolexpr reverse = merged
  where
    table1 = tableLookup var1 env
    table2 = tableLookup var2 env


    -- xIndex = extractIndex 1 boolexpr
    -- yIndex = extractIndex 1 boolexpr

    -- extractIndex :: Int -> BoolExpr -> Int
    -- extractIndex num (Equality (IndexedVar _ x1) (IndexedVar _ x2))
    --   | num == 1 = x1
    --   | otherwise = x2
    -- extractIndex num (Inequality (IndexedVar _ x1) (IndexedVar _ x2))
    --   | num == 1 = x1
    --   | otherwise = x2
    -- extractIndex _ e1 = error $ "Unrecognised boolExpr given to extractIndex: " ++ show e1


    merged
      | reverse = [ helperMerge y x | x <- table1, y <- table2, boolFunc x y ]
      | not reverse = [ helperMerge x y | x <- table1, y <- table2, boolFunc x y ]

    boolFunc x y = boolEval x y boolexpr


    firstOtherwiseSecond "" y = y
    firstOtherwiseSecond x _ = x

    -- merge two given lists, prioritising the first one
    helperMerge = zipWith firstOtherwiseSecond

boolEval :: [String] -> [String] -> BoolExpr -> Bool
boolEval xs ys (Equality e1@(IndexedVar _ _) e2@(IndexedVar _ _)) = (==) (xs!!x1) (ys!!x2)
  where
    (x1, x2) = orientatex1x2 e1 e2
boolEval xs ys (Inequality e1@(IndexedVar _ _) e2@(IndexedVar _ _)) = (/=) (xs!!x1) (ys!!x2)
  where
    (x1, x2) = orientatex1x2 e1 e2
boolEval xs ys (And boolExpr boolExpr2) = (&&) (boolEval xs ys boolExpr) (boolEval xs ys boolExpr2)
boolEval xs ys (Or boolExpr boolExpr2) = (||) (boolEval xs ys boolExpr) (boolEval xs ys boolExpr2)
boolEval _ _ _ = undefined

orientatex1x2 :: Expr -> Expr -> (Int, Int)
orientatex1x2 (IndexedVar n1 x1) (IndexedVar n2 x2)
      | n1 < n2 = (x1, x2)
      | otherwise = (x2, x1)
orientatex1x2 _ _ = error $ "Orientatex1x2 called on non-indexed-vars."

-- -- Helper function to check if all values are CSVs
-- allCSV :: [Value] -> Maybe [[String]]
-- allCSV [] = Just []
-- allCSV (CSV rows:rest) = do
--   restRows <- allCSV rest
--   return (rows ++ restRows)
-- allCSV _ = Nothing

-- Cartesian product of N lists (foldr-based)
cartesianProduct :: [[[String]]] -> [[String]]
cartesianProduct = foldr cartesianProduct2 [[]]
  where
    cartesianProduct2 :: [[String]] -> [[String]] -> [[String]]
    cartesianProduct2 xs ys = [ x ++ y | x <- xs, y <- ys ]

-- extractIO :: IO a -> a
-- extractIO io = case io of
--   Prelude.ReadFile s -> s
--   _ -> error "Not from a file, exiting..."

evalOutput :: Env -> Expr -> [[String]]
evalOutput env (Variable n) = varTable
  where
    varTable = tableLookup n env
evalOutput env (IndexedVar n x) = indexColumn varTable x
  where
    varTable = tableLookup n env
evalOutput _ (Constant n) = repeat [n]
evalOutput env (List e1 e2) = addColumn (evalOutput env e1) (evalOutput env e2)
evalOutput _ expr = error $ "Found an unmatched expr: " ++ show expr


tableLookup :: String -> Env -> [[String]]
tableLookup n env = case Map.lookup n env of
  Just (CSV table) -> table
  Just (Str _) -> error $ "Variable is not a table." ++ n
  Just (IntVal _) -> error $ "Variable is not a table." ++ n
  Nothing -> error $ "Unbound variable: " ++ n

parseCSV :: String -> [[String]]
parseCSV "" = []
parseCSV xs
  | all (== ' ') $ xs = []
parseCSV x = map (map trimSpaces . splitOn ",") . lines $ x

trimSpaces :: String -> String
trimSpaces = dropWhile (== ' ') . reverse . dropWhile (== ' ') . reverse

-- Utility function to grab a table variable on demand
grabCSV :: String -> IO Value
grabCSV csvName = do
  fileContents <- readFile csvName
  let table = parseCSV fileContents
  return (CSV table)

-- Function to extract a column of data from a table
indexColumn :: [[String]] -> Int -> [[String]]
indexColumn [] _ = []
indexColumn (x:xs) index
  | index + 1 > length x = error $ show $ "Index too big!"
  | otherwise = [x!!index] : indexColumn xs index

-- Function for building a table of data (for output). Adds a given column to the table
addColumn :: [[String]] -> [[String]] -> [[String]]
addColumn [] xs = xs
addColumn xs ys = zipWith (++) xs ys

-- Helper function to convert a table to a string in order to allow you to print it to stdout correctly.
formatOutResult :: [[String]] -> String
formatOutResult outputTable = stringToOut
  where
    sortedTable = sort outputTable
    stringToOut = intercalate "\n" $ map (intercalate ",") sortedTable
