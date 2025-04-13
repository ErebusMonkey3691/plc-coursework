-- Karso: Basically finished the parser, but please check if there's any mistakes / stuff to add / delete
-- Add notes here (or discord / github) if you have modify anything

-- To do: my + - * / ++ need restriction to avoid potential errors 

{ 
module Grammar where 
import Lexer 
}

%name parser
%tokentype { Token } 
%error { parseError }
%token 
    let         { TokenLet _ } 
    if          { TokenIf _ }
    else        { TokenElse _ }
    readFile    { TokenReadFile _ }
    cartesian   { TokenCartesian _ }
    permutation { TokenPermutation _ }
    existence   { TokenExistence _ }
    output      { TokenOutput _ }
    leftMerge   { TokenLeftMerge _ }
    constant    { TokenConstant _ }
    duplicate   { TokenDuplicate _ }
    string      { TokenString _ $$ }
    filename    { TokenFilename _ $$ }
    int         { TokenInt _ $$ }
    var         { TokenVar _ $$ }
    '='         { TokenEq _ }
    '=='        { TokenEquals _ }
    '!='        { TokenNotEquals _ }
    '('         { TokenLParen _ } 
    ')'         { TokenRParen _ } 
    '{'         { TokenLBrace _ }
    '}'         { TokenRBrace _ }
    ','         { TokenComma _ }
    '+'         { TokenAddition _ }
    '-'         { TokenSubstraction _ }
    '++'        { TokenConcatenation _ }
    '/'         { TokenDivision _ }
    '*'         { TokenMultiplication _ }

%right '='
%nonassoc if
%nonassoc else
%left '==' '!='
%left '*' '/'
%left '+' '-'
%left '++'

%% 

-- Basically :
-- Program have two non-terminals statement where you can do (let ... / output ... / if else) and you can keep looping it (Statement Program)
-- And Expr basically just something like the combination of Java and PLC labs, with functions for our tasks  

Program : Statement                { [$1] }
        | Statement Program        { $1 : $2 }


Statement : Assignment             { $1 }
          | OutputStmt             { $1 }
          | IfElseStmt             { $1 }


Assignment : let var '=' Expr      { Assignment $2 $4 }

OutputStmt : output '(' Expr ')'   { Output $3 }

IfElseStmt : if '(' Expr ')' '{' Program '}' else '{' Program '}'           { IfElse $3 $6 $10 }
                                  
           | if '(' Expr ')' '{' Program '}'                                { If $3 $6 }
                                 
-- I might have to separate the Expr (not sure) or user can do some carzy shit (for example cartesian(permutation, existence)) 
-- We can do that as an extension if we can finish the basic part of it, but rn i am gonna separate it

Expr : var                         { Variable $1 }
     | string                      { String (removeQuotes $1) } -- For our lovely language to remove ' ' 
     | int                         { IntLiteral $1 }
     | filename                    { Filename $1 }
     | ReadFileExpr                { $1 }
     | CartesianExpr               { $1 }
     | PermutationExpr             { $1 }
     | ExistenceExpr               { $1 }
     | LeftMergeExpr               { $1 }
     | ConstantExpr                { $1 }
     | DuplicateExpr               { $1 }
     | Expr '+' Expr               { Addition $1 $3 }
     | Expr '-' Expr               { Subtraction $1 $3 }
     | Expr '*' Expr               { Multiplication $1 $3 }
     | Expr '/' Expr               { Division $1 $3 }
     | string '++' string          { Concatenation (removeQuotes $1) (removeQuotes $3) }
     | '(' Expr ')'                { $2 }

-- This is for cartesian, cuz i think we need to split it out (?) Aaron, Dylan remember to double check thissssssssssssss
TableList : TableExpr                          { [$1] }
          | TableExpr ',' TableList            { $1 : $3 }

TableExpr : var                                { Variable $1 }
          | filename                           { Filename $1 }
          | ReadFileExpr                       { $1 }
          | '(' TableExpr ')'                  { $2 }


-- readFile operation
ReadFileExpr : readFile '(' filename ')'            { ReadFile $3 }     -- For example readFile(A.csv)
             | readFile '(' var ')'                 { ReadFileVar $3 }  -- For example readFile(A), where A is a variable (do we need this ?) 

-- Task 1
CartesianExpr : cartesian '(' TableList ')'          { Cartesian $3 } -- We can support N-way, but I think if input is only 1 csv file then it should output empty ?

-- Task 2
PermutationExpr : permutation '(' Expr ')'          { Permutation $3 } -- Only one csv file from what we saw in the spec

-- Task 3  
ExistenceExpr : existence '(' Expr ')'              { Existence $3 } -- Only one csv file from what we saw in the spec

-- Task 4
ConstantExpr : constant '(' string ')'              { Constant (removeQuotes $3) } -- Only one csv file from what we saw in the spec

-- Task 4
DuplicateExpr : duplicate '(' Expr ',' Expr ')'     { Duplicate $3 $5 } 

-- Task 5
LeftMergeExpr : leftMerge '(' Expr ',' Expr ')'     { LeftMerge $3 $5 }

-- A list of comma-separated expressions
ExprList : Expr                   { [$1] }
         | Expr ',' ExprList      { $1 : $3 }

{ 
-- From lab 
parseError :: [Token] -> a
parseError [] = error "Unknown Parse Error" 
parseError (t:ts) = error ("Parse error at line:column " ++ (tokenPosn t))

-- Helper function to remove quotes from string literals
removeQuotes :: String -> String
removeQuotes s = case s of
    ('\'':rest) -> init rest
    _ -> s

data Program = Program [Statement]
  deriving (Show, Eq)

data Statement 
  = Assignment String Expr
  | Output Expr
  | IfElse Expr [Statement] [Statement]
  | If Expr [Statement]
  deriving (Show, Eq)

data Expr
  = Variable String
  | String String
  | IntLiteral Int
  | Filename String
  | ReadFile String
  | ReadFileVar String
  | Cartesian [Expr]
  | Permutation Expr
  | Existence Expr
  | LeftMerge Expr Expr
  | Constant String
  | Duplicate Expr Expr
  | Addition Expr Expr
  | Subtraction Expr Expr
  | Multiplication Expr Expr
  | Division Expr Expr
  | Concatenation String String
  deriving (Show, Eq)
}