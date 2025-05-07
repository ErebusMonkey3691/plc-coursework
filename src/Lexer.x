-- Karso: this is the basic draft of lexer, you guys can make some modification based on it 
-- One example i can think of:

{
module Lexer where
}

%wrapper "posn" 

$digit  = 0-9     
$alpha  = [a-zA-Z]

tokens :-
  $white+                           ;  -- skip whitespace
  "--".*                            ;  -- comment like this one 

  "let"                        { \p s -> PT p TokenLet } -- for example let A = readFile ("A.csv")
  "if"                         { \p s -> PT p TokenIf }
  "else"                       { \p s -> PT p TokenElse }
  "readFile"                   { \p s -> PT p TokenReadFile } -- Should be for both tasks
  "cartesian"                  { \p s -> PT p TokenCartesian } -- for task 1 (we should also consider doing a N-way)
  "permutation"                { \p s -> PT p TokenPermutation } -- for task 2
  "existence"                  { \p s -> PT p TokenExistence } -- for task 3
  "output"                     { \p s -> PT p TokenOutput }
  "leftMerge"                  { \p s -> PT p TokenLeftMerge } -- used to define a left merge on two colomns (task 5)
  "constant"                   { \p s -> PT p TokenConstant } -- for task 4
  "duplicate"                  { \p s -> PT p TokenDuplicate } -- for task 4
  \'.*\'                       { \p s -> PT p (TokenString s) } -- allow for inserting of just raw strings (without need of variable or allowing variable assignment?) (task 4)
  
  

  -- "foo" is a fixed string, but i am not sure will Nick trick us in submission 2 (if he suddenly want a different string), 
  -- probably use a more general function (like constant token) to do this
  

 
 -- I realised I have to use \"A.csv\" if i want a double quote since its the same as Haskell, so i just simply removed double quote to avoid unexpected errors 
 -- Also this is some sort of rules for the csv format
 [$alpha $digit \_ \- \/ \\]+\.csv      { \p s -> PT p (TokenFilename s) } -- example: readFile(A.csv)

  -- Operators and punctuation (some of it i am not sure do we need it but i will put it there first)
  -- "["$digit+"]"                { \p s -> PT p (TokenIndex s)}
  "+"                          { \p s -> PT p TokenAddition }
  "-"                          { \p s -> PT p TokenSubstraction }
  "++"                         { \p s -> PT p TokenConcatenation }
  "*"                          { \p s -> PT p TokenMultiplication }
  "/"                          { \p s -> PT p TokenDivision }
  "="                          { \p s -> PT p TokenEq }
  "=="                         { \p s -> PT p TokenEquals }
  "!="                         { \p s -> PT p TokenNotEquals }
  "("                          { \p s -> PT p TokenLParen }
  ")"                          { \p s -> PT p TokenRParen }
  "{"                          { \p s -> PT p TokenLBrace } -- Not sure do we need this, optional
  "}"                          { \p s -> PT p TokenRBrace } -- Not sure do we need this, optional
  ","                          { \p s -> PT p TokenComma } -- For example outputCartesian(A,B)
  "["                          { \p s -> PT p TokenLSquare }
  "]"                          { \p s -> PT p TokenRSquare }

-- PT 
  $digit+                         { \p s -> PT p (TokenInt (read s)) }
  $alpha [$alpha $digit \_ \’]*   { \p s -> PT p (TokenVar s) } 

{
 
data PosnToken = PT AlexPosn Token deriving (Eq, Show)

data Token = 
 
  TokenLet                       |
  TokenIf                        |
  TokenElse                      |
  TokenReadFile                  |
  TokenCartesian                 |
  TokenPermutation               |
  TokenExistence                 |
  TokenFilename String           |
  TokenInt Int                   |  
  TokenVar String                |
  TokenEq                        |
  TokenEquals                    |
  TokenNotEquals                 |
  TokenRParen                    |
  TokenLParen                    |
  TokenLBrace                    |
  TokenRBrace                    |
  TokenOutput                    |
  TokenComma                     |
  TokenLeftMerge                 |
  TokenConstant                  |
  TokenDuplicate                 |       
  TokenAddition                  |
  TokenConcatenation             |
  TokenMultiplication            |
  TokenDivision                  |
  TokenSubstraction              |
  TokenString String             |
  TokenLSquare                   |
  TokenRSquare                   
  deriving (Eq,Show) 

}
