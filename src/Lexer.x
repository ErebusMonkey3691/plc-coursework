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

  "let"                        { \p s -> TokenLet p } -- for example let A = readFile ("A.csv")
  "if"                         { \p s -> TokenIf p }
  "else"                       { \p s -> TokenElse p }
  "readFile"                   { \p s -> TokenReadFile p } -- Should be for both tasks
  "cartesian"                  { \p s -> TokenCartesian p } -- for task 1 (we should also consider doing a N-way)
  "permutation"                { \p s -> TokenPermutation p } -- for task 2
  "existence"                  { \p s -> TokenExistence p } -- for task 3
  "output"                     { \p s -> TokenOutput p }
  "leftMerge"                  { \p s -> TokenLeftMerge p } -- used to define a left merge on two colomns (task 5)
  "constant"                   { \p s -> TokenConstant p } -- for task 4
  "duplicate"                  { \p s -> TokenDuplicate p } -- for task 4
  \'.*\'                       { \p s -> TokenString p s } -- allow for inserting of just raw strings (without need of variable or allowing variable assignment?) (task 4)
  

  -- "foo" is a fixed string, but i am not sure will Nick trick us in submission 2 (if he suddenly want a different string), 
  -- probably use a more general function (like constant token) to do this
  

 
 -- I realised I have to use \"A.csv\" if i want a double quote since its the same as Haskell, so i just simply removed double quote to avoid unexpected errors 
 -- Also this is some sort of rules for the csv format
 [$alpha $digit \_ \-]+\.csv      { \p s -> TokenFilename p s } -- example: readFile(A.csv)

  -- Operators and punctuation (some of it i am not sure do we need it but i will put it there first)
  "+"                          { \p s -> TokenAddition p }
  "-"                          { \p s -> TokenSubstraction p }
  "++"                         { \p s -> TokenConcatenation p }
  "*"                          { \p s -> TokenMultiplication p }
  "/"                          { \p s -> TokenDivision p }
  "="                          { \p s -> TokenEq p }
  "=="                         { \p s -> TokenEquals p }
  "!="                         { \p s -> TokenNotEquals p }
  "("                          { \p s -> TokenLParen p }
  ")"                          { \p s -> TokenRParen p }
  "{"                          { \p s -> TokenLBrace p } -- Not sure do we need this, optional
  "}"                          { \p s -> TokenRBrace p } -- Not sure do we need this, optional
  ","                          { \p s -> TokenComma p } -- For example outputCartesian(A,B)

  $digit+                     { \p s -> TokenInt p (read s) }
  $alpha [$alpha $digit \_ \’]*   { \p s -> TokenVar p s } 

{
 
data Token = 
 
  TokenLet AlexPosn                       |
  TokenIf AlexPosn                        |
  TokenElse AlexPosn                      |
  TokenReadFile AlexPosn                  |
  TokenCartesian AlexPosn                 |
  TokenPermutation AlexPosn               |
  TokenExistence AlexPosn                 |
  TokenFilename AlexPosn String           |
  TokenInt AlexPosn Int                   |  
  TokenVar AlexPosn String                |
  TokenEq AlexPosn                        |
  TokenEquals AlexPosn                    |
  TokenNotEquals AlexPosn                 |
  TokenLParen AlexPosn                    |
  TokenRParen AlexPosn                    |
  TokenLBrace AlexPosn                    |
  TokenRBrace AlexPosn                    |
  TokenOutput AlexPosn                    |
  TokenComma AlexPosn                     |
  TokenLeftMerge AlexPosn                 |
  TokenConstant AlexPosn                  |
  TokenDuplicate AlexPosn                 |       
  TokenAddition AlexPosn                  |
  TokenConcatenation AlexPosn             |
  TokenMultiplication AlexPosn            |
  TokenDivision AlexPosn                  |
  TokenSubstraction AlexPosn              |
  TokenString AlexPosn String
  deriving (Eq,Show) 


tokenPosn :: Token -> String
tokenPosn (TokenLet (AlexPn a l c)) = show(l) ++ ":" ++ show(c)
tokenPosn (TokenIf (AlexPn a l c)) = show(l) ++ ":" ++ show(c)
tokenPosn (TokenElse (AlexPn a l c)) = show(l) ++ ":" ++ show(c)
tokenPosn (TokenReadFile (AlexPn a l c)) = show(l) ++ ":" ++ show(c)
tokenPosn (TokenCartesian (AlexPn a l c)) = show(l) ++ ":" ++ show(c)
tokenPosn (TokenPermutation (AlexPn a l c)) = show(l) ++ ":" ++ show(c)
tokenPosn (TokenExistence (AlexPn a l c)) = show(l) ++ ":" ++ show(c)
tokenPosn (TokenFilename (AlexPn a l c) x) = show(l) ++ ":" ++ show(c)
tokenPosn (TokenInt (AlexPn a l c) n) = show(l) ++ ":" ++ show(c)
tokenPosn (TokenVar (AlexPn a l c) x) = show(l) ++ ":" ++ show(c)
tokenPosn (TokenEq (AlexPn a l c)) = show(l) ++ ":" ++ show(c)
tokenPosn (TokenEquals (AlexPn a l c)) = show(l) ++ ":" ++ show(c)
tokenPosn (TokenNotEquals (AlexPn a l c)) = show(l) ++ ":" ++ show(c)
tokenPosn (TokenLParen (AlexPn a l c)) = show(l) ++ ":" ++ show(c)
tokenPosn (TokenRParen (AlexPn a l c)) = show(l) ++ ":" ++ show(c)
tokenPosn (TokenLBrace (AlexPn a l c)) = show(l) ++ ":" ++ show(c)
tokenPosn (TokenRBrace (AlexPn a l c)) = show(l) ++ ":" ++ show(c)
tokenPosn (TokenComma (AlexPn a l c)) = show(l) ++ ":" ++ show(c)
tokenPosn (TokenOutput (AlexPn a l c)) = show(l) ++ ":" ++ show(c)
tokenPosn (TokenLeftMerge (AlexPn a l c)) = show(l) ++ ":" ++ show(c)
tokenPosn (TokenConstant (AlexPn a l c)) = show(l) ++ ":" ++ show(c)
tokenPosn (TokenDuplicate (AlexPn a l c)) = show(l) ++ ":" ++ show(c)
tokenPosn (TokenAddition (AlexPn a l c)) = show(l) ++ ":" ++ show(c)
tokenPosn (TokenSubstraction (AlexPn a l c)) = show(l) ++ ":" ++ show(c)
tokenPosn (TokenMultiplication (AlexPn a l c)) = show(l) ++ ":" ++ show(c)
tokenPosn (TokenDivision (AlexPn a l c)) = show(l) ++ ":" ++ show(c)
}
