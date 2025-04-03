-- Karso: this is the basic draft of lexer, you guys can make some modification based on it 
-- One example i can think of:

{
module Tokens where
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
  "cartesian"                  { \p s -> TokenCartesian p } -- for task 1
  "permutation"                { \p s -> TokenPermutation p } -- for task 2
  "existence"                  { \p s -> TokenExistence p } -- for task 3
  "output"                     { \p s -> TokenOutput p}

  

 
 -- I realised I have to use \"A.csv\" if i want a double quote since its the same as Haskell, so i just simply removed double quote to avoid unexpected errors 
 -- Also this is some sort of rules for the csv format
 [$alpha $digit \_ \-]+\.csv      { \p s -> TokenFilename p s } -- example: readFile(A.csv)

  -- Operators and punctuation
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
 
  TokenLet AlexPosn        |
  TokenIf AlexPosn         |
  TokenElse AlexPosn       |
  TokenReadFile AlexPosn   |
  TokenCartesian AlexPosn |
  TokenPermutation AlexPosn |
  TokenExistence AlexPosn |
  TokenFilename AlexPosn String |
  TokenInt AlexPosn Int    |
  TokenVar AlexPosn String |
  TokenEq AlexPosn         |
  TokenEquals AlexPosn     |
  TokenNotEquals AlexPosn  |
  TokenLParen AlexPosn     |
  TokenRParen AlexPosn     |
  TokenLBrace AlexPosn     |
  TokenRBrace AlexPosn     |
  TokenOutput AlexPosn     |
  TokenComma AlexPosn      
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
}
