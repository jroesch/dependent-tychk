{
module Hubris.Parser.Tokens where
}

%wrapper "basic"

$digit = 0-9			-- digits
$alpha = [a-zA-Z]		-- alphabetic characters

tokens :-

  $white+				                ;
  \-\>                          { \s -> TArrow }
  \\\/                          { \s -> TForall }
  \\                            { \s -> TLambda }
  let                           { \s -> TLet    }
  in                            { \s -> TIn     }
  $alpha [$alpha $digit \_ \']* { \s -> TVar s }
  :                             { \s -> TColon }
  \*                            { \s -> TStar }
  \.                            { \s -> TDot }
  \(                            { \s -> TLParens }
  \)                            { \s -> TRParens }
  =                             { \s -> TEq }

{
-- Each action has type :: String -> Token

-- The token type:
data Token = TLambda
           | TVar String
           | TColon
           | TArrow
           | TStar
           | TForall
           | TDot
           | TLParens
           | TRParens
           | TLet
           | TIn
           | TEq
	         deriving (Eq,Show)
}
