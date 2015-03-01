{
module Hubris.Parser.Internal (parseTerm) where

import Hubris.Syntax
import Hubris.Parser.Tokens

import Bound
import Data.List
}

%name parseTokens
%tokentype { Token }
%monad { Either String }
%error { parseError }

%token
      lambda          { TLambda }
      forall          { TForall }
      let             { TLet    }
      var             { TVar $$ }
      in              { TIn   }
      '*'             { TStar }
      ':'             { TColon }
      arrow           { TArrow }
      '.'             { TDot }
      '('             { TLParens }
      ')'             { TRParens }
      '='             { TEq }
%%

Term : Term0 ':' Term0 { Ascribe $1 $3 }
     | Term0 Term0 { Apply $1 $2 }

Term0 : '*' { Type }
      | forall var ':' Term '.' Term { Pi $4 (abstract (`elemIndex` [$2]) $6) }
      | let var '=' Term LetExpr { Let $2 $4 $5 }
      | var { Var $1 }
      | lambda var arrow Term { Lam (abstract (`elemIndex` [$2]) $4) }
      | '(' Term ')' { $2 }

LetExpr : in Term { Just $2 }
        | { Nothing }

{
parseError tkns = Left $ show tkns

parseTerm = parseTokens . alexScanTokens
}
