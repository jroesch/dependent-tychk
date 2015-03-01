{
module Hubris.Grammer (parseGrammer) where

import Hubris.Syntax
import Hubris.Tokens

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
      var             { TVar $$ }
      '*'             { TStar }
      ':'             { TColon }
      arrow           { TArrow }
      '.'             { TDot }
      '('             { TLParens }
      ')'             { TRParens }

%%

Term : Term0 ':' Term0 { Ascribe $1 $3 }
     | Term0 Term0 { Apply $1 $2 }

Term0 : '*' { Type }
      | forall var ':' Term '.' Term { Pi $4 (abstract (`elemIndex` [$2]) $6) }
      | var { Var $1 }
      | lambda var arrow Term { Lam (abstract (`elemIndex` [$2]) $4) }
      | '(' Term ')' { $2 }

{
parseError tkns = Left $ show tkns

parseGrammer = parseTokens . alexScanTokens
}
