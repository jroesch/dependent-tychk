module Hubris.Pretty where

import Hubris.Syntax
import Text.PrettyPrint
import Bound
import Data.Traversable

pretty :: Term String -> Doc
pretty a = fst $ pretty' (map Var vars) a
  where
    vars = [c : s | s <- [] : vars, c <- ['a'..'z'] ]
    pretty' vars (Ascribe a b) = (parens str1 <> text " : " <> parens str2, vars2)
      where
        (str1, vars1) = pretty' vars a
        (str2, vars2) = pretty' vars1 b
    pretty' vars (Pi a s) = (v <> text " : " <> ty <> text " . " <> body, vars2)
      where
        (v, _) = pretty' vars (head vars)
        (ty, vars1) = pretty' (tail vars) a
        (body, vars2) = pretty' vars1 $ instantiate1 (head vars) s
    pretty' vars (Var x) = (text x, vars)
    pretty' vars (Apply a b) = (str1 <> str2, vars2)
      where
        (str1, vars1) = pretty' vars a 
        (str2, vars2) = pretty' vars b
    pretty' vars (Lam s) = (char 'λ' <> v <> text " → " <> body, vars1)
      where
        (v, _) = pretty' vars (head vars)
        (body, vars1) = pretty' (tail vars) $ instantiate1 (head vars) s
    pretty' vars (Let a b (Just c)) = (text "let" <> text a <> text " = " <> body <> text "in" <> rest, vars2)
      where
        (body, vars1) = pretty' vars b 
        (rest, vars2) = pretty' vars1 c
    pretty' vars (Let a b Nothing) = (text "let" <> text a <> text " = " <> body, vars1)
      where
        (body, vars1) = pretty' vars b
    pretty' vars Type = (char '*', vars)
