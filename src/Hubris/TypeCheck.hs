module Hubris.TypeCheck where

import Bound
import Control.Monad.State
import qualified Data.Map as M

import Hubris.Syntax

type TyName = String

type Type = Term TyName
type TyTerm = Term TyName

type Context = M.Map TyName Type

emptyContext :: Context
emptyContext = M.empty

data TypeErr = InferenceErr
             | MismatchErr TyTerm TyTerm
             | MiscErr String
             deriving (Show)

type TypeCheck a = StateT Context (Either TypeErr) a

typeCheck :: Context -> Term String -> Either TypeErr TyTerm
typeCheck ctxt tm = evalStateT (infer tm) ctxt

check :: TyTerm -> TyTerm -> TypeCheck TyTerm
check e t = do
   infered <- infer e
   unify t infered

infer :: TyTerm -> TypeCheck TyTerm
infer (Ascribe e p) = do
    check p Type
    let t = eval p
    check e t
    return t
infer _ = tyError $ MiscErr "not supported"

unify :: TyTerm -> TyTerm -> TypeCheck TyTerm
unify t1 t2
  | t1 == t2 = return t1
  | otherwise = tyError $ MismatchErr t1 t2

tyError :: TypeErr -> TypeCheck a
tyError e = lift $ Left e

eval = id

-- Typechecking and evaluation are intertwined so we must
-- defined the evaluation relation here.
{-eval :: Term a -> Term a-}
{-eval (Ascribe e _) = eval e-}
{-eval Type = Type-}
{-eval (Pi p scope) = Pi (eval p) (scope >>>= eval)-}
{-eval v @ (Var _)  = v-}
{-eval (Apply e e') =-}
    {-case eval e of-}
      {-Lam scope -> eval (instantiate _somethign e')-}
      {-n         -> Apply n (eval e')-}
{-eval (Lam scope)   = Lam (scope >>>= eval)-}

