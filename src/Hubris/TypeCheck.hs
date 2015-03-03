module Hubris.TypeCheck where

import Bound
import Control.Monad.State
import qualified Data.Map as M

import Hubris.Syntax

type TyName = String

type Type = Term TyName
type TyTerm = Term TyName

data Context = Context { nameMap :: M.Map TyName Type, counter :: Integer }

instance Show Context where
    show (Context nm _) = show nm

emptyContext :: Context
emptyContext = Context { nameMap = M.empty, counter = 0 }

data TypeErr = InferenceErr
             | NameErr String
             | MismatchErr TyTerm TyTerm
             | MiscErr String
             | UnimplementedErr String
             deriving (Show)

type TypeCheck a = StateT Context (Either TypeErr) a

-- Typechecking takes a context Var -> Type, a Term to check
-- and will either return the type of the term, or an error.
typeCheck :: Context -> TyTerm -> Either TypeErr TyTerm
typeCheck ctxt tm = evalStateT (infer tm) ctxt

typeCheckWithContext :: Context -> TyTerm -> Either TypeErr (TyTerm, Context)
typeCheckWithContext ctxt tm = runStateT (infer tm) ctxt

-- We implement a bidirectional typechecker meaning we
-- have two forms of judgement.

-- This first of which `check` is a checking judgement
-- which asserts that a term checks to a certain type.
--
-- If we read the rules from LambdaPi we have two cases
-- that we must check, either we are executing the judgment
-- that a term must check with a certain type or we are checking
-- a lambda term annotated with a type.
check :: TyTerm -> TyTerm -> TypeCheck TyTerm
check (Lam scope) p @ (Pi t t') = do
    x <- freshName
    bindName x t (check (instantiate1 (Var x) scope) (instantiate1 (Var x) t'))
    return p
check e t = do
   infered <- infer e
   -- figure out how to do this
   unless (_quote)
     tyErr $ MistmatchErr _h _2
check _ _ = tyErr $ MiscErr "checking failed"

-- The second judgement is a inference judgement which
-- attempts to compute a type for term based on information.
-- We intermix these two judgements to type check a term.
infer :: TyTerm -> TypeCheck TyTerm
infer (Let x e _) = do -- handle body here
    -- (bindName x) `fmap` (infer e)
    -- lookupT x
    return $ error "a"
infer (Ascribe e p) = do
    check p Type
    let t = eval p
    check e t
    return t
infer Type = return Type
infer (Pi argT body) = do
  check argT Type
  let t = eval argT
  -- x <- freshName
  check (instantiate1 t body) Type
  return Type
infer (Var x) = lookupT x
infer (Apply fun arg) = do
   funT <- infer fun
   (argT, body) <- case funT of
             Pi argT body -> return (argT, body)
             _ -> tyError $ MiscErr "failed in typing app"
   check arg argT
   return $ eval (instantiate1 arg body)
infer _ = tyError InferenceErr

-- Attempt to unify two terms. For the time being
-- we require that these terms have the exact same
-- form. This probably doesn't work in general but
-- this will work for the time being.
unify :: TyTerm -> TyTerm -> TypeCheck TyTerm
unify (Var _) (Var _) =
unify t1 t2 = tyError $ MismatchErr t1 t2

-- Create a monadic type error.
tyError :: TypeErr -> TypeCheck a
tyError e = lift $ Left e

-- Generate a fresh name for type checking.
freshName :: TypeCheck TyName
freshName = do
   s <- get
   let tyName = "freshName___" ++ (show (counter s))
   put (s { counter = (counter s) + 1 })
   return tyName

-- Lookup the type corresponding to a name.
lookupT :: TyName -> TypeCheck TyTerm
lookupT n = do
    ctxt <- get
    case M.lookup n (nameMap ctxt) of
        Nothing -> tyError $ NameErr n
        Just t  -> return t

bindName :: TyName -> Type -> TypeCheck a -> TypeCheck a
bindName n ty action = do
    ctxt <- get
    put $ ctxt { nameMap = M.insert n ty (nameMap ctxt) }
    result <- action
    put ctxt
    return result

-- Typechecking and evaluation are intertwined so we must
-- defined the evaluation relation here.
eval :: Term a -> Term a
eval (Ascribe e _) = eval e
eval Type = Type
eval (Pi p scope) = Pi (eval p) (toScope $ eval $ fromScope scope)
eval v @ (Var _)  = v
eval (Apply e e') =
    case eval e of
      Lam scope -> eval (instantiate1 e' scope)
      n         -> Apply n (eval e')
eval (Lam scope) = Lam (toScope $ eval $ fromScope scope)

