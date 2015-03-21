module Hubris.TypeCheck where

import Bound
import Control.Monad.State
import qualified Data.Map as M

import Hubris.Syntax

-- The type of name we use for type checking.
type TyName = String

-- A convenience type for documentation purposes.
type Type = Term TyName

-- A term parametrized by TyName.
type TyTerm = Term TyName

-- A mapping of names to types (terms).
data Context = Context { nameMap :: M.Map TyName Type, counter :: Integer }

instance Show Context where
    show (Context nm _) = show nm

-- A context with no names, and types.
emptyContext :: Context
emptyContext = Context { nameMap = M.empty, counter = 0 }

-- A type error
data TypeErr = InferenceErr
             | NameErr String
             | MismatchErr TyTerm TyTerm
             | MiscErr String
             | UnimplementedErr String
             deriving (Show)

-- The monad we will use for type checking, we support state (StateT) and failure (Either).
type TypeCheck a = StateT Context (Either TypeErr) a

-- Typechecking takes a context Var -> Type, a Term to check
-- and will either return the type of the term, or an error.
typeCheck :: Context -> TyTerm -> Either TypeErr TyTerm
typeCheck ctxt tm = evalStateT (infer tm) ctxt

-- Returns both the typechecked term and the final context.
typeCheckWithContext :: Context -> TyTerm -> Either TypeErr (TyTerm, Context)
typeCheckWithContext ctxt tm = runStateT (infer tm) ctxt

-- We implement a bidirectional typechecker meaning we
-- have two forms of judgement.

-- This first of which `check` is a checking judgement
-- which asserts that a term checks to a certain type.

-- If we read the rules from LambdaPi we have two cases
-- that we must check, either we are executing the judgment
-- that a term must check with a certain type or we are checking
-- a lambda term annotated with a type.
check :: TyTerm -> TyTerm -> TypeCheck TyTerm
check (Lam scope) p @ (Pi t t') = do -- LAM
    x <- freshName
    bindName x t (check (instantiate1 (Var x) scope) (instantiate1 (Var x) t'))
    return p
check e t = do -- CHK
   infered <- infer e
   -- figure out how to do this
   case infered == t of
     False -> tyError $ MismatchErr e t
     True  -> return t

-- The second judgement is a inference judgement which
-- attempts to compute a type for term based on information.
-- We intermix these two judgements to type check a term.
infer :: TyTerm -> TypeCheck TyTerm
infer (Let x e _) = do -- handle body here
    -- (bindName x) `fmap` (infer e)
    -- lookupT x
    return $ error "let "
infer (Ascribe e p) = do -- ANN
    check p Type
    let t = eval p
    check e t
    return t
infer Type = return Type -- STAR
infer (Pi argT body) = do -- PI
  check argT Type
  let t = eval argT
  -- x <- freshName
  check (instantiate1 t body) Type
  return Type
infer (Var x) = lookupT x -- VAR
infer (Apply fun arg) = do -- APP
   funT <- infer fun
   (argT, body) <- case funT of
             Pi argT body -> return (argT, body)
             _ -> tyError $ MiscErr "failed in typing app"
   check arg argT
   return $ eval (instantiate1 arg body)
infer x = tyError $ MiscErr $ show x

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

-- Bind a name `n` with type `ty` in scope for `action`.
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

