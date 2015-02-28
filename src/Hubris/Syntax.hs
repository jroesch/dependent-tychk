module Hubris.Syntax where

import Bound
import Control.Applicative
import Prelude.Extras

data Term a = Ascribe (Term a) (Term a)
            | Type
            | Pi (Term a) (Scope Int Term a)
            | Var a
            | Apply (Term a) (Term a)
            | Lam (Scope Int Term a)
            deriving (Eq, Ord, Show, Read)

instance Functor Term where
    fmap f (Var a) = Var (f a)
    fmap f (Apply fun arg) = Apply (fmap f fun) (fmap f arg)
    fmap f (Lam scope) = Lam (fmap f scope)

instance Applicative Term where
    pure = return
    fa <*> v = do
      f <- fa
      a <- v
      return (f a)

instance Monad Term where
    return = Var
    Var a     >>= f = f a
    Apply h g >>= f = Apply (h >>= f) (g >>= f)
    Lam scope >>= f = Lam (scope >>>= f)

instance Eq1 Term      where (==#)      = (==)
instance Ord1 Term     where compare1   = compare
instance Show1 Term    where showsPrec1 = showsPrec
instance Read1 Term    where readsPrec1 = readsPrec

