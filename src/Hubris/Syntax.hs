module Hubris.Syntax where

import Bound
import Control.Applicative
import Prelude.Extras

data Term a = Ascribe (Term a) (Term a)       -- e : T
            | Type                            -- Type
            | Pi (Term a) (Scope () Term a)   -- (x : A) => e
            | Var a                           -- x
            | Apply (Term a) (Term a)         -- e e'
            | Lam (Scope () Term a)           -- \x -> e
            | Let a (Term a) (Maybe (Term a)) -- let e = x ?(in t)
            deriving (Eq, Ord, Show, Read)

instance Functor Term where
    fmap f (Var a)         = Var (f a)
    fmap f (Apply fun arg) = Apply (fmap f fun) (fmap f arg)
    fmap f (Lam scope)     = Lam (fmap f scope)
    fmap f (Pi ty scope)    = Pi (fmap f ty) (fmap f scope)
    fmap f Type            = Type
    fmap f (Ascribe e t)   = Ascribe (fmap f e) (fmap f t)

instance Applicative Term where
    pure = return
    fa <*> v = do
      f <- fa
      a <- v
      return (f a)

instance Monad Term where
    return = Var
    Var a       >>= f = f a
    Apply h g   >>= f = Apply (h >>= f) (g >>= f)
    Lam scope   >>= f = Lam (scope >>>= f)
    Pi ty scope >>= f = Pi (ty >>= f) (scope >>>= f)
    Type        >>= f = Type
    Ascribe e t >>= f = Ascribe (e >>= f) (t >>= f)

instance Eq1 Term      where (==#)      = (==)
instance Ord1 Term     where compare1   = compare
instance Show1 Term    where showsPrec1 = showsPrec
instance Read1 Term    where readsPrec1 = readsPrec

