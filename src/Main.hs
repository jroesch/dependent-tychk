module Main where

import Bound
import Control.Applicative
import Data.List (elemIndex)
import Text.Trifecta
import Text.Parser.Token.Highlight
import Text.Parser.Token.Style
import Text.Parser.Char
import Text.Trifecta.Delta
import Data.Maybe (fromMaybe, isJust)
import Prelude.Extras
import System.Console.Haskeline
import qualified Data.Map as M

repl :: IO ()
repl = runInputT defaultSettings loop
    where loop :: InputT IO ()
          loop = do
              minput <- getInputLine "hubris> "
              case minput >>= parseCommand of
                  Just command -> do
                      outputStrLn ("Command: " ++ show command)
                      loop
                  Nothing -> do
                      let input = fromMaybe "" minput
                      let result = parseString parseTerm (Columns 0 0) input
                      case result of
                          Failure doc -> do
                              outputStrLn (show doc)
                              loop
                          Success a -> do
                              outputStrLn (show a)
                              loop

data Command = Quit
             | Help
             | Unknown String
             deriving (Eq, Show)

parseCommand :: String -> Maybe Command
parseCommand (':':cmd) =
    Just $ case cmd of
        "quit" -> Quit
        "help" -> Help
        s      -> Unknown s

parseCommand _ = Nothing

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

parseTopLevel :: (Monad m, TokenParsing m) => m [Term String]
parseTopLevel = many parseTerm

parseTerm :: (Monad m, TokenParsing m) => m (Term String)
parseTerm = parseType
         <|> parseAscribe
         <|> Var <$> name

{-parseSTerm :: (Monad m, TokenParsing m) => m STerm-}
{-parseSTerm =  Name <$> hsymbol-}
         {-<|> Numb <$> integer-}
         {-<|> between (symbol "(") (symbol ")") parseSeq-}

{-parseSeq :: (Monad m, TokenParsing m) => m STerm-}
{-parseSeq = fromList <$> parseSTerm `sepBy` whiteSpace-}

parseTerm0 :: (Monad m, TokenParsing m) => m (Term String)
parseTerm0 = _hole -- everything but `t : T`

parseTerm1 :: (Monad m, TokenParsing m) => m (Term String)
parseTerm1 = _myHole -- parseTerm0 ifnot parseTerm : parseTer0 ifnot parseTerm

parseTerm :: (Monad m, TokenParsing m) => m (Term String)
parseTerm = do
   term <- parseTerm1
   char ':'
   ty <- parseTerm1
   return $ Ascribe term ty

{-parseAscribe :: (Monad m, TokenParsing m) => m (Term String)-}
{-parseAscribe = do-}
    {-term <- parseTerm-}
    {-rest term-}
  {-where rest x = do-}
          {-char ':'-}
          {-ty <- parseTerm-}
          {-return $ Ascribe x ty-}

parseType :: (Monad m, TokenParsing m) => m (Term String)
parseType = do
  string "Type"
  return Type

name :: (CharParsing m) => m String
name = some (letter <|> digit)

main :: IO ()
main = repl


