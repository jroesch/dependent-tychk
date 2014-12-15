module Main where

import Bound
import Control.Applicative
import Data.List (elemIndex)
import Text.Trifecta
import Text.Parser.Token.Highlight
import Text.Parser.Token.Style
import Text.Parser.Char
import Text.Trifecta.Delta
import Data.Maybe (fromMaybe)
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
                      let result = parseString parseSTerm (Columns 0 0) input
                      case result of
                          Failure doc -> do
                              outputStrLn (show doc)
                              loop
                          Success a -> do
                              outputStrLn (show (eval emptyContext (translate a)))
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

data STerm = Cell STerm STerm
          | Name String
          | Numb Integer
          | Nil
          deriving (Show, Eq)

data Term a = Var a
           | Apply (Term a) (Term a)
           | Lam (Scope Int Term a)
           deriving (Eq, Ord, Show, Read)

instance Functor Term where
    fmap f (Var a) = Var (f a)
    fmap f (Apply fun arg) = Apply (fmap f fun) (fmap f arg)
    fmap f (Lam scope) = Lam (fmap f scope)

instance Monad Term where
    return = Var
    Var a     >>= f = f a
    Apply h g >>= f = Apply (h >>= f) (g >>= f)
    Lam scope >>= f = Lam (scope >>>= f)

instance Eq1 Term      where (==#)      = (==)
instance Ord1 Term     where compare1   = compare
instance Show1 Term    where showsPrec1 = showsPrec
instance Read1 Term    where readsPrec1 = readsPrec

translate :: STerm -> Term String
translate (Cell (Name "lambda") (Cell args body)) =
    let unpackedArgs = map ensureName $ toList args
        in Lam (abstract (`elemIndex` unpackedArgs) (translate body))
translate (Cell (Name v) Nil) = Var v
translate (Cell fun args) =
    let unpackedArgs = map translate $ toList args
        in buildApplicationChain (translate fun) unpackedArgs
translate (Name v) = Var v
translate e = error $ "can't translate: " ++ show e

buildApplicationChain :: Term String -> [Term String] -> Term String
buildApplicationChain fun [] = fun
buildApplicationChain fun (arg:args) =
    buildApplicationChain (Apply fun arg) args

ensureName :: STerm -> String
ensureName (Name n) = n
ensureName _ = error "bad"

fromList :: [STerm] -> STerm
fromList [] = Nil
fromList (x:xs) = Cell x (fromList xs)

toList :: STerm -> [STerm]
toList Nil = []
toList (Cell x xs) = (x : toList xs)

parseTopLevel :: (Monad m, TokenParsing m) => m [STerm]
parseTopLevel = many parseSTerm

parseSTerm :: (Monad m, TokenParsing m) => m STerm
parseSTerm =  Name <$> hsymbol
         <|> Numb <$> integer
         <|> between (symbol "(") (symbol ")") parseSeq

parseSeq :: (Monad m, TokenParsing m) => m STerm
parseSeq = fromList <$> parseSTerm `sepBy` whiteSpace

hsymbol :: (CharParsing m) => m String
hsymbol = some (letter <|> oneOf "->")

main :: IO ()
main = repl

data Context = Context (M.Map String ContextEntry)
             deriving (Eq, Show)

data ContextEntry = Assumption (Term String)
                  | Definition (Term String) (Term String)

emptyContext :: Context
emptyContext = Context (M.empty)

eval :: Context -> Term String -> Term String
eval ctxt e =
    case e of
        Lam scope -> Lam scope
        Apply f g ->
            call ctxt f g
        Var v -> Var v -- M.lookup ctxt v

call :: Context -> Term String -> Term String -> Term String
call ctxt f g =
    let (fun:args) = reverse $ g : collectArgs f
        in case fun of
            Lam scope ->
                instantiate (\i -> args !! i) scope
            other ->
                case eval ctxt other of
                    Lam scope ->
                        instantiate (\i -> args !! i) scope
                    _ -> error "can't evaluate it"

collectArgs :: Term String -> [Term String]
collectArgs (Apply f' g') = g' : collectArgs f'
collectArgs l @ (Lam _) = [l]
collectArgs v @ (Var _) = [v]
