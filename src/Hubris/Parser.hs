module Hubris.Parser (parseTopLevel, parseTerm) where

import Hubris.Syntax

import Control.Applicative
import Text.Trifecta
import Text.Parser.Token.Highlight
import Text.Parser.Token.Style
import Text.Parser.Char
import Text.Trifecta.Delta
import Data.List

import Bound

name :: (CharParsing m) => m String
name = some (letter <|> digit)

parseVar :: (Monad m, TokenParsing m) => m (Term String)
parseVar = Var <$> name <?> "variable"

parseApply :: (Monad m, TokenParsing m) => m (Term String)
parseApply = Apply <$> parseTerm <*> parseTerm

parseLam :: (Monad m, TokenParsing m) => m (Term String)
parseLam = do
  char '\\'
  var <- name
  body <- parseTerm
  return $ Lam $ abstract (`elemIndex` [var]) body

parseType :: (Monad m, TokenParsing m) => m (Term String)
parseType = do
  string "Type"
  return Type

parseAscribe :: (Monad m, TokenParsing m) => m (Term String)
parseAscribe = 
  parse1 = do
  term <- parseTerm0
  char ':'
  ty <- parseTerm0
  return $ Ascribe term ty

parsePi :: (Monad m, TokenParsing m) => m (Term String)
parsePi = do
  var <- name
  p <- parseTerm
  char ':'
  body <- parseTerm
  return $ Pi p $ abstract (`elemIndex` [var]) body

parseTerm0 :: (Monad m, TokenParsing m) => m (Term String)
parseTerm0 = parseType <|> parseVar-- <|> parseApply <|> parseLam <|> parsePi

parseTerm :: (Monad m, TokenParsing m) => m (Term String)
parseTerm = parseType <|> parseAscribe <|> parseVar-- <|> parseApply <|> parseLam <|> parseType <|> parsePi

parseTopLevel :: (Monad m, TokenParsing m) => m (Term String)
parseTopLevel = do
  -- terms <- many parseTerm
  term <- parseTerm
  eof
  return term

-- parseSeq :: (Monad m, TokenParsing m) => m Term
-- parseSeq = fromList <$> parseTerm `sepBy` whiteSpace
--
-- parseTerm0 :: (Monad m, TokenParsing m) => m (Term String)
-- parseTerm0 = _hole -- everything but `t : T`
--
-- parseTerm1 :: (Monad m, TokenParsing m) => m (Term String)
-- parseTerm1 = _myHole -- parseTerm0 ifnot parseTerm : parseTer0 ifnot parseTerm
--
-- parseTerm :: (Monad m, TokenParsing m) => m (Term String)
-- parseTerm = do
--    term <- parseTerm1
--    char ':'
--    ty <- parseTerm1
--    return $ Ascribe term ty
--
