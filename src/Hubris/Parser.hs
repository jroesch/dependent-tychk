module Hubris.Parser (parseTopLevel, parseTerm) where

import Hubris.Syntax

import Control.Applicative
import Text.Trifecta
import Text.Parser.Token.Highlight
import Text.Parser.Token.Style
import Text.Parser.Char
import Text.Trifecta.Delta

parseTopLevel :: (Monad m, TokenParsing m) => m [Term String]
parseTopLevel = many parseTerm

parseTerm :: (Monad m, TokenParsing m) => m (Term String)
parseTerm = _hello
{-parseTerm = parseType-}
         {-<|> parseAscribe-}
         {-<|> Var <$> name-}

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

{-parseTerm :: (Monad m, TokenParsing m) => m (Term String)-}
{-parseTerm = do-}
   {-term <- parseTerm1-}
   {-char ':'-}
   {-ty <- parseTerm1-}
   {-return $ Ascribe term ty-}

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

