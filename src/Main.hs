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
-- import Hubris.Parser
import Hubris.Syntax
import Hubris.Grammer
import Hubris.Tokens

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
                      -- let result = parseString parseTopLevel (Columns 0 0) input
                      let result = parseGrammer input
                      case result of
                          Left doc -> do
                              outputStrLn $ "Error! Tokens left: " ++ doc
                              loop
                          Right a -> do
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

main :: IO ()
main = repl


