module Main where

import Bound
import Data.List (elemIndex)
import Data.Maybe (fromMaybe, isJust)
import Prelude.Extras
import System.Console.Haskeline
import qualified Data.Map as M
import Hubris.Parser
import Hubris.Syntax

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
                      let result = parseTerm input
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


