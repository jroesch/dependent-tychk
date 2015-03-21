module Main where

import Bound
import Data.List (elemIndex)
import Data.Maybe (fromMaybe, isJust)
import Prelude.Extras
import System.Console.Haskeline
import qualified Data.Map as M
import Hubris.Parser
import Hubris.Syntax
import Hubris.TypeCheck
import Hubris.Pretty
import Control.Monad.Trans (lift)

repl :: IO ()
repl = runInputT settings (loop emptyContext)
    where
      settings = defaultSettings {historyFile = Just ".hubris_history"}
      loop :: Context -> InputT IO ()
      loop ctxt = do
          minput <- getInputLine "hubris> "
          case minput >>= parseCommand of
              Just (Load file) -> do
                input <- lift $ readFile file
                outputStrLn file
                outputStrLn input
                runProgram input
                loop ctxt
              Just command -> do
                  outputStrLn ("Command: " ++ show command)
                  loop ctxt
              Nothing -> do
                  runProgram $ fromMaybe "" minput
                  loop ctxt
      runProgram s = case parseTerm s of
                       Left doc -> do
                           outputStrLn $ "Error! Tokens left: " ++ doc
                       Right term -> do
                           outputStrLn $ "Parsed: " ++ (show $ pretty term)
                           case typeCheckWithContext emptyContext term of
                             Left e -> do
                               outputStrLn ("TypeError: " ++ (show e))
                             Right (result, ctxt') -> do
                               outputStrLn $ "Typecheck: " ++ (show $ pretty term)


data Command = Quit
             | Help
             | Load String
             | Unknown String
             deriving (Eq, Show)

parseCommand :: String -> Maybe Command
parseCommand (':':cmd) =
    Just $ case cmd of
        "quit"                 -> Quit
        "help"                 -> Help
        'l':'o':'a':'d':' ':xs -> Load xs
        s                      -> Unknown s
parseCommand _ = Nothing

main :: IO ()
main = repl
