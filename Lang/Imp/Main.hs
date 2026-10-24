module Main where

import Data.List (intercalate)
import qualified Data.Map.Strict as M
import Lang.Imp.Ast
import Lang.Imp.Interpreter
import Lang.Imp.Parser
import System.Environment (getArgs)

-- Turns a single pair like ("m", 6) into the string "\"m\" -> 6"
showPair :: (Name, Integer) -> String
showPair (k, v) = show k ++ " -> " ++ show v

-- Turns the whole State map into a multi-line string
showState :: State -> String
showState st = intercalate "\n" (map showPair (M.toList st))

-- Splits an argument like "m=6" at the '=' sign into ("m", 6)
parseVar :: String -> (Name, Integer)
parseVar s = case break (== '=') s of
  (name, '=' : valStr) -> (name, read valStr)
  _ -> error ("Invalid argument: " ++ s)

main :: IO ()
main = do
  -- 1. Pattern match on the list of CLI args: 
  --    first arg is the file, the rest are ["m=6", "n=2", ...]
  (fileName : varArgs) <- getArgs
  content <- readFile fileName

  -- 2. Parse each "var=val" string and build the starting State map
  let inputState = M.fromList (map parseVar varArgs)
  putStrLn "Input State:"
  putStrLn (showState inputState)

  -- 3. Parse the Imp file and, if successful, run the interpreter
  case parseImp content of
    Left err -> putStrLn err
    Right cmd -> case runState (exec cmd) inputState of
      Left runtimeErr -> putStrLn runtimeErr
      Right (resultState, _) -> do
        putStrLn "Result State:"
        putStrLn (showState resultState)