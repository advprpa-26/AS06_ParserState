module Main where

import Data.List (intercalate)
import qualified Data.Map.Strict as M
import Lang.Imp.Ast
import Lang.Imp.Interpreter
import Lang.Imp.Parser
import System.Environment (getArgs)

parseVar :: String -> (String, Integer)
parseVar s =
  let (name, '=' : value) = span (/= '=') s
   in (name, read value)

printState :: State -> IO ()
printState st =
  mapM_
    (\(n,v) -> putStrLn (show n ++ " -> " ++ show v))
    (M.toList st)

main :: IO ()
main = do
  args <- getArgs

  let fileName = head args
  let vars = tail args

  content <- readFile fileName

  let inputState = M.fromList (map parseVar vars)

  case parseImp content of
    Left err ->
      putStrLn err

    Right prog ->
      case runState (exec prog) inputState of

        Left err ->
          putStrLn err

        Right (resultState, ()) -> do
          putStrLn "Input State:"
          printState inputState

          putStrLn "Result State:"
          printState resultState