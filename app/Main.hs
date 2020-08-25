module Main (main) where

import RooParser (runParser)
import RooLexer (runLexer)
import PrettyRoo (pprint)
import System.Environment (getArgs, getProgName)
import System.Exit (ExitCode(..), exitWith)

data Task
  = Parse
  | Pprint
  deriving (Eq, Show)

main :: IO ()
main =
  do
    progname <- getProgName
    args <- getArgs
    task <- checkArgs progname args
    case task of
      Parse ->
        do
          let [_, filename] = args
          input <- readFile filename
          doParse show input
      Pprint ->
        do
          let [_, filename] = args
          input <- readFile filename
          doParse pprint input

checkArgs :: String -> [String] -> IO Task
checkArgs _ ['-' : _] =
  do
    putStrLn ("Missing filename")
    exitWith (ExitFailure 1)
checkArgs _ ["-a", filename] =
  return Parse
checkArgs _ ["-p", filename] =
  return Pprint
checkArgs progname _ =
  do
    putStrLn ("Usage: " ++ progname ++ " [-p] filename")
    exitWith (ExitFailure 1)

doParse f input =
  case runLexer input >>= runParser of
    Left err -> doErr err 2
    Right parsed -> putStrLn $ f parsed

doErr s i = putStrLn s >> exitWith (ExitFailure i)