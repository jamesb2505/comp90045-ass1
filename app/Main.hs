module Main (main) where

import RooLexer
import RooParser
import RooAST
import PrettyRoo

import System.Environment (getArgs, getProgName)
import System.Exit (ExitCode (..), exitWith)

parse :: String -> Either String Program
parse = runParser . (\(Right a) -> a) . runLexer
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
          let output = parse input
          case output of
            Right tree ->
              putStrLn (show tree)
            Left err ->
              do
                print err
                exitWith (ExitFailure 2)
      Pprint ->
        do
          let [_, filename] = args
          input <- readFile filename
          let output = parse input
          case output of
            Right tree ->
              putStrLn (pprint tree)
            Left err ->
              do
                print err
                exitWith (ExitFailure 2)

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
