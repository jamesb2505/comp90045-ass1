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
main = do
  progname <- getProgName
  args <- getArgs
  task <- checkArgs progname args
  case task of
    Parse -> doParse show args
    Pprint -> doParse pprint args

checkArgs :: String -> [String] -> IO Task
checkArgs _ ['-' : _] = 
  doErr ("Missing filename") 1
checkArgs _ ["-a", filename] =
  return Parse
checkArgs _ ["-p", filename] =
  return Pprint
checkArgs progname _ =
  doErr ("Usage: " ++ progname ++ " [-p] filename") 1

doParse f args = do
  let [_, filename] = args
  input <- readFile filename
  case runLexer input >>= runParser of
    Left err -> doErr err 2
    Right parsed -> putStrLn $ f parsed

doErr s i = putStrLn s >> exitWith (ExitFailure i)