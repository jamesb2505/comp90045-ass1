module Main (main) where

import RooParser (runParser)
import RooLexer (runLexer)
import RooAST (Program)
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
  doErr 1 "Missing filename"
checkArgs _ ["-a", filename] =
  return Parse
checkArgs _ ["-p", filename] =
  return Pprint
checkArgs progname _ =
  doErr 1 $ "Usage: " ++ progname ++ " [-p] filename"

doParse :: (Program -> String) -> [String] -> IO ()
doParse f args = do
  let [_, filename] = args
  input <- readFile filename
  case runLexer input >>= runParser of
    Left err -> doErr 2 err
    Right parsed -> putStrLn $ f parsed

doErr :: Int -> String -> IO a
doErr code s = putStrLn s >> exitWith (ExitFailure code)
