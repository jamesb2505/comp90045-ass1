module Main (main) where

-------------------------
-- Roo Language Compiler
--
-- Command line arguments:
--  * -p <filename> 
--      "pretty print"s a given roo program, printing lexical and parsing
--      errors
--  * -a <filename>
--      prints the AST representing to Roo program
-------------------------

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
    Pprint -> doParse (pprint . snd) args

-- checks for valid command line arguments
-- returns corresonding Task if valid, else exxits program
checkArgs :: String -> [String] -> IO Task
checkArgs _ ['-':_] = 
  printErrorExit 1 "Missing filename"
checkArgs _ ["-a", filename] =
  return Parse
checkArgs _ ["-p", filename] =
  return Pprint
checkArgs progname _ =
  printErrorExit 1 $ "Usage: " ++ progname ++ " [-p] filename"

-- parses program specified by args and prints a rendered version specified by
-- function `f`
doParse :: (Program -> String) -> [String] -> IO ()
doParse f args = do
  let [_, filename] = args
  input <- readFile filename
  case runLexer input >>= runParser of
    Left err -> printErrorExit 2 err
    Right parsed -> putStrLn $ f parsed

-- prints an error message and exits with failure exit code
printErrorExit :: Int -> String -> IO a
printErrorExit code s = putStrLn s >> exitWith (ExitFailure code)
