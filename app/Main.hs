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
import RooOzCodeGen (runCodeGen)
import RooAST (Program)
import RooSymbolTable (SymbolTable)
import PrettyRoo (pprint)

import System.Environment (getArgs, getProgName)
import System.Exit (ExitCode(..), exitWith)
import Data.List (intercalate)

data Task
  = Parse
  | Pprint
  | CodeGen
  deriving (Eq, Show)

main :: IO ()
main = do
  progname <- getProgName
  args <- getArgs
  task <- checkArgs progname args
  case task of
    Parse   -> doParse show args
    Pprint  -> doParse pprint args
    CodeGen -> doCodeGen args

-- checks for valid command line arguments
-- returns corresonding Task if valid, else exxits program
checkArgs :: String -> [String] -> IO Task
checkArgs _ ['-':_] = 
  printErrorExit 1 "Missing filename"
checkArgs _ [filename] =
  return CodeGen
checkArgs _ ["-a", filename] =
  return Parse
checkArgs _ ["-p", filename] =
  return Pprint
checkArgs progname _ =
  printErrorExit 1 $ "Usage: " ++ progname ++ " [-a|-p] filename"

-- parses program specified by args and prints a rendered version specified by
-- function `f`
doParse :: (Program -> String) -> [String] -> IO ()
doParse f args = do
  let [_, filename] = args
  input <- readFile filename
  case runLexer input >>= runParser of
    Left err          -> printErrorExit 2 err
    Right (parsed, _) -> putStrLn $ f parsed

-- parses program specified by args and prints a rendered version specified by
-- function `f`
doCodeGen :: [String] -> IO ()
doCodeGen args = do
  let [filename] = args
  input <- readFile filename
  case runLexer input >>= runParser >>= uncurry runCodeGen of
    Left err   -> printErrorExit 2 err
    Right code -> putStrLn . intercalate "\n" $ map show code

-- prints an error message and exits with failure exit code
printErrorExit :: Int -> String -> IO a
printErrorExit code s = putStrLn s >> exitWith (ExitFailure code)
