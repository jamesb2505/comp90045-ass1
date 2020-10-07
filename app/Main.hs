module Main (main) where

-------------------------
-- Roo Language Compiler
--
-- Command line arguments:
--  * <filename> 
--      generates Oz code for the Roo program
--  * -p <filename> 
--      "pretty print"s a given roo program, printing lexical and parsing
--      errors
--  * -a <filename>
--      prints the AST representing to Roo program
-------------------------

import RooParser (runParser)
import RooLexer (runLexer)
import RooOzCodeGen (runCodeGen)
import OzCode (printOzCodes)
import RooAST (Program)
import RooSymbolTable (SymbolTable)
import PrettyRoo (prettyPrint)

import System.Environment (getArgs, getProgName)
import System.Exit (ExitCode(..), exitWith)
import Data.List (intercalate)

data Task
  = Parse String
  | PrettyPrint String
  | CodeGen String
  deriving (Eq, Show)

main :: IO ()
main = do
  progname <- getProgName
  args <- getArgs
  task <- checkArgs progname args
  case task of
    Parse filename       -> doParse show filename
    PrettyPrint filename -> doParse prettyPrint filename
    CodeGen filename     -> doCodeGen filename

-- checkArgs
-- checks for valid command line arguments
-- returns corresonding Task if valid, else exits program
checkArgs :: String -> [String] -> IO Task
checkArgs _ ['-':_] = 
  printErrorExit 1 "Missing filename"
checkArgs _ ["-a", filename] =
  return $ Parse filename
checkArgs _ ["-p", filename] =
  return $ PrettyPrint filename
checkArgs _ [filename] =
  return $ CodeGen filename
checkArgs progname _ =
  printErrorExit 1 $ "Usage: " ++ progname ++ " [-a|-p] filename"

-- doParse
-- parses program specified by a given filename and prints a rendered version 
-- specified by the function `f`
doParse :: (Program -> String) -> String -> IO ()
doParse f filename = do
  input <- readFile filename
  case runLexer input >>= runParser of
    Left err          -> printErrorExit 2 err
    Right (parsed, _) -> putStr $ f parsed

-- doCodeGen
-- generates Oz code for a given filename
doCodeGen :: String -> IO ()
doCodeGen filename = do
  input <- readFile filename
  case runLexer input >>= runParser >>= uncurry runCodeGen of
    Left err   -> printErrorExit 2 err
    Right codes -> printOzCodes codes

-- printErrorExit
-- prints an error message and exits with failure exit code
printErrorExit :: Int -> String -> IO a
printErrorExit code s = putStrLn s >> exitWith (ExitFailure code)
