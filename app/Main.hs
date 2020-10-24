-------------------------
-- Roo Language Compiler
--
-- Written by James Barnes, Jack Macumber, & Isitha Subasinghe
--
-- Command line arguments:
--  * <filename> 
--      generates Oz code for the Roo program
--  * -p <filename> 
--      "pretty print"s a given roo program, printing lexical and parsing
--      errors
--  * -a <filename>
--      prints the AST representing to Roo program
--  * -c <filename>
--      prints C code equivalent to the Roo code; beware of undefined behaviour
--  * -py <filename>
--      prints Python code equivalent to the Roo code; beware of undefined behaviour
-------------------------

module Main (main) where

import RooParser (runParser)
import RooLexer (runLexer)
import RooOzCodeGen (runCodeGen)
import RooCTrans (runCTrans)
import RooPyTrans (runPyTrans)
import OzCode (printOzCodes)
import RooAST (Program)
import RooSymbolTable (SymbolTable)
import PrettyRoo (prettyPrint)

import System.Environment (getArgs, getProgName)
import System.Exit (ExitCode(..), exitWith)
import System.IO (hPutStrLn, stderr)

data Task
  = Parse String
  | PrettyPrint String
  | CodeGen String
  | CTrans String
  | PyTrans String
  deriving (Eq, Show)

main :: IO ()
main = do
  progname <- getProgName
  args <- getArgs
  task <- checkArgs progname args
  case task of
    Parse filename       -> doParse show filename
    PrettyPrint filename -> doParse prettyPrint filename
    CodeGen filename     -> doCodeGen runCodeGen printOzCodes filename
    CTrans filename      -> doCodeGen runCTrans putStrLn filename
    PyTrans filename     -> doCodeGen runPyTrans putStrLn filename

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
checkArgs _ ["-c", filename] =
  return $ CTrans filename
checkArgs _ ["-py", filename] =
  return $ PyTrans filename
checkArgs _ [filename] =
  return $ CodeGen filename
checkArgs progname _ =
  printErrorExit 1 $ "Usage: " ++ progname ++ " [-a|-p|-c|-py] filename"

-- doParse
-- parses program specified by a given filename and prints a rendered version 
-- specified by the function `f`
doParse :: (Program -> String) -> String -> IO ()
doParse f filename = do
  input <- readFile filename
  case runLexer input >>= runParser of
    Left err         -> printErrorExit 2 err
    Right (parsed,_) -> putStrLn $ f parsed

-- doCodeGen
-- generates C code for a given filename
doCodeGen :: (Program -> SymbolTable -> Either String b) -> (b -> IO ()) 
          -> String -> IO ()
doCodeGen trans printer filename = do
  input <- readFile filename
  case runLexer input >>= runParser >>= uncurry trans of
    Left err    -> printErrorExit 2 err
    Right codes -> printer codes

-- printErrorExit
-- prints an error message and exits with failure exit code
printErrorExit :: Int -> String -> IO a
printErrorExit code s 
  = hPutStrLn stderr ("Error:\n" ++ s) >> exitWith (ExitFailure code)
