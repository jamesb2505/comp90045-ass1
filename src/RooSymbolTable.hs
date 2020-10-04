{-# LANGUAGE FlexibleInstances #-}

module RooSymbolTable where

import qualified RooAST as AST

import qualified Data.Containers.ListUtils as LU
import qualified Control.Monad as C
import qualified Data.Maybe as M

-- p = AST.Program 
--       [AST.Record [AST.Field AST.IntType "i", AST.Field AST.IntType "b"] "r"] 
--       [AST.Array 10 (AST.Alias "r") "a"] 
--       [ AST.Procedure "main" [] [] [AST.Writeln (AST.StrConst AST.StrT "hello")]
--       , AST.Procedure "roo" [AST.ParamBase AST.IntType AST.Ref "i", AST.ParamAlias "r" "j"] 
--                             [AST.Var (AST.Alias "a") ["h", "b"]]
--                             [AST.Assign (AST.LId AST.IntT "i") (AST.IntConst AST.IntT 0)]]

type Entry a = (AST.Ident, a)

type Table a = [Entry a]

data SymbolTable 
  = SymbolTable { unRecords    :: Table Record
                , unArrays     :: Table Array
                , unProcedures :: Table Procedure
                }
  deriving (Show, Eq)

data Record
  = Record { unFields :: Table Field
           }
  deriving (Show, Eq)

data Array
  = Array { unAType :: AST.TypeName
          , unSize  :: Int
          }
  deriving (Show, Eq)

data Procedure
  = Procedure { unParams    :: Table Param
              , unVars      :: Table Var
              , unStackSize :: Int
              }
  deriving (Show, Eq)

data Field 
  = Field { unFType   :: AST.BaseType
          , unFOffset :: Int
          }
  deriving (Show, Eq)

data Param 
  = Param { unPType   :: AST.TypeName
          , unMode    :: AST.Mode
          , unPOffset :: Int
          }
  deriving (Show, Eq)

data Var 
  = Var { unVType   :: AST.TypeName
        , unVOffset :: Int
        }
  deriving (Show, Eq)

-- tableKeys
-- Gets the list of a tables' keys
tableKeys :: Table a -> [AST.Ident]
tableKeys = map fst

-- isTableKey
-- Checks if an Idetn is a key in a Table a
isTableKey :: AST.Ident -> Table a -> Bool
isTableKey ident table = elem ident $ tableKeys table
