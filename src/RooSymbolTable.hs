{-# LANGUAGE FlexibleInstances #-}

module RooSymbolTable where

import qualified RooAST as AST

import qualified Data.Containers.ListUtils as LU
import qualified Control.Monad as C
import qualified Data.Maybe as M

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
isTableKey alias table = elem alias $ tableKeys table

isRef :: SymbolTable -> AST.Ident -> Bool
isRef (SymbolTable _ _ ((_,Procedure ps _ _):_)) alias
  | isTableKey alias ps
  = (AST.Ref ==) . unMode . M.fromJust $ lookup alias ps
isRef _ _ = False

getRecord :: SymbolTable -> AST.Ident -> Record
getRecord (SymbolTable rs _ _) alias = M.fromJust $ lookup alias rs

getField :: Record -> AST.Ident -> Field
getField (Record fs) field = M.fromJust $ lookup field fs

getArray :: SymbolTable -> AST.Ident -> Array
getArray (SymbolTable _ as _) alias = M.fromJust $ lookup alias as

getLocalOffset :: SymbolTable -> AST.Ident -> Int
getLocalOffset (SymbolTable _ _ ((_,Procedure ps vs _):_)) alias
  | isTableKey alias ps
  = unPOffset . M.fromJust $ lookup alias ps
  | isTableKey alias vs
  = unVOffset . M.fromJust $ lookup alias vs
