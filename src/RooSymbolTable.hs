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

getLocalOffset :: SymbolTable -> AST.Ident -> Maybe Int
getLocalOffset (SymbolTable _ _ ((_,Procedure ps vs _):_)) alias
  | isTableKey alias ps
    = unPOffset <$> lookup alias ps
  | isTableKey alias vs
    = unVOffset <$> lookup alias vs
getLocalOffset _ _ = Nothing

-- getType 
-- gets the ExprType of a given TypeName in a SymbolTable
-- ErrorT is returned if TypeName is not found in any context
getType :: SymbolTable -> AST.TypeName -> AST.ExprType
getType st@(SymbolTable rs as _) (AST.Alias alias)
  | isTableKey alias rs
    = AST.RecordT alias
  | isTableKey alias as
    = case lookup alias as of
        Nothing          -> AST.ErrorT
        Just (Array t _) -> AST.ArrayT alias $ getType st t
getType _ (AST.Base AST.BoolType) = AST.BoolT
getType _ (AST.Base AST.IntType) = AST.IntT
getType _ _ = AST.ErrorT

getProcType :: SymbolTable -> AST.Ident -> AST.ExprType
getProcType st@(SymbolTable _ _ ((_,Procedure ps vs _):_)) name
  | isTableKey name ps
    = case lookup name ps of
        Nothing            -> AST.ErrorT
        Just (Param t _ _) -> getType st t
  | isTableKey name vs
    = case lookup name vs of
        Nothing        -> AST.ErrorT
        Just (Var t _) -> getType st t
getProcType _ _ = AST.ErrorT

-- getAliasType 
-- gets the ExprType of a given alias (Ident) in a SymbolTable
-- ErrorT is returned if alias (Ident) is not found in any context
getAliasType :: SymbolTable -> AST.Ident -> AST.ExprType
getAliasType st ident = getType st (AST.Alias ident)

-- getFieldType 
-- gets the ExprType of a given Field (Ident) of a RecordT
-- ErrorT is returned if Field is not found in any context
getFieldType :: Table Record -> AST.ExprType -> AST.Ident -> AST.ExprType
getFieldType rs rt@(AST.RecordT r) f
    = case lookup r rs >>= lookup f . unFields of
        Nothing                       -> AST.ErrorT
        (Just (Field AST.BoolType _)) -> AST.BoolT
        (Just (Field AST.IntType _))  -> AST.IntT
getFieldType _ _ _ = AST.ErrorT 

-- getArrayType 
-- gets the ExprType of the values of a given ArrayT
-- ErrorT is returned if alias (Ident) is not found in any context
getArrayType :: AST.ExprType -> AST.ExprType
getArrayType (AST.ArrayT _ t) = t
getArrayType _              = AST.ErrorT
