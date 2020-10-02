module RooSymTable where

import qualified RooAST as AST

import qualified Data.Containers.ListUtils as LU
import qualified Control.Monad as C
import qualified Data.Maybe as M

p = AST.Program 
      [AST.Record [AST.Field AST.IntType "i"] "r"] 
      [AST.Array 10 (AST.Alias "r") "a"] 
      [ AST.Procedure "main" [] [] [AST.Writeln (AST.StrConst "hello")]
      , AST.Procedure "roo" [AST.ParamBase AST.IntType AST.Ref "i"] 
                            [AST.Var (AST.Alias "r") ["h"]]
                            [AST.Assign (AST.LId "i") (AST.IntConst 0)]]

type Entry a = (AST.Ident, a)

type Table a = [Entry a]

data RooSymTable 
  = RooSymTable { unRecords    :: Table Record
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
  = Procedure { unParams :: Table Parameter
              , unVars   :: Table Variable
              }
  deriving (Show, Eq)

data Field 
  = Field { unFType   :: AST.BaseType
          , unFOffset :: Int
          }
  deriving (Show, Eq)

data Parameter 
  = Parameter { unPType   :: AST.TypeName
              , unPName   :: AST.Ident
              , unMode    :: AST.Mode
              , unPOffset :: Int
              }
  deriving (Show, Eq)

data Variable 
  = Variable { unVType   :: AST.TypeName
             , unVName   :: AST.Ident
             , unVOffset :: Int
             }
  deriving (Show, Eq)

symTable :: AST.Program -> Either String RooSymTable
symTable (AST.Program rs as ps)
 = do 
     records <- recordTable rs
     arrays <- arrayTable records as
     procedures <- procedureTable records arrays ps
     let table = RooSymTable records arrays procedures
     if checkProcedures table 
     then Right table
     else Left "error in functions"

recordTable :: [AST.Record] -> Either String (Table Record)
recordTable rs = 
  do 
    records <- C.sequence (map convertRecord rs)
    checkDuplicates (map fst records) "duplicate record identifier" records
  where
    convertRecord :: AST.Record -> Either String (Entry Record)
    convertRecord (AST.Record fs ident) = 
      let fields = convertFields fs in
      checkDuplicates (map fst fields)
                      ("duplicate field identifier in `" ++ ident ++ "`")
                      (ident, Record fields)
    convertFields :: [AST.Field] -> [Entry Field]
    convertFields fs = zipWith convertField fs [0..] 
    convertField :: AST.Field -> Int -> Entry Field
    convertField (AST.Field t ident) offset = (ident, Field t offset)

arrayTable
  :: Table Record
  -> [AST.Array] -> Either String (Table Array)
arrayTable rs as = 
  do
    arrays <- C.sequence $ map convertArray as
    checkDuplicates (aliases ++ map fst arrays)
                    ("duplcate type alias")
                    arrays
  where
    convertArray :: AST.Array -> Either String (Entry Array)
    convertArray (AST.Array size t ident) = 
      if size <= 0
      then Left $ "non-positive array size (" ++ show size 
                  ++ ") for `" ++ ident ++ "`"
      else if not $ validType aliases t
      then Left $ "invalid type for `" ++ ident ++ "`"
      else Right (ident, Array t (fromInteger size))
    aliases :: [AST.Ident]
    aliases = map fst rs

procedureTable 
  :: Table Record -> Table Array 
  -> [AST.Procedure] -> Either String (Table Procedure)
procedureTable rs as ps = 
  do
    procedures <- C.sequence $ map convertProcedure ps
    checkDuplicates (map fst procedures)
                    "duplcate type alias"
                    procedures
  where
    convertProcedure :: AST.Procedure -> Either String (Entry Procedure)
    convertProcedure (AST.Procedure ident params vars _) =
      let parameters = convertParams params
          variables  = convertVars vars in
      checkDuplicates (map fst variables ++ map fst parameters)
                      ("duplcate parameter/variable name in `" ++ ident ++ "`")
                      (ident, Procedure parameters variables)
    convertParams :: [AST.Param] -> [(Entry Parameter)]
    convertParams params = zipWith convertParam params [0..]
    convertParam :: AST.Param -> Int -> Entry Parameter
    convertParam (AST.ParamAlias t ident) offset = 
      (ident, Parameter (AST.Alias t) ident AST.Ref offset)
    convertParam (AST.ParamBase t m ident) offset = 
      (ident, Parameter (AST.Base t) ident m offset)
    convertVars :: [AST.Var] -> [Entry Variable]
    convertVars vars = zipWith (\v i -> (unVName v, v { unVOffset = i })) 
                         (concatMap convertVar vars) [0..]
    convertVar :: AST.Var -> [Variable]
    convertVar (AST.Var t is) = map (convertVar' t) is
    convertVar' :: AST.TypeName -> AST.Ident -> Variable
    convertVar' t i = Variable t i (-1) -- fixed in convertVars

checkProcedures :: RooSymTable -> Bool
checkProcedures = const True

checkDuplicates :: (Ord c) => [c] -> a -> b -> Either a b 
checkDuplicates toCheck left right = 
  if length (LU.nubOrd toCheck) == length toCheck
  then Right right
  else Left left

validType :: [AST.Ident] -> AST.TypeName -> Bool
validType _       (AST.Base _)  = True
validType aliases (AST.Alias a) = elem a aliases