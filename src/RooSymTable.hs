module RooSymTable where

import qualified RooAST as AST

import qualified Data.Containers.ListUtils as LU
import qualified Control.Monad as C
import qualified Data.Maybe as M

import Debug.Trace

data RooSymTable 
  = RooSymTable { unRecords    :: [(AST.Ident, Record)]
                , unArrays     :: [(AST.Ident, Array)]
                , unProcedures :: [(AST.Ident, Procedure)]
                }
  deriving (Show, Eq)

data Record
  = Record { unFields :: [(AST.Ident, Field)] 
           }
  deriving (Show, Eq)

data Array
  = Array { unAType :: AST.TypeName
          , unSize  :: Int
          }
  deriving (Show, Eq)

data Procedure
  = Procedure { unParams :: [(AST.Ident, Variable)]
              , unVars   :: [(AST.Ident, Variable)]
              }
  deriving (Show, Eq)

data Field 
  = Field { unFType   :: AST.BaseType
          , unFOffset :: Int
          }
  deriving (Show, Eq)

data Variable 
  = Variable { unVType   :: AST.BaseType
             , unMode    :: AST.Mode
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

recordTable :: [AST.Record] -> Either String [(AST.Ident, Record)]
recordTable rs = 
  do 
    records <- C.sequence (map convert rs)
    checkDuplicates (map fst records) "duplicate record identifier" records
  where
    convert (AST.Record fs ident) = 
      let fields   = convertFields fs in
      checkDuplicates (map fst fields)
                      ("duplicate field identifier in `" ++ ident ++ "`")
                      (ident, Record fields)

convertFields :: [AST.Field] -> [(AST.Ident, Field)]
convertFields fs = zipWith convert fs [0..] 
  where convert (AST.Field t ident) offset = (ident, Field t offset)

arrayTable
  :: [(AST.Ident, Record)]
  -> [AST.Array] -> Either String [(AST.Ident, Array)]
arrayTable rs as = 
  do
    arrays <- C.sequence $ map convert as
    checkDuplicates (aliases ++ map fst arrays)
                    ("duplcate type alias")
                    arrays
  where
    convert (AST.Array size t ident) = 
      if size <= 0
      then Left $ "non-positive array size (" ++ show size 
                  ++ ") for `" ++ ident ++ "`"
      else if not $ validType t
      then Left $ "invalid type for `" ++ ident ++ "`"
      else Right (ident, Array t (fromInteger size))
    validType (AST.Base _)  = True
    validType (AST.Alias a) = elem a aliases
    aliases = map fst rs

procedureTable 
  :: [(AST.Ident, Record)] -> [(AST.Ident, Array)] 
  -> [AST.Procedure] -> Either String [(AST.Ident, Procedure)]
procedureTable = undefined

checkProcedures :: RooSymTable -> Bool
checkProcedures = undefined

checkDuplicates :: (Ord c) => [c] -> a -> b -> Either a b 
checkDuplicates toCheck left right = 
  if length (LU.nubOrd toCheck) == length toCheck
  then Right right
  else Left left