module RooSymbolTable where

import qualified RooAST as AST

import Data.Maybe (fromJust)

type Entry a = (AST.Ident, a)

type Table a = [Entry a]

-- SymbolTable
-- Data tpye to store the symbol table of a Roo Program
data SymbolTable 
  = SymbolTable { unRecords    :: Table Record
                , unArrays     :: Table Array
                , unProcedures :: Table Procedure
                }
  deriving (Show, Eq)

-- Record
-- Data type to store the symbols of a Record
data Record
  = Record { unFields :: Table Field }
  deriving (Show, Eq)

-- Array
-- Data type to store the symbols and data of an Array
data Array
  = Array { unAType :: AST.TypeName
          , unSize  :: Int
          }
  deriving (Show, Eq)

-- Procedure
-- Data type to store the symbols and data of a Procedure
data Procedure
  = Procedure { unParams    :: Table Param
              , unVars      :: Table Var
              , unStackSize :: Int
              }
  deriving (Show, Eq)

-- Field
-- Data type to store the data of a Field
data Field 
  = Field { unFType   :: AST.AtomicType
          , unFOffset :: Int
          }
  deriving (Show, Eq)

-- Param
-- Data type to store the data of a Param
data Param 
  = Param { unPType   :: AST.TypeName
          , unMode    :: AST.Mode
          , unPOffset :: Int
          }
  deriving (Show, Eq)

-- Var
-- Data type to store the data of a Var
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

-- entryRecord
-- Converts a AST.Record into a [Entry Record]
entryRecord :: AST.Record -> Entry Record
entryRecord (AST.Record fs ident) = (ident, Record $ entryFields fs)

-- entryFields
-- Converts a [AST.Field] into a [Entry Field]
entryFields :: [AST.Field] -> [Entry Field]
entryFields fs = zipWith entryField fs [0..] 

-- entryFields
-- Converts an AST.Field into an Entry Field
entryField :: AST.Field -> Int -> Entry Field
entryField (AST.Field t ident) offset = (ident, Field t offset)

-- entryArray
-- Converts an AST.Array into an Entry Array
entryArray :: AST.Array -> Entry Array
entryArray (AST.Array size t ident) = (ident, Array t (fromInteger size))

-- entryProcedure
-- Converts an AST.Procedure into an Entry Procedure with 
-- a SymbolTable
entryProcedure :: SymbolTable -> AST.Procedure -> Entry Procedure
entryProcedure st (AST.Procedure ident params vars _) =
  (ident, Procedure (entryParams params) fixedOffsetVars stackSize)
  where 
    vars' :: [Entry Var]
    vars' = entryVars vars
    fixedOffsetVars :: [Entry Var]
    fixedOffsetVars = zipWith fixOffset vars' offsets
      where fixOffset (ident, v) off = (ident, v { unVOffset = off })
    offsets :: [Int]
    offsets = scanl (+) (length params)
              $ map (lookupTotalSize st . unVType . snd) vars'
    stackSize :: Int
    stackSize = last offsets

-- entryParams
-- Converts a [AST.Param] into a [Entry Param]
entryParams :: [AST.Param] -> [Entry Param]
entryParams params = zipWith entryParam params [0..]

-- entryParam
-- Converts an AST.Param into an Entry Param
entryParam :: AST.Param -> Int -> Entry Param
entryParam (AST.ParamAlias t ident) offset = 
  (ident, Param (AST.Alias t) AST.Ref offset)
entryParam (AST.ParamAtomic t m ident) offset = 
  (ident, Param (AST.Atomic t) m offset)

-- entryVars
-- Converts a [AST.Var] into a [Entry Var]
entryVars :: [AST.Var] -> [Entry Var]
entryVars vars = concatMap entryVar vars

-- entryVar
-- Converts an AST.Var into an Entry Var
-- unVOffset is erroneous in output
entryVar :: AST.Var -> [Entry Var]
entryVar (AST.Var t is) = map (\i -> (i, Var t (-1))) is

-- isRef
-- Checks if an Ident is in Reference mode
-- Only checks the top most Procedure
isRef :: SymbolTable -> AST.Ident -> Bool
isRef (SymbolTable _ _ ((_,Procedure ps _ _):_)) alias
  | isTableKey alias ps
  = (AST.Ref ==) . unMode . fromJust $ lookup alias ps
isRef _ _ = False

-- getRecord
-- Looks up a Record for Ident in the SymbolTable
-- Nothing if no Record, else Just
getRecord :: SymbolTable -> AST.Ident -> Maybe Record
getRecord (SymbolTable rs _ _) alias = lookup alias rs

-- getField
-- Looks up a Field for Ident in the Record
-- Nothing if no Field, else Just
getField :: Record -> AST.Ident -> Maybe Field
getField (Record fs) field = lookup field fs

-- getArray
-- Looks up a Array for Ident in the SymbolTable
-- Nothing if no Array, else Just
getArray :: SymbolTable -> AST.Ident -> Maybe Array
getArray (SymbolTable _ as _) alias = lookup alias as

-- getLocalOffset
-- Gets the offset of an Ident in the top most Procedure of a SymbolTable
-- Nothing if no Ident, else Just
getLocalOffset :: SymbolTable -> AST.Ident -> Maybe Int
getLocalOffset (SymbolTable _ _ ((_,Procedure ps vs _):_)) alias
  | isTableKey alias ps
    = unPOffset <$> lookup alias ps
  | isTableKey alias vs
    = unVOffset <$> lookup alias vs
getLocalOffset _ _ = Nothing

-- getType 
-- Gets the ExprType of a given TypeName in a SymbolTable
-- ErrorT is returned if TypeName is not found in any context
getType :: SymbolTable -> AST.TypeName -> AST.ExprType
getType st@(SymbolTable rs as _) (AST.Alias alias)
  | isTableKey alias rs
    = AST.RecordT alias
  | isTableKey alias as
    = case lookup alias as of
        Nothing          -> AST.ErrorT
        Just (Array t _) -> AST.ArrayT alias $ getType st t
getType _ (AST.Atomic AST.BoolType) = AST.BoolT
getType _ (AST.Atomic AST.IntType) = AST.IntT
getType _ _ = AST.ErrorT

-- getProcType 
-- Gets the ExprType of a given Ident in a SymbolTable
-- ErrorT is returned if Ident is not found in variables or procedures
-- Only the topmost Procedure is checked
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
-- Gets the ExprType of a given alias (Ident) in a SymbolTable
-- ErrorT is returned if alias (Ident) is not found in any context
getAliasType :: SymbolTable -> AST.Ident -> AST.ExprType
getAliasType st ident = getType st (AST.Alias ident)

-- getFieldType 
-- Gets the ExprType of a given Field (Ident) of a RecordT
-- ErrorT is returned if Field is not found in any context
getFieldType :: Table Record -> AST.ExprType -> AST.Ident -> AST.ExprType
getFieldType rs rt@(AST.RecordT r) f
    = case lookup r rs >>= lookup f . unFields of
        Nothing                       -> AST.ErrorT
        (Just (Field AST.BoolType _)) -> AST.BoolT
        (Just (Field AST.IntType _))  -> AST.IntT
getFieldType _ _ _ = AST.ErrorT 

-- getArrayType 
-- Gets the ExprType of the values of a given ArrayT
-- ErrorT is returned if alias (Ident) is not found in any context
getArrayType :: AST.ExprType -> AST.ExprType
getArrayType (AST.ArrayT _ t) = t
getArrayType _                = AST.ErrorT

-- getLValueType
getLValueType :: SymbolTable -> AST.LValue -> AST.ExprType
getLValueType st (AST.LId alias) = getProcType st alias
getLValueType st@(SymbolTable _ as _) (AST.LInd alias _)
  | AST.isArrayT aliasType
    = getArrayType aliasType
  where aliasType = getProcType st alias
getLValueType st@(SymbolTable rs _ _) (AST.LField alias field)
  | AST.isRecordT aliasType
    = getFieldType rs aliasType field
  where aliasType = getProcType st alias
getLValueType st@(SymbolTable rs _ _) (AST.LIndField alias _ field) 
  | AST.isArrayT aliasType && AST.isRecordT recordType
    = getFieldType rs recordType field
  where 
    aliasType = getProcType st alias
    recordType = getArrayType aliasType
getLValueType _ _ = AST.ErrorT

-- lookupTotalSize
-- Looks up the total size of a given AST.TypeName in a SymbolTable
lookupTotalSize :: SymbolTable -> AST.TypeName -> Int
lookupTotalSize st@(SymbolTable rs as _) (AST.Alias alias)
  | isTableKey alias rs
    = length . unFields . fromJust $ getRecord st alias
  | isTableKey alias as
    = s * lookupTotalSize st t
  where (Array t s) = fromJust $ getArray st alias
lookupTotalSize _ _ = 1

-- lookupElementSize
-- Looks up up the size of an element of a givent AST.TypeName
lookupElementSize st@(SymbolTable _ as _) (AST.Alias alias)
  | isTableKey alias as
    = lookupTotalSize st t
  where (Array t _) = fromJust $ getArray st alias
lookupElementSize _ _ = 1
