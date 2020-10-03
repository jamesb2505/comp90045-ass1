{-# LANGUAGE FlexibleInstances #-}

module RooSymbolTable where

import qualified RooAST as AST

import qualified Data.Containers.ListUtils as LU
import qualified Control.Monad as C
import qualified Data.Maybe as M

p = AST.Program 
      [AST.Record [AST.Field AST.IntType "i", AST.Field AST.IntType "b"] "r"] 
      [AST.Array 10 (AST.Alias "r") "a"] 
      [ AST.Procedure "main" [] [] [AST.Writeln (AST.StrConst "hello")]
      , AST.Procedure "roo" [AST.ParamBase AST.IntType AST.Ref "i", AST.ParamAlias "r" "j"] 
                            [AST.Var (AST.Alias "a") ["h", "b"]]
                            [AST.Assign (AST.LId "i") (AST.IntConst 0)]]

type Entry a = (AST.Ident, a)

type Table a = [Entry a]

data SymbolTable 
  = SymbolTable { unRecords    :: Table Record
                , unArrays     :: Table Array
                , unProcedures :: Table Procedure
                }
  deriving (Show, Eq)

data PartialTable 
  = PartialTable (Table Record) (Table Array)
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

noDuplicates :: (Ord a) => [a] -> Bool
noDuplicates xs = length (LU.nubOrd xs) == length xs 

-- class SizeOf a where
--   sizeOf :: Table Record -> Table Array -> a -> Int

-- -- symTable
-- -- constructs a SymbolTable from an AST.Program
-- -- Left if there is any type errors in the program, else Right
-- symbolTable :: AST.Program -> Either String SymbolTable
-- symbolTable (AST.Program rs as ps)
--  = do 
--      records <- recordTable rs
--      arrays <- arrayTable records as
--      procedures <- procedureTable records arrays ps
--      let table = SymbolTable records arrays procedures
--      if checkProcedures table ps
--         && checkMain table
--      then Right table
--      else Left "error in functions"

-- -- recordTable
-- -- constructs a Table Record from a list of AST.Records
-- -- Left if there is any name conlicts in each record, else Right
-- recordTable :: [AST.Record] -> Either String (Table Record)
-- recordTable rs = 
--   do 
--     records <- C.sequence (map entryRecord rs)
--     checkDuplicates (tableKeys records) "duplicate record identifier" records
--   where
entryRecord :: AST.Record -> Entry Record
entryRecord (AST.Record fs ident) = (ident, Record $ entryFields fs)
                 

-- entryFields
-- Converts a [AST.Field] into a [Entry Field]
entryFields :: [AST.Field] -> [Entry Field]
entryFields fs = zipWith entryField fs [0..] 

-- entryFields
-- Converts an aST.Field into an Entry Field
entryField :: AST.Field -> Int -> Entry Field
entryField (AST.Field t ident) offset = (ident, Field t offset)

-- -- arrayTable
-- -- constructs a Table Array from a [AST.Array] and a Table Record
-- -- Left if there is any name conlicts or size errors, else Right
-- arrayTable :: Table Record -> [AST.Array] -> Either String (Table Array)
-- arrayTable rs as = 
--   do
--     arrays <- C.sequence $ map entryArray as
--     checkDuplicates (aliases ++ tableKeys arrays)
--                     ("duplcate type alias")
--                     arrays
--   where
--     entryArray :: AST.Array -> Either String (Entry Array)
--     entryArray (AST.Array size t ident) = 
--       if size <= 0
--       then Left $ "non-positive array size (" ++ show size 
--                   ++ ") for `" ++ ident ++ "`"
--       else if not $ validTypeName aliases t
--       then Left $ "invalid type for `" ++ ident ++ "`"
--       else Right (ident, Array t (fromInteger size))
--     aliases :: [AST.Ident]
--     aliases = tableKeys rs

entryArray :: AST.Array -> Entry Array
entryArray (AST.Array size t ident) = (ident, Array t (fromInteger size))


-- -- procedureTable
-- -- constructs a Table Procedure from a [AST.Records], a Table Record, and a
-- -- Table Array
-- -- Left if there is any name conlicts, else Right
-- procedureTable 
--   :: Table Record -> Table Array 
--   -> [AST.Procedure] -> Either String (Table Procedure)
-- procedureTable rs as ps = 
--   do
--     procedures <- C.sequence $ map entryProcedure ps
--     checkDuplicates (tableKeys procedures)
--                     ("duplcate procedure name")
--                     procedures
--   where
--     entryProcedure :: AST.Procedure -> Either String (Entry Procedure)
--     entryProcedure (AST.Procedure ident params vars _) =
--       let parameters = entryParams params
--           variables  = entryVars vars in
--       if checkTypes parameters variables
--       then checkDuplicates (tableKeys variables ++ tableKeys parameters)
--                            ("duplcate parameter/Var name in `" 
--                             ++ ident ++ "`")
--                            (ident, Procedure parameters 
--                                      (fixOffsets (length parameters) variables) 
--                                      (stackSize (length parameters) variables))
--       else Left $ "invalid type in `" ++ ident ++ "`"
--     checkTypes :: Table Param -> Table Var -> Bool
--     checkTypes params vars =
--       all (validTypeName aliases) $ map (unPType . snd) params
--                                     ++ map (unVType . snd) vars
--     aliases :: [AST.Ident]
--     aliases = tableKeys rs ++ tableKeys as


entryProcedure :: PartialTable -> AST.Procedure -> Entry Procedure
entryProcedure pt (AST.Procedure ident params vars _) =
  (ident, Procedure (entryParams params) fixedOffsetVars stackSize)
  where 
    vars' :: [Entry Var]
    vars' = entryVars vars
    fixedOffsetVars :: [Entry Var]
    fixedOffsetVars = zipWith fixOffset vars' offsets
      where fixOffset (ident, v) off = (ident, v { unVOffset = off })
    offsets :: [Int]
    offsets = scanl (+) (length params)
              $ map (lookupSize pt . unVType . snd) vars'
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
entryParam (AST.ParamBase t m ident) offset = 
  (ident, Param (AST.Base t) m offset)

-- entryVars
-- Converts a [AST.Var] into a [Entry Var]
entryVars :: [AST.Var] -> [Entry Var]
entryVars vars = concatMap entryVar vars

-- entryVar
-- Converts an AST.Var into an Entry Var
-- unVOffset is erroneous in output
entryVar :: AST.Var -> [Entry Var]
entryVar (AST.Var t is) = map (\i -> (i, Var t (-1))) is

-- -- checkProcedures
-- -- Checks that the types of all statements in all [AST.Procedure] are correct
-- -- using SymbolTable
-- checkProcedures :: SymbolTable -> [AST.Procedure] -> Bool
-- checkProcedures _ _ = True

-- -- checkMain
-- -- Check if "main" procedure in SymbolTable is correctly defined
-- -- i.e. is defined and has no parameters
-- checkMain :: SymbolTable -> Bool
-- checkMain (SymbolTable _ _ ps) = 
--   elem "main" (tableKeys ps)
--   && (null $ unParams $ M.fromJust $ lookup "main" ps)

-- -- checkDupicates
-- -- Checks for dupicates in [c]
-- -- If there is no duplicates, Right b, else Left a
-- checkDuplicates :: (Ord c) => [c] -> a -> b -> Either a b 
-- checkDuplicates toCheck left right = 
--   if length (LU.nubOrd toCheck) == length toCheck
--   then Right right
--   else Left left

-- -- validTypeName
-- -- Checks if an AST.Type name is valid, given a list of aliases
-- validTypeName :: [AST.Ident] -> AST.TypeName -> Bool
-- validTypeName _       (AST.Base _)  = True
-- validTypeName aliases (AST.Alias a) = elem a aliases

-- lookupSize
-- looks up the size of a given AST.TypeName in a PartialTable
lookupSize :: PartialTable -> AST.TypeName -> Int
lookupSize (PartialTable rs _) (AST.Alias ident)
  | elem ident (tableKeys rs)
    = length . unFields . M.fromJust $ lookup ident rs
lookupSize st@(PartialTable _ as) (AST.Alias ident)
  | elem ident (tableKeys as)
    = s * lookupSize st t
  where (Array t s) = M.fromJust $ lookup ident as
lookupSize _ _ = 1
