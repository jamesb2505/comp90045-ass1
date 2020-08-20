module PrettyRoo (pprint) where

import RooAST

import Data.List (intercalate)

pprint :: Program -> String
pprint (Program [] s) = concatMap pStmt s
pprint (Program d s)  = intercalate ";\n" (map pDecl d) 
                        ++ "\n\n" 
                        ++ concatMap pStmt s

pDecl :: Decl -> String
pDecl (Decl t i) = pTypeName t ++ " " ++ i

pStmt :: Stmt -> String
pStmt (Assign l e) = pLValue l ++ " <- " ++ pExpr e
pStmt (Read l) = "read " ++ pLValue l
pStmt (Write e) = "write " ++ pExpr e

pExpr :: Expr -> String
pExpr (Lval l)            = pLValue l
pExpr (BoolConst b)       = if b then "true" else "false"
pExpr (IntConst i)        = show i
pExpr (StrConst s)        = show s
pExpr (BinOpExpr o e1 e2) = "(" ++ pExpr e1 ++ ")"
                            ++ pBinOp o
                            ++ "(" ++ pExpr e2 ++ ")"
pExpr (UnOpExpr o e)      = pUnOp o ++ "(" ++ pExpr e ++ ")" 

pUnOp :: UnOp -> String
pUnOp Op_not = "not"
pUnOp Op_neg = "-"

pBinOp :: BinOp -> String
pBinOp Op_or  = "or"
pBinOp Op_and = "and"
pBinOp Op_eq  = "="
pBinOp Op_neq = "!="
pBinOp Op_ls  = "<"
pBinOp Op_leq = "<="
pBinOp Op_gt  = ">"
pBinOp Op_geq = ">="
pBinOp Op_add = "+"
pBinOp Op_sub = "-"
pBinOp Op_mul = "*"
pBinOp Op_div = "/"

pLValue :: LValue -> String
pLValue (LId i)           = i
pLValue (LField i f)      = i ++ "." ++ f
pLValue (LInd i e)        = i ++ "[" ++ pExpr e ++ "]"
pLValue (LIndField i e f) = i ++ "[" ++ pExpr e ++ "]." ++ f

pArray :: Array -> String
pArray (Array t i) = "[" ++ show i ++ "] " ++ pTypeName t

pRecord :: Record -> String
pRecord (Record d) = "\n    {" 
                     ++ intercalate ";\n    ; " (map pDecl d)
                     ++ "\n    } "

pTypeName :: TypeName -> String
pTypeName BoolType       = "bool"
pTypeName IntType        = "integer"
pTypeName (RecordType r) = "record " ++ pRecord r
pTypeName (ArrayType a)  = "array " ++ pArray a