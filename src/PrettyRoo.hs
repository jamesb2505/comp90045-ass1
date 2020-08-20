module PrettyRoo (pprint) where

import RooAST

import Data.List (intercalate)

pprint :: Program -> String
pprint (Program r a p) = defs pRecord r
                         ++ defs pArray a
                         ++ (if null r && null a then "" else "\n\n")
                         ++ intercalate "\n\n" (map pProcedure p)
  where
    defs p [] = ""
    defs p ds = intercalate "\n" (map p ds) ++ "\n"

pDecl :: Decl -> String
pDecl (Decl t i) = pTypeName t ++ " " ++ i ++ ";"

pStmt :: Int -> Stmt -> String
pStmt i (Assign l e)     = pIndent i ++ pLValue l ++ " <- " ++ pExpr e ++ ";"
pStmt i (Read l)         = pIndent i ++ "read " ++ pLValue l ++ ";"
pStmt i (Write e)        = pIndent i ++ "write " ++ pExpr e ++ ";"
pStmt i (If e ss)        = pIndent i ++ "if " ++ pExpr e ++ "then\n"
                           ++ pStmtList (i + 1) ss
                           ++ "\n" ++ pIndent i ++ "fi"
pStmt i (IfElse e ts fs) = pIndent i ++ "if " ++ pExpr e ++ "then\n"
                           ++ pStmtList (i + 1) ts
                           ++ "\n" ++ pIndent i ++ "else\n"
                           ++ pStmtList (i + 1) fs
                           ++ "\n"
pStmt i (While e ss)     = pIndent i ++ "while " ++ pExpr e ++ "do\n"
                           ++ pStmtList (i + 1) ss
                           ++ "\n" ++ pIndent i ++ "do"
pStmt i (Call f es)      = pIndent i ++ "call " ++ f ++ "("
                           ++ pExprList es ++ ");"

pStmtList :: Int -> [Stmt] -> String
pStmtList i ss = intercalate "\n" (map (pStmt i) ss)

pExpr :: Expr -> String
pExpr (Lval l)            = pLValue l
pExpr (BoolConst b)       = if b then "true" else "false"
pExpr (IntConst i)        = show i
pExpr (StrConst s)        = show s
pExpr (BinOpExpr o e1 e2) = "(" ++ pExpr e1 ++ ")"
                            ++ pBinOp o
                            ++ "(" ++ pExpr e2 ++ ")"
pExpr (UnOpExpr o e)      = pUnOp o ++ "(" ++ pExpr e ++ ")" 

pExprList :: [Expr] -> String
pExprList es = intercalate ", " (map pExpr es)

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

pProcedure :: Procedure -> String
pProcedure (Procedure ds ss) = "procedure " ++ "()\n" 
                               ++ intercalate "\n" (map ((pIndent 1 ++) . pDecl) ds) 
                               ++ "\n{\n"
                               ++ pStmtList 1 ss
                               ++ "\n}"

pArray :: ArrayDef -> String
pArray (Array s t i) = "array [" ++ show s ++ "] " ++ pTypeName t ++ i ++ ";"

pRecord :: RecordDef -> String
pRecord (Record d i) = "record \n" ++ pIndent 1 ++ "{" 
                     ++ intercalate ";\n    ; " (map pDecl d)
                     ++ "\n" ++ pIndent 1 ++ "} " ++ i ++ ";"

pTypeName :: TypeName -> String
pTypeName BoolType      = "bool"
pTypeName IntType       = "integer"
pTypeName (TypeAlias i) = i

pIndent :: Int -> String
pIndent i = replicate (4 * i) ' '