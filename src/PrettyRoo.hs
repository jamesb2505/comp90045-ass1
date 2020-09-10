module PrettyRoo (pprint) where

import RooAST

import Data.List (intercalate)

----------------------
-- Printing Functions
----------------------

pprint :: Program -> String
pprint (Program rs as ps) 
  = decs pRecord rs ++ decs pArray as
    ++ (if null rs && null as then "" else "\n")
    ++ intercalate "\n\n" (map pProcedure ps)
  where
    decs _ [] = ""
    decs p ds = unlines $ map p ds

pStmt :: [Stmt] -> String
pStmt ss = unlines $ pStmtL ss

pStmtL :: [Stmt] -> [String]
pStmtL ss = concatMap (map indent . pStmt') ss
  where
    pStmt' :: Stmt -> [String]
    pStmt' (Assign l e)     = [ pLValue l ++ " <- " ++ pExpr e ++ ";" ]
    pStmt' (Read l)         = [ "read " ++ pLValue l ++ ";" ]
    pStmt' (Write e)        = [ "write " ++ pExpr e ++ ";" ]
    pStmt' (Writeln e)      = [ "writeln " ++ pExpr e ++ ";" ]
    pStmt' (If e ss)        = [ "if " ++ pExpr e ++ " then" ]
                              ++ pStmtL ss
                              ++ [ "fi" ]
    pStmt' (IfElse e ts fs) = [ "if " ++ pExpr e ++ " then" ]
                              ++ pStmtL ts 
                              ++ [ "else" ]
                              ++ pStmtL fs
                              ++ [ "fi" ]
    pStmt' (While e ss)     = [ "while " ++ pExpr e ++ " do" ]
                              ++ pStmtL ss
                              ++ [ "od" ]
    pStmt' (Call f es)      = [ "call " ++ f ++ "(" ++ pExprL es ++ ");" ]

pExpr :: Expr -> String
pExpr (LVal l)          = pLValue l
pExpr (BoolConst b)     = if b then "true" else "false"
pExpr (IntConst i)      = show i
pExpr (StrConst s)      = "\"" ++ s ++ "\""
pExpr (UnOpExpr o e)    = pUnOp o ++ (if isParenOp e && prec o > prec e
                                      then paren $ pExpr e
                                      else pExpr e)
pExpr (BinOpExpr o l r) = binParen isRAssoc o l
                          ++ pBinOp o
                          ++ binParen isLAssoc o r
  where 
    -- uses the minimal amount of required parens
    binParen a o e = if  (prec o > prec e || (prec o == prec e && a o))
                     then paren $ pExpr e 
                     else pExpr e

pExprL :: [Expr] -> String
pExprL es = intercalate ", " $ map pExpr es

pUnOp :: UnOp -> String
pUnOp Op_not = "not "
pUnOp Op_neg = "-"

pBinOp :: BinOp -> String
pBinOp Op_or  = " or "
pBinOp Op_and = " and "
pBinOp Op_eq  = " = "
pBinOp Op_neq = " != "
pBinOp Op_lt  = " < "
pBinOp Op_leq = " <= "
pBinOp Op_gt  = " > "
pBinOp Op_geq = " >= "
pBinOp Op_add = " + "
pBinOp Op_sub = " - "
pBinOp Op_mul = " * "
pBinOp Op_div = " / "

pLValue :: LValue -> String
pLValue (LId i)           = i
pLValue (LField i f)      = i ++ "." ++ f
pLValue (LInd i e)        = i ++ "[" ++ pExpr e ++ "]"
pLValue (LIndField i e f) = i ++ "[" ++ pExpr e ++ "]." ++ f

pProcedure :: Procedure -> String
pProcedure (Procedure i ps vs ss) = "procedure " ++ i 
                                      ++ " (" ++ pParamL ps ++")\n" 
                                    ++ vars vs ++ "{\n" ++ pStmt ss ++ "}"
  where 
    vars [] = []
    vars vs = intercalate ";\n" (map (indent . pVar) vs) ++ ";\n"

pVar :: Var -> String
pVar (Var t is) = pTypeName t ++ " " ++ intercalate ", " is

pParam :: Param -> String
pParam (ParamAlias t i)    = t ++ " " ++ i
pParam (ParamBase t m i) = pBaseType t ++ pMode m ++ i

pMode :: Mode -> String
pMode Val = " val "
pMode Ref = " "

pParamL :: [Param] -> String
pParamL ds = intercalate ", " (map pParam ds)

pArray :: Array -> String
pArray (Array s t i) = "array[" ++ show s ++ "] " 
                         ++ pTypeName t ++ " " ++ i ++ ";"

pRecord :: Record -> String
pRecord (Record fs i) = "record\n" ++ indent "{ " ++ fields ++ "\n"
                                   ++ indent "} " ++ i ++ ";"
  where fields = intercalate ("\n" ++ indent "; ") (map pField fs) 

pField :: Field -> String
pField (Field t i) = pBaseType t ++ " " ++ i

pTypeName :: TypeName -> String
pTypeName (Base b)  = pBaseType b
pTypeName (Alias i) = i

pBaseType :: BaseType -> String
pBaseType IntType  = "integer"
pBaseType BoolType = "boolean"

---------------------
-- Printer Utilities
---------------------

indent :: String -> String
indent s = replicate 4 ' ' ++ s

paren :: String -> String
paren s = "(" ++ s ++ ")"

isParenOp :: Expr -> Bool
isParenOp (BinOpExpr _ _ _) = True
isParenOp _                 = False
