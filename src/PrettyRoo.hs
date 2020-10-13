module PrettyRoo (prettyPrint) where

import RooAST

import Data.List (intercalate)

-- prettyPrint
-- Constructs a pretty printed String representation of a Program
prettyPrint :: Program -> String
prettyPrint (Program rs as ps) 
  = decs pRecord rs ++ decs pArray as
    ++ (if null rs && null as then "" else "\n")
    ++ intercalate "\n\n" (map pProcedure ps)
  where
    decs _ [] = ""
    decs p ds = unlines $ map p ds

-- pStmts
-- Constructs a pretty printed String representation of a [Stmt]
pStmts :: [Stmt] -> String
pStmts ss = unlines $ pListStmts ss

-- pListStmts
-- Constructs a pretty printed [String] representation of a [Stmt]
-- unlines to get String
pListStmts :: [Stmt] -> [String]
pListStmts ss = concatMap (map indent . pStmt) ss

-- pStmt
-- Constructs a pretty printed [String] representation of a Stmt
-- unlines to get String
pStmt :: Stmt -> [String]
pStmt (Assign l e)     = [ pLValue l ++ " <- " ++ pExpr e ++ ";" ]
pStmt (Read l)         = [ "read " ++ pLValue l ++ ";" ]
pStmt (Write e)        = [ "write " ++ pExpr e ++ ";" ]
pStmt (Writeln e)      = [ "writeln " ++ pExpr e ++ ";" ]
pStmt (If e ss)        = [ "if " ++ pExpr e ++ " then" ]
                      ++ pListStmts ss
                      ++ [ "fi" ]
pStmt (IfElse e ts fs) = [ "if " ++ pExpr e ++ " then" ]
                      ++ pListStmts ts 
                      ++ [ "else" ]
                      ++ pListStmts fs
                      ++ [ "fi" ]
pStmt (While e ss)     = [ "while " ++ pExpr e ++ " do" ]
                      ++ pListStmts ss
                      ++ [ "od" ]
pStmt (Call f es)      = [ "call " ++ f ++ "(" ++ pExprComma es ++ ");" ]

-- pExpr
-- Constructs a pretty printed String representation of a Expr
pExpr :: Expr -> String
pExpr (LVal _ l)          = pLValue l
pExpr (BoolConst _ b)     = if b then "true" else "false"
pExpr (IntConst _ i)      = show i
pExpr (StrConst _ s)      = show s
pExpr (UnOpExpr _ o e)    = pUnOp o ++ (if isParenExpr e && prec o > prec e
                                        then paren $ pExpr e
                                        else pExpr e)
pExpr (BinOpExpr _ o l r) = binParen isRAssoc o l
                            ++ pBinOp o
                            ++ binParen isLAssoc o r
  where 
    -- uses the minimal amount of required parens
    binParen :: (BinOp -> Bool) -> BinOp -> Expr -> String
    binParen assoc op expr 
      = if prec op > prec expr || (prec op == prec expr && assoc op)
        then paren $ pExpr expr
        else pExpr expr

-- pExprComma
-- Constructs a comma separated pretty printed String representation of a 
-- [Expr]
pExprComma :: [Expr] -> String
pExprComma es = intercalate ", " $ map pExpr es

-- pUnOp
-- Constructs a pretty printed String representation of a UnOp
pUnOp :: UnOp -> String
pUnOp Op_not = "not "
pUnOp Op_neg = "-"

-- pBinOp
-- Constructs a pretty printed String representation of a BinOp
pBinOp :: BinOp -> String
pBinOp Op_or  = " or "
pBinOp Op_and = " and "
pBinOp Op_eq  = " = "
pBinOp Op_ne  = " != "
pBinOp Op_lt  = " < "
pBinOp Op_le  = " <= "
pBinOp Op_gt  = " > "
pBinOp Op_ge  = " >= "
pBinOp Op_add = " + "
pBinOp Op_sub = " - "
pBinOp Op_mul = " * "
pBinOp Op_div = " / "

-- pLValue
-- Constructs a pretty printed String representation of a LValue
pLValue :: LValue -> String
pLValue (LId i)           = i
pLValue (LField i f)      = i ++ "." ++ f
pLValue (LInd i e)        = i ++ "[" ++ pExpr e ++ "]"
pLValue (LIndField i e f) = i ++ "[" ++ pExpr e ++ "]." ++ f

pProcedure :: Procedure -> String
pProcedure (Procedure i ps vs ss) = "procedure " ++ i 
                                    ++ " (" ++ pParamComma ps ++")\n" 
                                    ++ vars vs ++ "{\n" ++ pStmts ss ++ "}"
  where 
    vars [] = []
    vars vs = intercalate ";\n" (map (indent . pVar) vs) ++ ";\n"

-- pVar
-- Constructs a pretty printed String representation of a Var
pVar :: Var -> String
pVar (Var t is) = pTypeName t ++ " " ++ intercalate ", " is

-- pParam
-- Constructs a pretty printed String representation of a Param
pParam :: Param -> String
pParam (ParamAlias t i)    = t ++ " " ++ i
pParam (ParamAtomic t m i) = pAtomicType t ++ pMode m ++ i

-- pMode
-- Constructs a pretty printed String representation of a Mode
pMode :: Mode -> String
pMode Val = " val "
pMode Ref = " "

-- pParamComma
-- Constructs a comma separated pretty printed String representation of a 
-- [Param]
pParamComma :: [Param] -> String
pParamComma ds = intercalate ", " $ map pParam ds

-- pArray
-- Constructs a pretty printed String representation of an Array
pArray :: Array -> String
pArray (Array s t i) = "array[" ++ show s ++ "] " 
                       ++ pTypeName t ++ " " ++ i ++ ";"

-- pRecord
-- Constructs a pretty printed String representation of a Record
pRecord :: Record -> String
pRecord (Record fs i) = "record\n" ++ indent "{ " ++ fields ++ "\n"
                        ++ indent "} " ++ i ++ ";"
  where fields = intercalate ("\n" ++ indent "; ") $ map pField fs

-- pField
-- Constructs a pretty printed String representation of a Field
pField :: Field -> String
pField (Field t i) = pAtomicType t ++ " " ++ i

-- pTypeName
-- Constructs a pretty printed String representation of a TypeName
pTypeName :: TypeName -> String
pTypeName (Atomic b)  = pAtomicType b
pTypeName (Alias i) = i

-- pAtomicType
-- Constructs a pretty printed String representation of a AtomicType
pAtomicType :: AtomicType -> String
pAtomicType IntType  = "integer"
pAtomicType BoolType = "boolean"

-- indentation
-- Specifies the indentation used for indent
indentation :: Int
indentation = 4

-- indent
-- Indents a String with preceeding spaces
indent :: String -> String
indent s = replicate indentation ' ' ++ s

-- paren
-- Wraps a String in parentheses
paren :: String -> String
paren s = "(" ++ s ++ ")"

-- isParenExpr
-- Specifies if an Expr requires parentheses
isParenExpr :: Expr -> Bool
isParenExpr (BinOpExpr _ _ _ _) = True
isParenExpr _                   = False
