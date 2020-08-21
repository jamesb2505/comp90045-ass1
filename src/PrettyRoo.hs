module PrettyRoo (pprint) where

import RooAST

import Data.List (intercalate)

-- testing
prog = Program [r] [a] [p, p2, p3]
p = Procedure [Decl BoolType "h"] [] 
p2 = Procedure [Decl IntType "h"] [Assign (LId "i") (Lval (LId "j"))] 
p3 = Procedure [] [Assign (LId "i") cf, Assign (LId "i") cc, w, i, e] 
r = Record [Decl BoolType "b", Decl IntType "i"] "r"
a = Array 10 BoolType "r"
w = While (BoolConst False) [Assign (LId "i") (Lval (LId "j"))]
i = If (BoolConst True) [w]
e = IfElse (BoolConst True) [Assign (LId "u") cf] [w]
cf = BinOpExpr Op_div (BinOpExpr Op_mul 
                        (BinOpExpr Op_add (IntConst 1) (IntConst 2)) 
                        (IntConst 3)) 
                      (IntConst 4)
-- ((13+(3*5)*7)-(2*7))-(11/(20/(2*5)))
cc = BinOpExpr Op_sub 
  (BinOpExpr Op_sub 
    (BinOpExpr Op_add 
      (IntConst 13)
      (BinOpExpr Op_mul 
        (BinOpExpr Op_mul (IntConst 3) (IntConst 5))
        (IntConst 7)
      )
    )
    (BinOpExpr Op_mul (IntConst 2) (IntConst 2))
  ) 
  (BinOpExpr Op_div
    (IntConst 11)
    (BinOpExpr Op_div
      (IntConst 20)
      (BinOpExpr Op_mul (IntConst 2) (IntConst 5) )
    )
  )

pprint :: Program -> String
pprint (Program r a p) 
  = intercalate "\n" [ decs pRecord r
                     , decs pArray a
                     , (if null r && null a then "" else "\n")
                       ++ intercalate "\n" (map pProcedure p)
                     ]
  where
    decs _ [] = ""
    decs p ds = intercalate "\n" (map p ds)

pDecl :: Decl -> String
pDecl (Decl t i) = pTypeName t ++ " " ++ i

pStmt :: Int -> Stmt -> String
pStmt i (Assign l e)     = indent i $ pLValue l ++ " <- " ++ pExpr e ++ ";"
pStmt i (Read l)         = indent i $ "read " ++ pLValue l ++ ";"
pStmt i (Write e)        = indent i $ "write " ++ pExpr e ++ ";"
pStmt i (If e ss)        = indent i "if " ++ pExpr e ++ " then\n"
                           ++ pStmtList (i + 1) ss ++ "\n"
                           ++ indent i "fi"
pStmt i (IfElse e ts fs) = indent i "if " ++ pExpr e ++ " then\n"
                           ++ pStmtList (i + 1) ts ++ "\n" 
                           ++ (indent i "else\n")
                           ++ pStmtList (i + 1) fs ++ "\n"
                           ++ indent i "fi"
pStmt i (While e ss)     = indent i "while " ++ pExpr e ++ " do\n"
                           ++ pStmtList (i + 1) ss ++ "\n"
                           ++ indent i "od"
pStmt i (Call f es)      = indent i "call " ++ f 
                             ++ "(" ++ pExprList es ++ ");"

pStmtList :: Int -> [Stmt] -> String
pStmtList i ss = intercalate "\n" (map (pStmt i) ss)

pExpr :: Expr -> String
pExpr (Lval l)          = pLValue l
pExpr (BoolConst b)     = if b then "true" else "false"
pExpr (IntConst i)      = show i
pExpr (StrConst s)      = show s
pExpr (UnOpExpr o e)    = pUnOp o ++ (if isOp e
                                      then paren (pExpr e)
                                      else (pExpr e))
pExpr (BinOpExpr o l r) = binParen isRAssoc o l
                          ++ " " ++ pBinOp o ++ " "
                          ++ binParen isLAssoc o r
  where 
    binParen r o e = if opPrec o < prec e || (opPrec o == prec e && r o)
                     then paren (pExpr e) 
                     else pExpr e
    prec (BinOpExpr o _ _) = opPrec o 
    prec _                 = -1 

pExprList :: [Expr] -> String
pExprList es = intercalate ", " (map pExpr es)

pUnOp :: UnOp -> String
pUnOp Op_not = "not "
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
pProcedure (Procedure ds ss) = "procedure " ++ {-name-} "<name>" 
                                 ++ " (" ++ pParamList [{-params-}] ++")\n" 
                               ++ decls ds ++ "{\n"
                               ++ pStmtList 1 ss ++ "\n}\n"
  where 
    decls [] = []
    decls ds = intercalate ";\n" (map (indent 1 . pDecl) ds) ++ ";\n"

pParamList :: [Decl] -> String
pParamList ds = intercalate ", " (map pDecl ds)

pArray :: ArrayDef -> String
pArray (Array s t i) = "array [" ++ show s ++ "] " 
                       ++ pTypeName t ++ " " ++ i ++ ";"

pRecord :: RecordDef -> String
pRecord (Record d i) = "record \n" ++ indent 1 "{ " 
                       ++ intercalate "\n    ; " (map pDecl d) ++ "\n"
                       ++ indent 1 "} " ++ i ++ ";"

pTypeName :: TypeName -> String
pTypeName BoolType      = "boolean"
pTypeName IntType       = "integer"
pTypeName (TypeAlias i) = i

indent :: Int -> String -> String
indent i s = replicate (4 * i) ' ' ++ s

paren :: String -> String
paren s = "(" ++ s ++ ")"

isLAssoc :: BinOp -> Bool
isLAssoc _  = True

isRAssoc :: BinOp -> Bool
isRAssoc = not . isLAssoc

opPrec :: BinOp -> Int
opPrec Op_or  = 1
opPrec Op_and = 2
opPrec Op_mul = 2
opPrec Op_div = 2
opPrec _      = 3

isOp :: Expr -> Bool
isOp (BinOpExpr _ _ _)  = True
isOp (UnOpExpr _ _)     = True
isOp _                  = False
