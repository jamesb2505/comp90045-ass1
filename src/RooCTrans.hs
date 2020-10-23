module RooCTrans
    ( runCTrans
    ) where

import qualified RooAST as AST
import qualified RooSymbolTable as ST

import Control.Monad (liftM)
import Data.List (intercalate)
import Data.Maybe (fromJust)



-- runCTrans 
-- Performs C transpilations of a Roo Program
runCTrans :: AST.Program -> ST.SymbolTable -> Either String String
runCTrans (AST.Program rs as ps) st =
  do 
    records <- mapM transRecord rs
    arrays <- mapM transArray as
    forwardDecs <- mapM transForwardDec ps
    procs <- mapM (transProc st) ps
    return $ "#include <stdio.h>\n#include <stdlib.h>\n#include <string.h>\n\n" 
          ++ unlines records
          ++ unlines arrays
          ++ unlines forwardDecs 
          ++ "\nint main(int argc, int *argv[]) {\n\
             \    main_p();\n    return 0;\n}\n\n"
          ++ unlines procs 

-- transRecord 
-- Transpiles a Roo Record into a C struct
transRecord :: AST.Record -> Either String String
transRecord (AST.Record fs alias) = 
  do 
    fields <- mapM transField fs
    return $ "typedef struct {" 
          ++ concatMap ("\n    " ++) fields 
          ++ "\n} " ++ fmtAlias alias ++ ";"

-- transField 
-- Transpiles a Roo record field into a C struct member 
transField :: AST.Field -> Either String String
transField (AST.Field t name) = return $ fmtAtomic t ++ " " 
                                      ++ fmtField name ++ ";"

-- transArray 
-- Transpiles a Roo array declaration into a C array typedef
transArray :: AST.Array -> Either String String
transArray (AST.Array size t alias) 
  = return $ "typedef " ++ fmtType t ++ " " ++ fmtAlias alias
          ++ " [" ++ show size ++ "];"

-- transForwardDec 
-- Transpiles a Roo Procedure into a C-style forward declaration
transForwardDec :: AST.Procedure -> Either String String
transForwardDec p = (++ ";") <$> transProcHeader p

-- transProcHeader 
-- Transpiles a Roo Procedure into a C-style function header
transProcHeader :: AST.Procedure -> Either String String 
transProcHeader (AST.Procedure name ps _ _) 
  = ("void " ++) <$> (fmtProcName name ++) <$> ("(" ++) 
    <$> (++ ")") <$> transParams ps

-- transParams 
-- Transpiles a list of Roo Params into a list of C function parameters
transParams :: [AST.Param] -> Either String String
transParams ps = intercalate ", " <$> mapM transParam ps 

-- transParam 
-- Transpiles a Roo Param into a C function parameter
transParam :: AST.Param -> Either String String
transParam (AST.ParamAtomic t mode ident)
  = return $ fmtAtomic t ++ " " ++ fmtMode mode ++ fmtLocal ident
transParam (AST.ParamAlias alias ident)
  = return $ fmtAlias alias ++ " *" ++ fmtLocal ident

-- transProc 
-- Transpiles a Roo Procedure into a C function definition
transProc :: ST.SymbolTable -> AST.Procedure -> Either String String
transProc st@(ST.SymbolTable _ _ ps) p@(AST.Procedure name _ vs ss) =
  do
    let proc = fromJust $ lookup name ps
    let st' = st { ST.unProcedures = (name, proc):ps }
    header <- transProcHeader p
    vars <- transVars st' vs
    stmts <- transStmts st' ss
    if ST.isTableKey name ps 
    then return $ header ++ " {\n" 
               ++ unlines vars
               ++ unlines stmts
               ++ "}"
    else Left $ "unknown procedure `" ++ name ++ "`"

-- transVars 
-- Transpiles a list of Roo Vars into a list of C variable declarations
transVars :: ST.SymbolTable -> [AST.Var] -> Either String [String]
transVars st vs = liftM (map indent) $ mapM (transVar st) vs

-- transVar 
-- Transpiles a Roo Var declaration into a C variable declaration
transVar :: ST.SymbolTable -> AST.Var -> Either String String
transVar st (AST.Var t vs) = 
  do
    vars <- mapM (return . fmtLocal) vs
    return $ fmtType t ++ " " 
          ++ (intercalate ", " . map (++ " = " ++ initVal)) vars ++ ";"
  where 
    initVal = case t of
                AST.Atomic _ -> "0"
                AST.Alias a  -> if AST.isArrayT tArr && AST.isRecordT tRec
                                then "{{0}}"
                                else "{0}"
                  where 
                    tArr = ST.getAliasType st a
                    AST.ArrayT _ tRec = tArr 

-- transStmts 
-- Transpiles a list of Roo Stmts into a list of C statements
transStmts :: ST.SymbolTable -> [AST.Stmt] -> Either String [String] 
transStmts st ss = liftM concat $ mapM ((map indent <$>) . transStmt st) ss

-- transStmt 
-- Transpiles a Roo Stmt into a list of C statements
transStmt :: ST.SymbolTable -> AST.Stmt -> Either String [String]
transStmt st (AST.Assign lval (AST.LVal _ rval))
  | ST.isRef st (AST.getLId lval) && ST.isRef st (AST.getLId rval) = 
    do
      lCode <- transLValue st lval
      rCode <- transLValue st rval
      let size = ST.lookupTotalSize st . AST.getTypeName 
                 $ ST.getLValueType st rval
      return [ "memcpy(" ++ lCode ++ ", " ++ rCode 
               ++ ", (" ++ show size ++ ") * sizeof(int));" ]
transStmt st (AST.Assign l e) = 
  do 
    lCode <- transLValue st l
    eCode <- transExpr st e
    return [ "*" ++ lCode ++ " = " ++ eCode ++ ";" ]
transStmt st (AST.Read l) =
  do
    lCode <- transLValue st l  
    -- need to error check depending on type
    case ST.getLValueType st l of
      AST.IntT  -> return $ [ "if (scanf(\"%d\", " ++ lCode
                              ++ ") != 1) { fprintf(stderr, \
                                 \\"cannot read integer\\n\"); exit(1); }" ]
      AST.BoolT -> return $ [ "{ char buf[7]; \
                              \if (scanf(\"%6s\", buf) != 1) { \
                              \fprintf(stderr, \
                              \\"cannot read bool\\n\"); exit(1); \
                              \} else if (!strcmp(buf, \"true\")) { *"
                              ++ lCode ++ " = 1; \
                              \} else if (!strcmp(buf, \"false\")) { *"
                              ++ lCode ++ " = 0; \
                              \} else { fprintf(stderr, \
                              \\"read invalid bool\\n\"); exit(1); } }" ]
      _         -> Left "bad read type"
transStmt st (AST.Write e) = 
  do
    eCode <- transExpr st e
    case AST.getExprType e of
      AST.BoolT -> return [ "printf(" ++ eCode ++ " ? \"true\" : \"false\");" ]
      AST.IntT  -> return [ "printf(\"%d\", " ++ eCode ++ ");" ]
      AST.StrT  -> return [ "printf(\"%s\", " ++ eCode ++ ");" ]
      _         -> Left "bad write type"
transStmt st (AST.Writeln e) = 
  do 
    writeCode <- transStmt st (AST.Write e)
    return $ writeCode
          ++ [ "printf(\"\\n\");" ]
transStmt st (AST.If e ss) =
  do
    eCode <- transExpr st e
    ssCode <- transStmts st ss 
    return $ ("if (" ++ eCode ++ ") {")
           : ssCode
          ++ [ "}" ]           
transStmt st (AST.IfElse e ts fs) = 
  do
    eCode <- transExpr st e
    tsCode <- transStmts st ts 
    fsCode <- transStmts st fs 
    return $ ("if (" ++ eCode ++ ") {")
           : tsCode
          ++ [ "} else {" ]
          ++ fsCode
          ++ [ "}" ]    
transStmt st (AST.While e ss) = 
  do
    eCode <- transExpr st e
    ssCode <- transStmts st ss 
    return $ ("while (" ++ eCode ++ ") {")
           : ssCode
          ++ [ "}" ]
transStmt st@(ST.SymbolTable _ _ ps) (AST.Call name args) =
  do 
    let params = map snd . ST.unParams . fromJust $ lookup name ps
    pCode <- mapM (uncurry $ transArg st) (zip params args)
    if ST.isTableKey name ps
    then return [ fmtProcName name ++ "(" ++ intercalate ", " pCode ++ ");" ]
    else Left $ "unknown procedure `" ++ name ++ "`"

-- transArg 
-- Transpiles a Roo call argument into a C function argument
transArg :: ST.SymbolTable -> ST.Param -> AST.Expr -> Either String String
transArg st (ST.Param _ AST.Ref _) (AST.LVal _ lval) 
  = transLValue st lval
transArg st _ e 
  = transExpr st e

-- transExpr 
-- Transpiles a Roo Expr into a C expression
transExpr :: ST.SymbolTable -> AST.Expr -> Either String String
transExpr st (AST.LVal _ l) = ("*" ++) <$> transLValue st l 
transExpr _ (AST.BoolConst _ b) = return $ if b then "1" else "0"
transExpr _ (AST.IntConst _ i) = return $ show i
transExpr _ (AST.StrConst _ s) = return $ show s
transExpr st (AST.BinOpExpr _ op a b) = 
  do 
    aCode <- transExpr st a
    bCode <- transExpr st b
    return $ "(" ++ aCode ++ ") " ++ fmtBinOp op ++ " (" ++ bCode ++ ")" 
transExpr st (AST.UnOpExpr _ op a) 
  = (fmtUnOp op ++) <$> ("(" ++) <$> (++ ")") <$> transExpr st a

-- transLValue 
-- Transpiles a Roo LValue into a C lvalue
-- C lvalue is by referece, so to get the value, prepend a *
transLValue :: ST.SymbolTable -> AST.LValue -> Either String String
transLValue st (AST.LId ident) 
  = return $ if ST.isRef st ident
             then fmtLocal ident
             else "&" ++ fmtLocal ident
transLValue st (AST.LField ident field) 
  = return $ if ST.isRef st ident
             then "&" ++ fmtLocal ident ++ "->" ++ fmtField field
             else "&" ++ fmtLocal ident ++ "." ++ fmtField field
transLValue st (AST.LInd ident e) =
  do 
    eCode <- transExpr st e
    return $ if ST.isRef st ident
             then "&(*" ++ fmtLocal ident ++ ")[" ++ eCode ++ "]"
             else "&(" ++ fmtLocal ident ++ "[" ++ eCode ++ "])"
transLValue st (AST.LIndField ident e field) =
  do 
    eCode <- transExpr st e
    return $ if ST.isRef st ident
             then "&(*" ++ fmtLocal ident ++ ")[" ++ eCode ++ "]."
                  ++ fmtField field
             else "&(" ++ fmtLocal ident ++ "[" ++ eCode ++ "]." 
                  ++ fmtField field ++ ")"


-- fmtIdent 
-- Formats an Roo Ident to be a vlaid C identifier
-- C does not allow ' in identifiers, and as such ' are replaced with _a
-- and _ are replaced with __ to avoid clashes 
fmtIdent :: AST.Ident -> String
fmtIdent ident = concatMap fmtIdentChar ident
  where
    fmtIdentChar '_'  = "__"
    fmtIdentChar '\'' = "_a"
    fmtIdentChar c    = [c]

-- fmtAtomic 
-- Formats a Roo AtomicType into a C type
-- All types are converted to int
fmtAtomic :: AST.AtomicType -> String
fmtAtomic _ = "int"

-- fmtAlias 
-- Formats an Roo alias (record/array name) into a C name
-- Appends _t to avoid namespace clashes
fmtAlias :: AST.Ident -> String
fmtAlias a = fmtIdent a ++ "_t"

-- fmtType 
-- Formats a Roo TypeName into a C type
fmtType :: AST.TypeName -> String
fmtType (AST.Alias a) = fmtAlias a
fmtType _             = "int"

-- fmtProcName 
-- Formats a Roo Procedure name into a C name
-- Appends _p to avoid namespace clashes 
fmtProcName :: AST.Ident -> String
fmtProcName name = fmtIdent name ++ "_p"

-- fmtField 
-- Formats a Roo Field name name into a C name
fmtField :: AST.Ident -> String
fmtField f = fmtIdent f

-- fmtMode 
-- Formats a Roo Mode name name into a C mode
fmtMode :: AST.Mode -> String
fmtMode AST.Ref = "*"
fmtMode AST.Val = ""

-- fmtLocal 
-- Formats a Roo Procedure variable/parameter into a C name
-- Appends _i to avoid namespace clashes 
fmtLocal :: AST.Ident -> AST.Ident
fmtLocal ident = fmtIdent ident ++ "_i"

-- fmtBinOp 
-- Formats a Roo BinOp into a C binary operator
fmtBinOp :: AST.BinOp -> String
fmtBinOp AST.Op_or  = "||"
fmtBinOp AST.Op_and = "&&"
fmtBinOp AST.Op_eq  = "=="
fmtBinOp AST.Op_ne  = "!="
fmtBinOp AST.Op_lt  = "<"
fmtBinOp AST.Op_le  = "<="
fmtBinOp AST.Op_gt  = ">"
fmtBinOp AST.Op_ge  = ">="
fmtBinOp AST.Op_add = "+"
fmtBinOp AST.Op_sub = "-"
fmtBinOp AST.Op_mul = "*"
fmtBinOp AST.Op_div = "/"

-- fmtUnOp 
-- Formats a Roo UnOp into a C unary operator
fmtUnOp :: AST.UnOp -> String
fmtUnOp AST.Op_not = "!"
fmtUnOp AST.Op_neg = "-"

-- indent 
-- Indents a String
indent :: String -> String
indent s = "    " ++ s