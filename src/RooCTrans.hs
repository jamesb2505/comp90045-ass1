module RooCTrans
    ( runCTrans
    ) where

import qualified RooAST as AST
import qualified RooSymbolTable as ST

import Control.Monad (liftM)
import Data.List (intercalate)
import Data.Maybe (fromJust)

runCTrans :: AST.Program -> ST.SymbolTable -> Either String String
runCTrans (AST.Program rs as ps) st =
  do 
    typedefs <- transTypedefs rs as
    forwardDecs <- transForwardDecs ps
    procs <- transProcs st ps
    return $ "#include <stdio.h>\n#include <stdlib.h>\n#include <string.h>\n\n" 
          ++ unlines typedefs
          ++ unlines forwardDecs 
          ++ "\nint main(int argc, int *argv[]) {\n"
          ++ "    main_p();\n    return 0;\n}\n\n"
          ++ unlines procs 

transTypedefs :: [AST.Record] -> [AST.Array] -> Either String [String]
transTypedefs rs as =
  do
    records <- mapM transRecord rs
    arrays <- mapM transArray as
    return $ records ++ arrays

transRecord :: AST.Record -> Either String String
transRecord (AST.Record fs alias) = 
  do 
    fields <- mapM transField fs
    return $ "typedef struct {" 
          ++ concatMap ("\n    " ++) fields 
          ++ "\n} " ++ fmtAlias alias ++ ";"

transField :: AST.Field -> Either String String
transField (AST.Field t name) = return $ fmtAtomic t ++ " " 
                                      ++ fmtField name ++ ";"

transArray :: AST.Array -> Either String String
transArray (AST.Array size t alias) 
  = return $ "typedef " ++ fmtType t ++ " " ++ fmtAlias alias
          ++ " [" ++ show size ++ "];"

transForwardDecs :: [AST.Procedure] -> Either String [String]
transForwardDecs ps = mapM transForwardDec ps

transForwardDec :: AST.Procedure -> Either String String
transForwardDec p = (++ ";") <$> transProcHeader p

transProcHeader :: AST.Procedure -> Either String String 
transProcHeader (AST.Procedure name ps _ _) 
  = ("void " ++) <$> (fmtProcName name ++) <$> ("(" ++) 
    <$> (++ ")") <$> transParams ps

transParams :: [AST.Param] -> Either String String
transParams ps = intercalate ", " <$> mapM transParam ps 

transParam :: AST.Param -> Either String String
transParam (AST.ParamAtomic t mode ident)
  = return $ fmtAtomic t ++ " " ++ fmtMode mode ++ fmtIdent ident
transParam (AST.ParamAlias alias ident)
  = return $ fmtAlias alias ++ " *" ++ fmtIdent ident

transProcs :: ST.SymbolTable -> [AST.Procedure] -> Either String [String]
transProcs st ps = mapM (transProc st) ps

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

transVars :: ST.SymbolTable -> [AST.Var] -> Either String [String]
transVars st vs = liftM (map indent) $ mapM (transVar st) vs

transVar :: ST.SymbolTable -> AST.Var -> Either String String
transVar st (AST.Var t vs) = 
  do
    vars <- mapM (return . fmtIdent) vs
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

transStmts :: ST.SymbolTable -> [AST.Stmt] -> Either String [String] 
transStmts st ss = liftM concat $ mapM ((map indent <$>) . transStmt st) ss

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
    case ST.getLValueType st l of
      AST.IntT  -> return $ [ "if (scanf(\"%d\", " ++ lCode
                              ++ ") != 1) { fprintf(stderr, "
                              ++ "\"cannot read integer\\n\"); exit(1); }" ]
      AST.BoolT -> return $ [ "{ char buf[256]; " 
                              ++ "if (scanf(\"%s\", buf) <= 0 "
                              ++ "|| (strcmp(buf, \"true\") "
                              ++ "&& strcmp(buf, \"false\"))) { "
                              ++ "fprintf(stderr, "
                              ++ "\"cannot read boolean\\n\"); exit(1); } *" 
                              ++ lCode ++ " = strcmp(buf, \"true\") == 0; }" ]
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
    pCode <- mapM (uncurry (transArg st)) (zip params args)
    if ST.isTableKey name ps
    then return [ fmtProcName name ++ "(" ++ intercalate ", " pCode ++ ");" ]
    else Left $ "unknown procedure `" ++ name ++ "`"

transArg :: ST.SymbolTable -> ST.Param -> AST.Expr -> Either String String
transArg st (ST.Param _ AST.Ref _) (AST.LVal _ lval) 
  = transLValue st lval
transArg st _ e 
  = transExpr st e

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

transLValue :: ST.SymbolTable -> AST.LValue -> Either String String
transLValue st (AST.LId ident) 
  = return $ if ST.isRef st ident
             then fmtIdent ident
             else "&" ++ fmtIdent ident
transLValue st (AST.LField ident field) 
  = return $ if ST.isRef st ident
             then "&" ++ fmtIdent ident ++ "->" ++ fmtField field
             else "&" ++ fmtIdent ident ++ "." ++ fmtField field
transLValue st (AST.LInd ident e) =
  do 
    eCode <- transExpr st e
    return $ if ST.isRef st ident
             then "&(*" ++ fmtIdent ident ++ ")[" ++ eCode ++ "]"
             else "&(" ++ fmtIdent ident ++ "[" ++ eCode ++ "])"
transLValue st (AST.LIndField ident e field) =
  do 
    eCode <- transExpr st e
    return $ if ST.isRef st ident
             then "&(*" ++ fmtIdent ident ++ ")[" ++ eCode ++ "]."
                  ++ fmtField field
             else "&(" ++ fmtIdent ident ++ "[" ++ eCode ++ "]." 
                  ++ fmtField field ++ ")"

fmtAtomic :: AST.AtomicType -> String
fmtAtomic _ = "int"

fmtAlias :: AST.Ident -> String
fmtAlias a = fmtIdentifier a ++ "_t"

fmtType :: AST.TypeName -> String
fmtType (AST.Alias a) = fmtAlias a
fmtType _             = "int"

fmtProcName :: AST.Ident -> String
fmtProcName name = fmtIdentifier name ++ "_p"

fmtField :: AST.Ident -> String
fmtField f = fmtIdentifier f

fmtMode :: AST.Mode -> String
fmtMode AST.Ref = "*"
fmtMode AST.Val = ""

fmtIdent :: AST.Ident -> AST.Ident
fmtIdent ident = fmtIdentifier ident ++ "_i"

indent :: String -> String
indent s = "    " ++ s

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

fmtUnOp :: AST.UnOp -> String
fmtUnOp AST.Op_not = "!"
fmtUnOp AST.Op_neg = "-"

fmtIdentifier :: AST.Ident -> String
fmtIdentifier ident = concatMap fmtIdentChar ident
  where
    fmtIdentChar '_'  = "__"
    fmtIdentChar '\'' = "_a"
    fmtIdentChar c    = [c]