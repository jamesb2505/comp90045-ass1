-------------------------
-- RooPyTrans
--
-- Written by James Barnes, Jack Macumber, & Isitha Subasinghe
--
-- Roo -> Python transcompiler
-------------------------

module RooPyTrans
    ( runPyTrans
    ) where

import qualified RooAST as AST
import qualified RooSymbolTable as ST

import Control.Monad (liftM)
import Data.List (intercalate)
import Data.Maybe (fromJust)

runPyTrans :: AST.Program -> ST.SymbolTable -> Either String String
runPyTrans (AST.Program _ _ ps) st =
  do 
    procs <- transProcs st ps
    return $ "from copy import deepcopy\n\n"
          ++ unlines procs 
          ++ "if __name__ == '__main__':\n\
             \    main_p()\n"

transProcHeader :: AST.Procedure -> Either String String 
transProcHeader (AST.Procedure name ps _ _) 
  = ("def " ++)<$> (fmtProcName name ++) <$> ("(" ++) 
    <$> (++ ")") <$> transParams ps

transReturnValues :: [AST.Param] -> Either String String
transReturnValues ps
  = return . intercalate ", " . map getName $ filter isRef ps
  where
    isRef (AST.ParamAtomic _ AST.Val _) = False
    isRef _ = True
    getName (AST.ParamAtomic _ _ name) = fmtIdent name
    getName (AST.ParamAlias _ name) = fmtIdent name

transParams :: [AST.Param] -> Either String String
transParams ps = intercalate ", " <$> mapM transParam ps 

transParam :: AST.Param -> Either String String
transParam (AST.ParamAtomic _ _ ident)
  = return $ fmtIdent ident
transParam (AST.ParamAlias _ ident)
  = return $ fmtIdent ident

transProcs :: ST.SymbolTable -> [AST.Procedure] -> Either String [String]
transProcs st ps = mapM (transProc st) ps

transProc :: ST.SymbolTable -> AST.Procedure -> Either String String
transProc st@(ST.SymbolTable _ _ ps) p@(AST.Procedure name params vs ss) =
  do
    let proc = fromJust $ lookup name ps
    let st' = st { ST.unProcedures = (name, proc):ps }
    header <- transProcHeader p
    vars <- transVars st' vs
    stmts <- transStmts st' ss
    ret <- transReturnValues params
    if ST.isTableKey name ps 
    then return $ header ++ ":\n" 
               ++ unlines vars
               ++ unlines stmts
               ++ indent "return" ++ fixReturn (" " ++ ret) ++ "\n"
    else Left $ "unknown procedure `" ++ name ++ "`"
  where 
    fixReturn " " = ""
    fixReturn ret = ret

transVars :: ST.SymbolTable -> [AST.Var] -> Either String [String]
transVars st vs = liftM (map indent) $ mapM (transVar st) vs

transVar :: ST.SymbolTable -> AST.Var -> Either String String
transVar st (AST.Var t vs) = 
  do
    vars <- mapM (return . fmtIdent) vs
    initV <- initVal
    return $ (intercalate "; " . map (++ " = " ++ initV)) vars ++ ";"
  where 
    initVal = case t of
                AST.Atomic t' -> initAtomicType t'
                AST.Alias a  -> initExprType st $ ST.getAliasType st a
                     
initExprType :: ST.SymbolTable -> AST.ExprType -> Either String String
initExprType (ST.SymbolTable rs _ _ ) (AST.RecordT ident) 
  = case lookup ident rs of
      Just (ST.Record fs) ->   
        do 
          let names = map fst fs
          let ts = map (\(_,ST.Field t _) -> t) fs
          inits <- mapM initAtomicType ts
          return $ "{" 
                ++ intercalate ", " (zipWith fmtFields inits names) 
                ++ "}" 
        where fmtFields i n = "'" ++ fmtField n ++ "':" ++ i
      _ -> Left $ "unknown record " ++ ident
initExprType st@(ST.SymbolTable _ as _ ) (AST.ArrayT ident _) 
 = case lookup ident as of
    Just (ST.Array t size) -> 
      do 
        initV <- initVal
        return $ "[" ++ initV ++ " for _ in range(" ++ show size ++ ")]"  
      where 
        initVal = case t of
          AST.Atomic t' -> initAtomicType t'
          AST.Alias a -> initExprType st $ ST.getAliasType st a
    _ -> Left $ "unknown array " ++ ident
initExprType _ _ = Left "unknown alias"

initAtomicType :: AST.AtomicType -> Either String String
initAtomicType AST.BoolType = return "False"
initAtomicType AST.IntType = return "0"

transStmts :: ST.SymbolTable -> [AST.Stmt] -> Either String [String] 
transStmts st ss = liftM concat $ mapM ((map indent <$>) . transStmt st) ss

transStmt :: ST.SymbolTable -> AST.Stmt -> Either String [String]
transStmt st (AST.Assign lval (AST.LVal _ rval))
  | ST.isRef st (AST.getLId lval) && ST.isRef st (AST.getLId rval) = 
    do
      lCode <- transLValue lval
      rCode <- transLValue rval
      return [ lCode ++ " = deepcopy(" ++ rCode ++ ")"]
transStmt _ (AST.Assign l e) = 
  do 
    lCode <- transLValue l
    eCode <- transExpr e
    return [ lCode ++ " = " ++ eCode ]
transStmt st (AST.Read l) =
  do
    lCode <- transLValue l  
    case ST.getLValueType st l of
      AST.IntT  -> return $ [ lCode ++ " = int(input(''))" ]
      AST.BoolT -> return $ [ lCode ++ " = {'true':1,'false':0}\
                                       \[input('')]" ]
      _         -> Left "bad read type"
transStmt _ (AST.Write e) = 
  do
    eCode <- transExpr e
    case AST.getExprType e of
      AST.BoolT -> return [ "print(str(" ++ eCode ++ ").lower(), end='')" ]
      AST.IntT  -> return [ "print(" ++ eCode ++ ", end='')" ]
      AST.StrT  -> return [ "print(" ++ eCode ++ ", end='')" ]
      _         -> Left "bad write type"
transStmt _ (AST.Writeln e) = 
  do 
    eCode <- transExpr e
    case AST.getExprType e of
      AST.BoolT -> return [ "print(str(" ++ eCode ++ ").lower())" ]
      AST.IntT  -> return [ "print(" ++ eCode ++ ")" ]
      AST.StrT  -> return [ "print(" ++ eCode ++ ")" ]
      _         -> Left "bad write type"
transStmt st (AST.If e ss) =
  do
    eCode <- transExpr e
    ssCode <- transStmts st ss 
    return $ ("if " ++ eCode ++ ":") 
           : ssCode           
transStmt st (AST.IfElse e ts fs) = 
  do
    eCode <- transExpr e
    tsCode <- transStmts st ts 
    fsCode <- transStmts st fs 
    return $ ("if " ++ eCode ++ ":")
           : tsCode
          ++ [ "else:" ]
          ++ fsCode   
transStmt st (AST.While e ss) = 
  do
    eCode <- transExpr e
    ssCode <- transStmts st ss 
    return $ ("while " ++ eCode ++ ":")
           : ssCode
transStmt (ST.SymbolTable _ _ ps) (AST.Call name args) =
  do 
    let params = map snd . ST.unParams . fromJust $ lookup name ps
    pCode <- mapM (uncurry transArg) (zip params args)
    capture <- transReturnCapture params args
    if ST.isTableKey name ps
    then return [ capture ++ " = " 
                  ++ fmtProcName name 
                  ++ "(" ++ intercalate ", " pCode ++ ")" ]
    else Left $ "unknown procedure `" ++ name ++ "`"

transReturnCapture :: [ST.Param] -> [AST.Expr] -> Either String String
transReturnCapture ps es
  = liftM (fixReturn . intercalate ", ") . mapM transLValue
    . map (fromJust . AST.getLVal)
    . filter AST.isLVal . map snd . filter isRef $ zip ps es
  where
    isRef (ST.Param _ AST.Val _,_) = False
    isRef _ = True
    fixReturn "" = "_"
    fixReturn ret = ret

transArg :: ST.Param -> AST.Expr -> Either String String
transArg (ST.Param _ AST.Ref _) (AST.LVal _ lval) 
  = transLValue lval
transArg _ e 
  = transExpr e

transExpr :: AST.Expr -> Either String String
transExpr (AST.LVal _ l) = transLValue l 
transExpr (AST.BoolConst _ b) = return $ if b then "True" else "False"
transExpr (AST.IntConst _ i) = return $ show i
transExpr (AST.StrConst _ s) = return $ show s
transExpr (AST.BinOpExpr _ op a b) = 
  do 
    aCode <- transExpr a
    bCode <- transExpr b
    return $ "(" ++ aCode ++ ") " ++ fmtBinOp op ++ " (" ++ bCode ++ ")" 
transExpr (AST.UnOpExpr _ op a) 
  = (fmtUnOp op ++) <$> ("(" ++) <$> (++ ")") <$> transExpr a

transLValue :: AST.LValue -> Either String String
transLValue (AST.LId ident) 
  = return $ fmtIdent ident
transLValue (AST.LField ident field) 
  = return $ fmtIdent ident ++ "['" ++ fmtField field ++ "']"
transLValue (AST.LInd ident e) =
  do 
    eCode <- transExpr e
    return $ "(" ++ fmtIdent ident ++ ")[" ++ eCode ++ "]"
transLValue (AST.LIndField ident e field) =
  do 
    eCode <- transExpr e
    return $ "(" ++ fmtIdent ident ++ ")[" ++ eCode ++ "]" 
          ++ "['" ++ fmtField field ++ "']"

fmtProcName :: AST.Ident -> String
fmtProcName name = fmtIdentifier name ++ "_p"

fmtField :: AST.Ident -> String
fmtField f = fmtIdentifier f

fmtIdent :: AST.Ident -> AST.Ident
fmtIdent ident = fmtIdentifier ident ++ "_i"

indent :: String -> String
indent s = "    " ++ s

fmtBinOp :: AST.BinOp -> String
fmtBinOp AST.Op_or  = "or"
fmtBinOp AST.Op_and = "and"
fmtBinOp AST.Op_eq  = "=="
fmtBinOp AST.Op_ne  = "!="
fmtBinOp AST.Op_lt  = "<"
fmtBinOp AST.Op_le  = "<="
fmtBinOp AST.Op_gt  = ">"
fmtBinOp AST.Op_ge  = ">="
fmtBinOp AST.Op_add = "+"
fmtBinOp AST.Op_sub = "-"
fmtBinOp AST.Op_mul = "*"
fmtBinOp AST.Op_div = "//"

fmtUnOp :: AST.UnOp -> String
fmtUnOp AST.Op_not = "not"
fmtUnOp AST.Op_neg = "-"

fmtIdentifier :: AST.Ident -> String
fmtIdentifier ident = concatMap fmtIdentChar ident
  where
    fmtIdentChar '_'  = "__"
    fmtIdentChar '\'' = "_a"
    fmtIdentChar c    = [c]
