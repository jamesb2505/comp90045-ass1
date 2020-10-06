module RooOzCodeGen where 

import RooAST as AST
import RooSymbolTable as ST

import Control.Monad.State
import Control.Monad.Except
import Data.List (intercalate)
import Data.Maybe
  
-- LabelNum, RegNum, SlotNum
-- Type aliases for Ints, specifying the use of an Int argument
type LabelNum = Int
type RegNum = Int
type SlotNum = Int

-- Gen
-- Data type for a generator
data Gen = Gen LabelNum RegNum
  deriving (Show, Eq)

-- ExceptStateGen
-- ExceptT wrapped (Stage Gen)
-- Used to propogate errors through a successive generators
type ExceptStateGen = ExceptT String (State Gen)

-- Label
-- Data type for two types of labels, with custom Show instance
--  * ProcLabel, label of a procedure
--  * BranchLabel, label of a branch
data Label 
  = ProcLabel String
  | BranchLabel LabelNum
  deriving (Eq)

instance Show Label where
  show (ProcLabel l) = "proc_" ++ l
  show (BranchLabel l) = "label_" ++ show l

-- OzCode
-- Data type for Oz code, with custom Show instance
data OzCode
  = Oz_push_stack_frame Int
  | Oz_pop_stack_frame Int
  | Oz_store SlotNum RegNum
  | Oz_load RegNum SlotNum
  | Oz_load_address RegNum SlotNum
  | Oz_load_indirect RegNum RegNum
  | Oz_store_indirect RegNum RegNum
  | Oz_int_const RegNum Int
  | Oz_real_const RegNum Double
  | Oz_string_const RegNum String
  | Oz_add_int RegNum RegNum RegNum
  | Oz_add_real RegNum RegNum RegNum
  | Oz_add_offset RegNum RegNum RegNum
  | Oz_sub_int RegNum RegNum RegNum
  | Oz_sub_real RegNum RegNum RegNum
  | Oz_sub_offset RegNum RegNum RegNum
  | Oz_mul_int RegNum RegNum RegNum
  | Oz_mul_real RegNum RegNum RegNum
  | Oz_div_int RegNum RegNum RegNum
  | Oz_div_real RegNum RegNum RegNum
  | Oz_neg_int RegNum RegNum
  | Oz_neg_real RegNum RegNum
  | Oz_cmp_eq_int RegNum RegNum RegNum
  | Oz_cmp_ne_int RegNum RegNum RegNum
  | Oz_cmp_gt_int RegNum RegNum RegNum
  | Oz_cmp_ge_int RegNum RegNum RegNum
  | Oz_cmp_lt_int RegNum RegNum RegNum
  | Oz_cmp_le_int RegNum RegNum RegNum
  | Oz_cmp_eq_real RegNum RegNum RegNum
  | Oz_cmp_ne_real RegNum RegNum RegNum
  | Oz_cmp_gt_real RegNum RegNum RegNum
  | Oz_cmp_ge_real RegNum RegNum RegNum
  | Oz_cmp_lt_real RegNum RegNum RegNum
  | Oz_cmp_le_real RegNum RegNum RegNum
  | Oz_and RegNum RegNum RegNum
  | Oz_or RegNum RegNum RegNum
  | Oz_not RegNum RegNum
  | Oz_int_to_real RegNum RegNum
  | Oz_move RegNum RegNum
  | Oz_branch_on_true RegNum Label
  | Oz_branch_on_false RegNum Label
  | Oz_branch_uncond Label
  | Oz_label Label
  | Oz_call Label
  | Oz_call_builtin String
  | Oz_return
  | Oz_halt
  | Oz_debug_reg RegNum
  | Oz_debug_slot SlotNum
  | Oz_debug_stack
  deriving (Eq)

instance Show OzCode where
  show (Oz_push_stack_frame i)   
    = "  push_stack_frame " ++ show i
  show (Oz_pop_stack_frame i)    
    = "  pop_stack_frame " ++ show i
  show (Oz_store s r)            
    = "  store " ++ intercalate ", " [ show s, fmtReg r ]
  show (Oz_load r s)             
    = "  load " ++ intercalate ", " [ fmtReg r, show s ]
  show (Oz_load_address r s)     
    = "  load_address " ++ intercalate ", " [ fmtReg r, show s ]
  show (Oz_load_indirect rI rJ)  
    = "  load_indirect " ++ intercalate ", " (map fmtReg [ rI, rJ ])
  show (Oz_store_indirect rI rJ) 
    = "  store_indirect " ++ intercalate ", " (map fmtReg [ rI, rJ ])
  show (Oz_int_const r i)       
    = "  int_const " ++ intercalate ", " [ fmtReg r, show i ]
  show (Oz_real_const r d)      
    = "  real_const " ++ intercalate ", " [ fmtReg r, show d ]
  show (Oz_string_const r s)    
    = "  string_const " ++ fmtReg r ++ ", \"" ++ s ++ "\"" 
  show (Oz_add_int rI rJ rK)
    = "  add_int " ++ intercalate ", " (map fmtReg [ rI, rJ, rK ])
  show (Oz_add_real rI rJ rK)
    = "  add_real " ++ intercalate ", " (map fmtReg [ rI, rJ, rK ])
  show (Oz_add_offset rI rJ rK)
    = "  add_offset " ++ intercalate ", " (map fmtReg [ rI, rJ, rK ])
  show (Oz_sub_int rI rJ rK)
    = "  sub_int " ++ intercalate ", " (map fmtReg [ rI, rJ, rK ])
  show (Oz_sub_real rI rJ rK)
    = "  sub_real " ++ intercalate ", " (map fmtReg [ rI, rJ, rK ])
  show (Oz_sub_offset rI rJ rK)
    = "  sub_offset " ++ intercalate ", " (map fmtReg [ rI, rJ, rK ])
  show (Oz_mul_int rI rJ rK)
    = "  mul_int " ++ intercalate ", " (map fmtReg [ rI, rJ, rK ])
  show (Oz_mul_real rI rJ rK)
    = "  mul_real " ++ intercalate ", " (map fmtReg [ rI, rJ, rK ])
  show (Oz_div_int rI rJ rK)
    = "  div_int " ++ intercalate ", " (map fmtReg [ rI, rJ, rK ])
  show (Oz_div_real rI rJ rK)
    = "  div_real " ++ intercalate ", " (map fmtReg [ rI, rJ, rK ])
  show (Oz_neg_int rI rJ)
    = "  neg_int " ++ intercalate ", " (map fmtReg [ rI, rJ ])
  show (Oz_neg_real rI rJ)
    = "  neg_real " ++ intercalate ", " (map fmtReg [ rI, rJ ])
  show (Oz_cmp_eq_int rI rJ rK)
    = "  cmp_eq_int " ++ intercalate ", " (map fmtReg [ rI, rJ, rK ])
  show (Oz_cmp_ne_int rI rJ rK)
    = "  cmp_ne_int " ++ intercalate ", " (map fmtReg [ rI, rJ, rK ])
  show (Oz_cmp_gt_int rI rJ rK)
    = "  cmp_gt_int " ++ intercalate ", " (map fmtReg [ rI, rJ, rK ])
  show (Oz_cmp_ge_int rI rJ rK)
    = "  cmp_ge_int " ++ intercalate ", " (map fmtReg [ rI, rJ, rK ])
  show (Oz_cmp_lt_int rI rJ rK)
    = "  cmp_lt_int " ++ intercalate ", " (map fmtReg [ rI, rJ, rK ])
  show (Oz_cmp_le_int rI rJ rK)
    = "  cmp_le_int " ++ intercalate ", " (map fmtReg [ rI, rJ, rK ])
  show (Oz_cmp_eq_real rI rJ rK)
    = "  cmp_eq_real " ++ intercalate ", " (map fmtReg [ rI, rJ, rK ])
  show (Oz_cmp_ne_real rI rJ rK)
    = "  cmp_ne_real " ++ intercalate ", " (map fmtReg [ rI, rJ, rK ])
  show (Oz_cmp_gt_real rI rJ rK)
    = "  cmp_gt_real " ++ intercalate ", " (map fmtReg [ rI, rJ, rK ])
  show (Oz_cmp_ge_real rI rJ rK)
    = "  cmp_ge_real " ++ intercalate ", " (map fmtReg [ rI, rJ, rK ])
  show (Oz_cmp_lt_real rI rJ rK)
    = "  cmp_lt_real " ++ intercalate ", " (map fmtReg [ rI, rJ, rK ])
  show (Oz_cmp_le_real rI rJ rK)
    = "  cmp_le_real " ++ intercalate ", " (map fmtReg [ rI, rJ, rK ])
  show (Oz_and rI rJ rK)
    = "  and " ++ intercalate ", " (map fmtReg [ rI, rJ, rK ])
  show (Oz_or rI rJ rK)
    = "  or " ++ intercalate ", " (map fmtReg [ rI, rJ, rK ])
  show (Oz_not rI rJ)            
    = "  not " ++ intercalate ", " (map fmtReg [ rI, rJ ])
  show (Oz_int_to_real rI rJ)    
    = "  int_to_real " ++ intercalate ", " (map fmtReg [ rI, rJ ])
  show (Oz_move rI rJ)           
    = "  move " ++ intercalate ", " (map fmtReg [ rI, rJ ])
  show (Oz_branch_on_true r l)  
    = "  branch_on_true " ++ intercalate ", " [ fmtReg r, show l ]
  show (Oz_branch_on_false r l) 
    = "  branch_on_false " ++ intercalate ", " [ fmtReg r, show l ]
  show (Oz_branch_uncond l)      
    = "  branch_uncond " ++ show l
  show (Oz_label l)               
    = show l ++ ":"
  show (Oz_call l)               
    = "  call " ++ show l
  show (Oz_call_builtin s)       
    = "  call_builtin " ++ s
  show (Oz_return)               
    = "  return"
  show (Oz_halt)                 
    = "halt"
  show (Oz_debug_reg r)         
    = "  debug_reg " ++ fmtReg r
  show (Oz_debug_slot s)         
    = "  debug_slot " ++ show s
  show (Oz_debug_stack)          
    = "  debug_stack"

-- fmtReg
-- Formates a RegNum
fmtReg :: RegNum -> String
fmtReg n = "r" ++ show n

-- printOzCodes
-- Prints a given [OzCode] with appropiate formatting
printOzCodes :: [OzCode] -> IO ()
printOzCodes code = putStr . unlines $ map show code

-- nextLabel
-- Returns the next Label for a generator, post-incrementing the counter
nextLabel :: ExceptStateGen Label
nextLabel =
  do
    Gen ln rn <- lift get
    lift . put $ Gen (ln + 1) rn
    return $ BranchLabel ln

-- getRegister
-- Returns the current RegNum for a generator
getRegister :: ExceptStateGen RegNum
getRegister = 
  do
    Gen _ rn <- lift get
    return rn

-- nextRegister
-- Returns the next RegNum for a generator, post-incrementing the counter
nextRegister :: ExceptStateGen RegNum
nextRegister = 
  do
    Gen ln rn <- lift get
    lift . put . Gen ln $ rn + 1
    return rn

-- putRegister 
-- Sets the RegNum inside the State
putRegister :: RegNum -> ExceptStateGen ()
putRegister r =
  do
    Gen ln _ <- lift get
    lift . put $ Gen ln r

-- maybeErr
-- Lifts a Maybe a into a ExceptStateGen, using the input String for the 
-- Left value on Nothing
maybeErr :: String -> Maybe a -> ExceptStateGen a
maybeErr a Nothing  = liftEither $ Left a
maybeErr _ (Just b) = liftEither $ Right b

-- initGenState
-- Initial State for a generator
initGenState :: Gen
initGenState = Gen 0 0

-- runCodeGen
-- Generates a list of OzCode of a given AST.Program
-- Right on success, Left on error
runCodeGen :: AST.Program -> ST.SymbolTable -> Either String [OzCode]
runCodeGen prog st = evalState (runExceptT $ genProgram st prog) initGenState

-- repeatGen
-- Repeats a generator over a list, concatenating the results
repeatGen :: (a -> ExceptStateGen [OzCode]) -> [a] -> ExceptStateGen [OzCode]
repeatGen gen = liftM concat . mapM gen
    
-- genProgram
-- Generates a [OzCode] for a given AST.Program
genProgram :: ST.SymbolTable -> AST.Program -> ExceptStateGen [OzCode]
genProgram st (AST.Program _ _ ps) =
  do 
    procs <- repeatGen (genProcedure st) ps
    return $ [ Oz_call $ ProcLabel "main" 
             , Oz_halt 
             ]
          ++ procs

-- genProcedure
-- Generates a [OzCode] for a given AST.Program
genProcedure :: ST.SymbolTable -> AST.Procedure -> ExceptStateGen [OzCode]
genProcedure st@(ST.SymbolTable _ _ ps) (AST.Procedure name _ _ ss) = 
  do 
    proc@(ST.Procedure params vars stackSize) 
      <- maybeErr ("Unknown procedure `" ++ name ++ "`") 
              $ lookup name ps
    let nParams = length params
    let pCode = [ Oz_store i i | i <- [0..nParams - 1] ]
    stmts <- repeatGen (genStmt $ st { unProcedures = (name, proc):ps }) ss
    if stackSize > 0
    then return $ Oz_label (ProcLabel name)
                : Oz_push_stack_frame stackSize
                : pCode
              ++ [ Oz_int_const 0 0 ]
              ++ [ Oz_store (i + nParams) 0 | i <- [0..stackSize - 1] ]
              ++ stmts 
              ++ [ Oz_pop_stack_frame stackSize
                 , Oz_return
                 ]
    else return $ Oz_label (ProcLabel name) 
                : pCode
              ++ stmts 
              ++ [ Oz_return ]

-- genStmt
-- Generates a [OzCode] for a given AST.Stmt
-- It is assumed that the current procedure is at the top of the 
-- procedures of the ST.SymbolTable
genStmt :: ST.SymbolTable -> AST.Stmt -> ExceptStateGen [OzCode]
genStmt st (AST.Assign (AST.LId lAlias) (AST.LVal _ (AST.LId rAlias)))
  | ST.isRef st lAlias && ST.isRef st rAlias
  = do 
      lOffset <- maybeErr ("Unknown parameter `" ++ lAlias ++ "`")
                 $ getLocalOffset st lAlias
      rOffset <- maybeErr ("Unknown parameter `" ++ rAlias ++ "`")
                 $ getLocalOffset st rAlias
      return $ [ Oz_load 0 rOffset
               , Oz_store lOffset 0
               ]
genStmt st (AST.Assign l e) =
  do 
    putRegister 0
    eCode <- genExpr st e
    putRegister 1
    lCode <- genLValue st l
    return $ eCode 
          ++ lCode
          ++ [ Oz_store_indirect 1 0 ]
genStmt st (AST.Read l) =
  do 
    putRegister 1
    lCode <- genLValue st l
    reader <- getReadBuiltin $ getLValT st l
    return $ Oz_call_builtin reader
           : lCode
          ++ [ Oz_store_indirect 1 0 ]
genStmt st (AST.Write e) =
  do 
    putRegister 0
    expr <- genExpr st e
    printer <- getPrintBuiltin $ AST.getExprType e
    return $ expr 
          ++ [ Oz_call_builtin printer ]
genStmt st (AST.Writeln e) =
  do 
    putRegister 0
    expr <- genExpr st e
    printer <- getPrintBuiltin $ AST.getExprType e
    return $ expr
          ++ [ Oz_call_builtin printer 
             , Oz_call_builtin "print_newline"
             ]
genStmt st (AST.If e ss) =
  do 
    putRegister 0
    eCode <- genExpr st e
    putRegister 0
    ssCode <- genStmts st ss
    endLabel <- nextLabel
    return $ eCode
          ++ [ Oz_branch_on_false 0 endLabel ]
          ++ ssCode
          ++ [ Oz_label endLabel ]
genStmt st (AST.IfElse e ts fs) =
  do 
    putRegister 0
    eCode <- genExpr st e
    tsCode <- genStmts st ts
    fsCode <- genStmts st fs
    falseLabel <- nextLabel
    endLabel <- nextLabel
    return $ eCode
          ++ [ Oz_branch_on_false 0 falseLabel ]
          ++ tsCode
          ++ [ Oz_branch_uncond endLabel ]
          ++ fsCode
          ++ [ Oz_label endLabel ]
genStmt st (AST.While e ss) =
  do 
    putRegister 0
    eCode <- genExpr st e
    putRegister 0
    ssCode <- genStmts st ss
    startLabel <- nextLabel
    endLabel <- nextLabel
    return $ Oz_label startLabel
           : eCode
          ++ [ Oz_branch_on_false 0 endLabel ]
          ++ ssCode
          ++ [ Oz_branch_uncond startLabel
             , Oz_label endLabel
             ]
genStmt st (AST.Call _ _) = -- TODO
  do 
    putRegister 0
    return []

-- genStmts
-- Generates a [OzCode] for a given [AST.Program]
genStmts :: ST.SymbolTable -> [AST.Stmt] -> ExceptStateGen [OzCode]
genStmts st ss = repeatGen (genStmt st) ss

-- genLValue
-- Generates a [OzCode] for a given AST.LValue
-- It is assumed that the current procedure is at the top of the 
-- procedures of the ST.SymbolTable
genLValue :: ST.SymbolTable -> AST.LValue -> ExceptStateGen [OzCode]
genLValue st (AST.LId alias) = 
  do 
    offset <- maybeErr ("Unknown parameter/variable `" ++ alias ++ "`")
              $ getLocalOffset st alias
    r <- nextRegister
    if ST.isRef st alias 
    then return $ [ Oz_load r offset ]
    else return $ [ Oz_load_address r offset ]
genLValue st (AST.LField alias field) = 
  do 
    aOffset <- maybeErr ("Unknown parameter/variable `" ++ alias ++ "`")
               $ getLocalOffset st alias
    let fOffset = unFOffset $ getField (ST.getRecord st alias) field
    r <- nextRegister
    if ST.isRef st alias 
    then let r' = r + 1 in
         return $ [ Oz_load r aOffset
                  , Oz_int_const r' fOffset
                  , Oz_sub_offset r r r'
                  ]
    else return $ [ Oz_load_address r (aOffset - fOffset) ]
genLValue st (AST.LInd alias e) = -- TODO: fix records
  do 
    offset <- maybeErr ("Unknown parameter/variable `" ++ alias ++ "`")
              $ getLocalOffset st alias
    r <- getRegister
    eCode <- genExpr st e
    let r' = r + 1
    return $ eCode
          ++ [ Oz_load_address r' offset
             , Oz_sub_offset r r' r
             ]
genLValue st (AST.LIndField alias e field) = -- TODO
  do 
    putRegister 0
    eCode <- genExpr st e
    return $ []
    
-- genExpr
-- Generates a [OzCode] for a given AST.Expr
-- It is assumed that the current procedure is at the top of the 
-- procedures of the ST.SymbolTable
genExpr :: ST.SymbolTable -> AST.Expr -> ExceptStateGen [OzCode]
genExpr st (AST.LVal _ lval) = 
  do 
    lCode <- genLValue st lval
    return $ lCode
genExpr _ (AST.BoolConst _ b) =
  do 
    r <- nextRegister
    return $ [ Oz_int_const r $ if b then 1 else 0 ]
genExpr _ (AST.IntConst _ i) =
  do 
    r <- nextRegister
    return $ [ Oz_int_const r $ fromInteger i ]
genExpr _ (AST.StrConst _ s) =
  do 
    r <- nextRegister
    return $ [ Oz_string_const r s ]
genExpr st (AST.BinOpExpr _ op a b) =
  do 
    r <- getRegister
    aCode <- genExpr st a
    bCode <- genExpr st b
    putRegister $ r + 1
    r' <- getRegister
    return $ aCode
          ++ bCode
          ++ [ getBinOpCode op r r r' ]
genExpr st (AST.UnOpExpr _ op a) =
  do 
    r <- getRegister
    aCode <- genExpr st a
    putRegister $ r + 1
    return $ aCode
          ++ [ getUnOpCode op r r ]

getBuiltinSuffix :: AST.ExprType -> Either String String
getBuiltinSuffix AST.BoolT = Right "bool"
getBuiltinSuffix AST.IntT  = Right "int"
getBuiltinSuffix AST.StrT  = Right "string"
getBuiltinSuffix t         = Left $ "no builtin for " ++ show t

getPrintBuiltin :: AST.ExprType -> ExceptStateGen String
getPrintBuiltin t = liftEither $ ("print_" ++) <$> getBuiltinSuffix t

getReadBuiltin :: AST.ExprType -> ExceptStateGen String
getReadBuiltin t = liftEither $ ("read_" ++) <$> getBuiltinSuffix t

getBinOpCode :: AST.BinOp -> (RegNum -> RegNum -> RegNum -> OzCode)
getBinOpCode Op_or  = Oz_or
getBinOpCode Op_and = Oz_and
getBinOpCode Op_eq  = Oz_cmp_eq_int
getBinOpCode Op_neq = Oz_cmp_ne_int
getBinOpCode Op_lt  = Oz_cmp_lt_int
getBinOpCode Op_leq = Oz_cmp_le_int
getBinOpCode Op_gt  = Oz_cmp_gt_int
getBinOpCode Op_geq = Oz_cmp_ge_int
getBinOpCode Op_add = Oz_add_int
getBinOpCode Op_sub = Oz_sub_int
getBinOpCode Op_mul = Oz_mul_int
getBinOpCode Op_div = Oz_div_int

getUnOpCode :: AST.UnOp -> (RegNum -> RegNum -> OzCode)
getUnOpCode Op_not = Oz_not
getUnOpCode Op_neg = Oz_neg_int

getLValT :: ST.SymbolTable -> AST.LValue -> AST.ExprType -- TODO
getLValT st (LId alias) = ST.getProcType st alias
getLValT st@(ST.SymbolTable _ as _) (LInd alias _)
  | AST.isArrayT aliasType
    = ST.getArrayType aliasType
  where aliasType = ST.getProcType st alias
getLValT st@(ST.SymbolTable rs _ _) (LField alias field)
  | AST.isRecordT aliasType
    = ST.getFieldType rs aliasType field
  where aliasType = ST.getProcType st alias
getLValT st@(ST.SymbolTable rs _ _) (LIndField alias _ field) 
  | AST.isArrayT aliasType && AST.isRecordT recordType
    = ST.getFieldType rs recordType field
  where 
    aliasType = ST.getProcType st alias
    recordType = ST.getArrayType aliasType
getLValT _ _ = AST.ErrorT

p = "record { integer field } rec; procedure main () { writeln \"Hello, World!\"; } procedure r () rec r; { writeln rec.field; }"
p2 = "array [1] integer arr; procedure main () { writeln \"Hello, World!\"; } procedure r () arr a; { writeln a[0]; }"
ps = (AST.Program [] [] [AST.Procedure "main" [] [] [AST.Writeln (AST.StrConst AST.StrT "Hello, World!")]],ST.SymbolTable {ST.unRecords = [], ST.unArrays = [], ST.unProcedures = [("main",ST.Procedure {ST.unParams = [], ST.unVars = [], ST.unStackSize = 0})]})
pr = AST.Program [] [] [ AST.Procedure "main" [] [] [AST.Writeln (AST.BinOpExpr AST.IntT AST.Op_add (AST.IntConst IntT 0) (AST.IntConst IntT 0))]
                       , AST.Procedure "b" [] [] [ AST.Writeln (AST.BinOpExpr AST.BoolT AST.Op_and (AST.UnOpExpr AST.BoolT AST.Op_not (AST.BoolConst BoolT True)) (AST.BoolConst AST.BoolT False))
                                                 , AST.While (AST.BoolConst AST.BoolT False) [AST.Writeln (AST.StrConst AST.StrT "Hello, World!")]
                                                 ] -- incomplete
                       ]
st = ST.SymbolTable {ST.unRecords = [], ST.unArrays = [], ST.unProcedures = [ ("main",ST.Procedure {ST.unParams = [], ST.unVars = [], ST.unStackSize = 0})
                                                                            , ("b",ST.Procedure {ST.unParams = [], ST.unVars = [], ST.unStackSize = 1})
                                                                            ]}
pr2 = AST.Program [] [AST.Array 1 (AST.Atomic AST.IntType) "arr"] [AST.Procedure "main" [] [] [AST.Writeln (AST.StrConst AST.StrT "Hello, World!")],AST.Procedure "r" [] [AST.Var (AST.Alias "arr") ["a"]] [AST.Read (AST.LInd "a" (AST.IntConst AST.IntT 0))]]
st2 = ST.SymbolTable {ST.unRecords = [], ST.unArrays = [("arr",ST.Array {unAType = AST.Atomic AST.IntType, ST.unSize = 1})], ST.unProcedures = [("main",ST.Procedure {ST.unParams = [], ST.unVars = [], ST.unStackSize = 0}),("r",ST.Procedure {ST.unParams = [], ST.unVars = [("a",ST.Var {ST.unVType = AST.Alias "arr", ST.unVOffset = 0})], unStackSize = 1})]}
s = [AST.Writeln (AST.StrConst AST.StrT "Hello, World!"), AST.Writeln (AST.StrConst AST.StrT "Hello, World!")]

