module RooOzCodeGen where 

import RooAST as AST
import RooSymbolTable as ST

import Control.Monad.State
import Data.List (intercalate)
import Data.Maybe

type GenState = State Gen

data Gen = Gen LabelNum RegNum
  deriving (Show, Eq)
  
type SlotNum = Int
type LabelNum = Int
data RegNum = Reg Int
  deriving (Eq)

instance Show RegNum where
  show (Reg i) = "r" ++ show i 

data Label 
  = ProcLabel String
  | BranchLabel LabelNum
  deriving (Eq)

instance (Show Label) where
  show (ProcLabel l) = "proc_" ++ l
  show (BranchLabel l) = "label_" ++ show l

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
    = "  store " ++ intercalate ", " [ show s, show r ]
  show (Oz_load r s)             
    = "  load " ++ intercalate ", " [ show r, show s ]
  show (Oz_load_address r s)     
    = "  load_address " ++ intercalate ", " [ show r, show s ]
  show (Oz_load_indirect rI rJ)  
    = "  load_indirect " ++ intercalate ", " (map show [ rI, rJ ])
  show (Oz_store_indirect rI rJ) 
    = "  store_indirect " ++ intercalate ", " (map show [ rI, rJ ])
  show (Oz_int_const r i)       
    = "  int_const " ++ intercalate ", " [ show r, show i ]
  show (Oz_real_const r d)      
    = "  real_const " ++ intercalate ", " [ show r, show d ]
  show (Oz_string_const r s)    
    = "  string_const " ++ show r ++ ", \"" ++ s ++ "\"" 
  show (Oz_add_int rI rJ rK)
    = "  add_int " ++ intercalate ", " (map show [ rI, rJ, rK ])
  show (Oz_add_real rI rJ rK)
    = "  add_real " ++ intercalate ", " (map show [ rI, rJ, rK ])
  show (Oz_add_offset rI rJ rK)
    = "  add_offset " ++ intercalate ", " (map show [ rI, rJ, rK ])
  show (Oz_sub_int rI rJ rK)
    = "  sub_int " ++ intercalate ", " (map show [ rI, rJ, rK ])
  show (Oz_sub_real rI rJ rK)
    = "  sub_real " ++ intercalate ", " (map show [ rI, rJ, rK ])
  show (Oz_sub_offset rI rJ rK)
    = "  sub_offset " ++ intercalate ", " (map show [ rI, rJ, rK ])
  show (Oz_mul_int rI rJ rK)
    = "  mul_int " ++ intercalate ", " (map show [ rI, rJ, rK ])
  show (Oz_mul_real rI rJ rK)
    = "  mul_real " ++ intercalate ", " (map show [ rI, rJ, rK ])
  show (Oz_div_int rI rJ rK)
    = "  div_int " ++ intercalate ", " (map show [ rI, rJ, rK ])
  show (Oz_div_real rI rJ rK)
    = "  div_real " ++ intercalate ", " (map show [ rI, rJ, rK ])
  show (Oz_neg_int rI rJ)
    = "  neg_int " ++ intercalate ", " (map show [ rI, rJ ])
  show (Oz_neg_real rI rJ)
    = "  neg_real " ++ intercalate ", " (map show [ rI, rJ ])
  show (Oz_cmp_eq_int rI rJ rK)
    = "  cmp_eq_int " ++ intercalate ", " (map show [ rI, rJ, rK ])
  show (Oz_cmp_ne_int rI rJ rK)
    = "  cmp_ne_int " ++ intercalate ", " (map show [ rI, rJ, rK ])
  show (Oz_cmp_gt_int rI rJ rK)
    = "  cmp_gt_int " ++ intercalate ", " (map show [ rI, rJ, rK ])
  show (Oz_cmp_ge_int rI rJ rK)
    = "  cmp_ge_int " ++ intercalate ", " (map show [ rI, rJ, rK ])
  show (Oz_cmp_lt_int rI rJ rK)
    = "  cmp_lt_int " ++ intercalate ", " (map show [ rI, rJ, rK ])
  show (Oz_cmp_le_int rI rJ rK)
    = "  cmp_le_int " ++ intercalate ", " (map show [ rI, rJ, rK ])
  show (Oz_cmp_eq_real rI rJ rK)
    = "  cmp_eq_real " ++ intercalate ", " (map show [ rI, rJ, rK ])
  show (Oz_cmp_ne_real rI rJ rK)
    = "  cmp_ne_real " ++ intercalate ", " (map show [ rI, rJ, rK ])
  show (Oz_cmp_gt_real rI rJ rK)
    = "  cmp_gt_real " ++ intercalate ", " (map show [ rI, rJ, rK ])
  show (Oz_cmp_ge_real rI rJ rK)
    = "  cmp_ge_real " ++ intercalate ", " (map show [ rI, rJ, rK ])
  show (Oz_cmp_lt_real rI rJ rK)
    = "  cmp_lt_real " ++ intercalate ", " (map show [ rI, rJ, rK ])
  show (Oz_cmp_le_real rI rJ rK)
    = "  cmp_le_real " ++ intercalate ", " (map show [ rI, rJ, rK ])
  show (Oz_and rI rJ rK)
    = "  and " ++ intercalate ", " (map show [ rI, rJ, rK ])
  show (Oz_or rI rJ rK)
    = "  or " ++ intercalate ", " (map show [ rI, rJ, rK ])
  show (Oz_not rI rJ)            
    = "  not " ++ intercalate ", " (map show [ rI, rJ ])
  show (Oz_int_to_real rI rJ)    
    = "  int_to_real " ++ intercalate ", " (map show [ rI, rJ ])
  show (Oz_move rI rJ)           
    = "  move " ++ intercalate ", " (map show [ rI, rJ ])
  show (Oz_branch_on_true r l)  
    = "  branch_on_true " ++ intercalate ", " [ show r, show l ]
  show (Oz_branch_on_false r l) 
    = "  branch_on_false " ++ intercalate ", " [ show r, show l ]
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
    = "  debug_reg " ++ show r
  show (Oz_debug_slot s)         
    = "  debug_slot " ++ show s
  show (Oz_debug_stack)          
    = "  debug_stack"

nextLabel :: GenState Label
nextLabel =
  do
    Gen ln rn <- get
    put $ Gen (ln + 1) rn
    return $ BranchLabel ln

getRegister :: GenState RegNum
getRegister = 
  do
    Gen ln r@(Reg rn) <- get
    return r

nextRegister :: GenState RegNum
nextRegister = 
  do
    Gen ln r@(Reg rn) <- get
    put . Gen ln . Reg $ rn + 1
    return r

prevRegister :: GenState RegNum
prevRegister = 
  do
    Gen ln (Reg rn) <- get
    let r = Reg $ rn - 1 
    put $ Gen ln r
    return r

putRegister :: Int -> GenState ()
putRegister r =
  do
    Gen ln _ <- get
    put . Gen ln $ Reg r

initState :: Gen
initState = Gen 0 $ Reg 0

runCodeGen :: AST.Program -> ST.SymbolTable -> Either String [OzCode]
runCodeGen prog st =
  do 
    case evalState (genProg st prog) initState of
      code -> return code
      
repeatGen :: (a -> GenState [OzCode]) -> [a] -> GenState [OzCode]
repeatGen gen = liftM concat . mapM gen
    
genProg :: ST.SymbolTable -> AST.Program -> GenState [OzCode]
genProg st (AST.Program _ _ ps) =
  do 
    procs <- repeatGen (genProc st) ps
    return $ [ Oz_call (ProcLabel "main"), Oz_halt ] ++ procs

genProc :: ST.SymbolTable -> AST.Procedure -> GenState [OzCode]
genProc st@(ST.SymbolTable _ _ ps) (AST.Procedure name _ _ ss) = 
  do 
    let st'@(ST.Procedure params vars stackSize) = fromJust $ lookup name ps
    let nParams = length params
    let pCode = [ Oz_store i (Reg i) | i <- [0..nParams - 1] ]
    stmts <- repeatGen (genStmt $ st { unProcedures = [ (name, st') ] }) ss
    if stackSize > 0
    then return $ Oz_label (ProcLabel name)
                : Oz_push_stack_frame stackSize
                : pCode
               ++ [ Oz_int_const (Reg 0) 0 ]
               ++ [ Oz_store (i + nParams) $ Reg 0 | i <- [0..stackSize - 1] ]
               ++ stmts 
               ++ [ Oz_pop_stack_frame stackSize
                  , Oz_return
                  ]
    else return $ Oz_label (ProcLabel name) 
                : pCode
               ++ stmts 
               ++ [ Oz_return ]

genStmt :: ST.SymbolTable -> AST.Stmt -> GenState [OzCode]
genStmt st (AST.Assign l e) =
  do 
    putRegister 0
    eCode <- genExpr st e
    putRegister 1
    lCode <- genLValue st l
    return $ eCode 
          ++ lCode
          ++ [ Oz_store_indirect (Reg 1) (Reg 0) ]
genStmt st (AST.Read l) = -- TODO
  do 
    putRegister 1
    lCode <- genLValue st l
    return $ Oz_call_builtin (getReadBuiltin 
                              $ getLValT (snd . head $ ST.unProcedures st) l)
           : lCode
          ++ [ Oz_store_indirect (Reg 1) (Reg 0) ]
genStmt st (AST.Write e) =
  do 
    putRegister 0
    expr <- genExpr st e
    return $ expr 
          ++ [ Oz_call_builtin . getPrintBuiltin $ AST.getExprT e ]
genStmt st (AST.Writeln e) =
  do 
    putRegister 0
    expr <- genExpr st e
    return $ expr
          ++ [ Oz_call_builtin . getPrintBuiltin $ AST.getExprT e 
             , Oz_call_builtin "print_newline"
             ]
genStmt st (AST.If e ss) =
  do 
    putRegister 0
    eCode <- genExpr st e
    putRegister 0
    ssCode <- repeatGen (genStmt st) ss
    endLabel <- nextLabel
    return $ eCode
          ++ [ Oz_branch_on_false (Reg 0) endLabel ]
          ++ ssCode
          ++ [ Oz_label endLabel ]
genStmt st (AST.IfElse e ts fs) =
  do 
    putRegister 0
    eCode <- genExpr st e
    putRegister 0
    tsCode <- repeatGen (genStmt st) ts
    putRegister 0
    fsCode <- repeatGen (genStmt st) fs
    falseLabel <- nextLabel
    endLabel <- nextLabel
    return $ eCode
          ++ [ Oz_branch_on_false (Reg 0) falseLabel ]
          ++ tsCode
          ++ [ Oz_branch_uncond endLabel ]
          ++ fsCode
          ++ [ Oz_label endLabel ]
genStmt st (AST.While e ss) =
  do 
    putRegister 0
    eCode <- genExpr st e
    putRegister 0
    ssCode <- repeatGen (genStmt st) ss
    startLabel <- nextLabel
    endLabel <- nextLabel
    return $ Oz_label startLabel
           : eCode
          ++ [ Oz_branch_on_false (Reg 0) endLabel ]
          ++ ssCode
          ++ [ Oz_branch_uncond startLabel
             , Oz_label endLabel
             ]
genStmt st (AST.Call _ _) = -- TODO
  do 
    putRegister 0
    return []

genLValue :: ST.SymbolTable -> AST.LValue -> GenState [OzCode]
genLValue _ _ = undefined -- TODO

genExpr :: ST.SymbolTable -> AST.Expr -> GenState [OzCode]
genExpr _ (AST.LVal _ _) = -- TODO
  do 
    return []
genExpr _ (AST.BoolConst _ b) =
  do 
    r <- nextRegister
    return [ Oz_int_const r $ if b then 1 else 0 ]
genExpr _ (AST.IntConst _ i) =
  do 
    r <- nextRegister
    return [ Oz_int_const r (fromInteger i) ]
genExpr _ (AST.StrConst _ s) =
  do 
    r <- nextRegister
    return [ Oz_string_const r s ]
genExpr st (AST.BinOpExpr _ op a b) =
  do 
    r@(Reg n) <- getRegister
    aCode <- genExpr st a
    bCode <- genExpr st b
    putRegister (n + 1)
    r' <- getRegister
    return $ aCode
          ++ bCode
          ++ [ getBinOpCode op r r r' ]
genExpr st (AST.UnOpExpr _ op a) =
  do 
    r@(Reg n) <- getRegister
    aCode <- genExpr st a
    putRegister (n + 1)
    return $ aCode
          ++ [ getUnOpCode op r r ]

getBuiltinSuffix :: AST.ExprType -> String
getBuiltinSuffix AST.BoolT = "print_bool"
getBuiltinSuffix AST.IntT  = "print_int"
getBuiltinSuffix AST.StrT  = "print_string"
getBuiltinSuffix t         = error $ "no such builtin for " ++ show t

getPrintBuiltin :: AST.ExprType -> String
getPrintBuiltin t = "print_" ++ getBuiltinSuffix t

getReadBuiltin :: AST.ExprType -> String
getReadBuiltin t = "read_" ++ getBuiltinSuffix t

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

getLValT :: ST.Procedure -> AST.LValue -> AST.ExprType
getLValT = undefined

p = "procedure main () { writeln \"Hello, World!\"; }"
ps = (AST.Program [] [] [AST.Procedure "main" [] [] [AST.Writeln (AST.StrConst AST.StrT "Hello, World!")]],ST.SymbolTable {ST.unRecords = [], ST.unArrays = [], ST.unProcedures = [("main",ST.Procedure {ST.unParams = [], ST.unVars = [], ST.unStackSize = 0})]})
pr = AST.Program [] [] [ AST.Procedure "main" [] [] [AST.Writeln (AST.BinOpExpr AST.IntT AST.Op_add (AST.IntConst IntT 0) (AST.IntConst IntT 0))]
                       , AST.Procedure "b" [] [] [ AST.Writeln (AST.BinOpExpr AST.BoolT AST.Op_and (AST.UnOpExpr AST.BoolT AST.Op_not (AST.BoolConst BoolT True)) (AST.BoolConst AST.BoolT False))
                                                 , AST.While (AST.BoolConst AST.BoolT False) [AST.Writeln (AST.StrConst AST.StrT "Hello, World!")]
                                                 ] -- incomplete
                       ]
st = ST.SymbolTable {ST.unRecords = [], ST.unArrays = [], ST.unProcedures = [ ("main",ST.Procedure {ST.unParams = [], ST.unVars = [], ST.unStackSize = 0})
                                                                            , ("b",ST.Procedure {ST.unParams = [], ST.unVars = [], ST.unStackSize = 1})
                                                                            ]}
s = [AST.Writeln (AST.StrConst AST.StrT "Hello, World!"), AST.Writeln (AST.StrConst AST.StrT "Hello, World!")]
printCode :: Either String [OzCode] -> IO ()
printCode (Right code) = putStr . unlines $ map show code
