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
  | Oz_neg_int RegNum RegNum RegNum
  | Oz_neg_real RegNum RegNum RegNum
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
    = "  store " ++ intercalate ", " [show r, show s]
  show (Oz_load r s)             
    = "  load " ++ intercalate ", " [show r, show s]
  show (Oz_load_address r s)     
    = "  load_address " ++ intercalate ", " [show r, show s]
  show (Oz_load_indirect rI rJ)  
    = "  load_indirect " ++ intercalate ", " (map show [rI, rJ])
  show (Oz_store_indirect rI rJ) 
    = "  store_indirect " ++ intercalate ", " (map show [rI, rJ])
  show (Oz_int_const r i)       
    = "  int_const " ++ intercalate ", " [show r, show i]
  show (Oz_real_const r d)      
    = "  real_const " ++ intercalate ", " [show r, show d]
  show (Oz_string_const r s)    
    = "  string_const " ++ show r ++ ", \"" ++ s ++ "\"" 
  show (Oz_add_int rI rJ rK)
    = "  add_int " ++ intercalate ", " (map show [rI, rJ, rK])
  show (Oz_add_real rI rJ rK)
    = "  add_real " ++ intercalate ", " (map show [rI, rJ, rK])
  show (Oz_add_offset rI rJ rK)
    = "  add_offset " ++ intercalate ", " (map show [rI, rJ, rK])
  show (Oz_sub_int rI rJ rK)
    = "  sub_int " ++ intercalate ", " (map show [rI, rJ, rK])
  show (Oz_sub_real rI rJ rK)
    = "  sub_real " ++ intercalate ", " (map show [rI, rJ, rK])
  show (Oz_sub_offset rI rJ rK)
    = "  sub_offset " ++ intercalate ", " (map show [rI, rJ, rK])
  show (Oz_mul_int rI rJ rK)
    = "  mul_int " ++ intercalate ", " (map show [rI, rJ, rK])
  show (Oz_mul_real rI rJ rK)
    = "  mul_real " ++ intercalate ", " (map show [rI, rJ, rK])
  show (Oz_div_int rI rJ rK)
    = "  div_int " ++ intercalate ", " (map show [rI, rJ, rK])
  show (Oz_div_real rI rJ rK)
    = "  div_real " ++ intercalate ", " (map show [rI, rJ, rK])
  show (Oz_neg_int rI rJ rK)
    = "  neg_int " ++ intercalate ", " (map show [rI, rJ, rK])
  show (Oz_neg_real rI rJ rK)
    = "  neg_real " ++ intercalate ", " (map show [rI, rJ, rK])
  show (Oz_cmp_eq_int rI rJ rK)
    = "  cmp_eq_int " ++ intercalate ", " (map show [rI, rJ, rK])
  show (Oz_cmp_ne_int rI rJ rK)
    = "  cmp_ne_int " ++ intercalate ", " (map show [rI, rJ, rK])
  show (Oz_cmp_gt_int rI rJ rK)
    = "  cmp_gt_int " ++ intercalate ", " (map show [rI, rJ, rK])
  show (Oz_cmp_ge_int rI rJ rK)
    = "  cmp_ge_int " ++ intercalate ", " (map show [rI, rJ, rK])
  show (Oz_cmp_lt_int rI rJ rK)
    = "  cmp_lt_int " ++ intercalate ", " (map show [rI, rJ, rK])
  show (Oz_cmp_le_int rI rJ rK)
    = "  cmp_le_int " ++ intercalate ", " (map show [rI, rJ, rK])
  show (Oz_cmp_eq_real rI rJ rK)
    = "  cmp_eq_real " ++ intercalate ", " (map show [rI, rJ, rK])
  show (Oz_cmp_ne_real rI rJ rK)
    = "  cmp_ne_real " ++ intercalate ", " (map show [rI, rJ, rK])
  show (Oz_cmp_gt_real rI rJ rK)
    = "  cmp_gt_real " ++ intercalate ", " (map show [rI, rJ, rK])
  show (Oz_cmp_ge_real rI rJ rK)
    = "  cmp_ge_real " ++ intercalate ", " (map show [rI, rJ, rK])
  show (Oz_cmp_lt_real rI rJ rK)
    = "  cmp_lt_real " ++ intercalate ", " (map show [rI, rJ, rK])
  show (Oz_cmp_le_real rI rJ rK)
    = "  cmp_le_real " ++ intercalate ", " (map show [rI, rJ, rK])
  show (Oz_and rI rJ rK)
    = "  and " ++ intercalate ", " (map show [rI, rJ, rK])
  show (Oz_or rI rJ rK)
    = "  or " ++ intercalate ", " (map show [rI, rJ, rK])
  show (Oz_not rI rJ)            
    = "  not " ++ intercalate ", " (map show [rI, rJ])
  show (Oz_int_to_real rI rJ)    
    = "  int_to_real " ++ intercalate ", " (map show [rI, rJ])
  show (Oz_move rI rJ)           
    = "  move " ++ intercalate ", " (map show [rI, rJ])
  show (Oz_branch_on_true r l)  
    = "  branch_on_true " ++ intercalate ", " [show r, show l]
  show (Oz_branch_on_false r l) 
    = "  branch_on_false " ++ intercalate ", " [show r, show l]
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

nextLabel :: GenState LabelNum
nextLabel =
  do
    Gen ln rn <- get
    put $ Gen (ln + 1) rn
    return ln

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
      
repeatM :: (Monad m) => (a -> m [b]) -> [a] -> m [b]
repeatM f = liftM mconcat . mapM f
    
genProg :: ST.SymbolTable -> AST.Program -> GenState [OzCode]
genProg st (AST.Program _ _ ps) =
  do 
    procs <- repeatM (genProc st) ps
    return $ [ Oz_call (ProcLabel "main"), Oz_halt ] ++ procs

genProc :: ST.SymbolTable -> AST.Procedure -> GenState [OzCode]
genProc st@(ST.SymbolTable _ _ ps) (AST.Procedure name _ _ ss) = 
  do 
    let (ST.Procedure params vars stackSize) = fromJust $ lookup name ps
    let nParams = length params
    stmts <- repeatM (genStmt st) ss
    if stackSize > 0
    then return $ Oz_label (ProcLabel name)
                : Oz_push_stack_frame stackSize
                -- TODO: put params on stack
                : Oz_int_const (Reg 0) 0
                : take stackSize [ Oz_store i $ Reg 0 | i <- [nParams..] ]
               ++ stmts 
               ++ [ Oz_pop_stack_frame stackSize
                  , Oz_return
                  ]
    else return $ Oz_label (ProcLabel name) 
                -- TODO: put params on stack
                : stmts 
               ++ [ Oz_return ]

genStmt :: ST.SymbolTable -> AST.Stmt -> GenState [OzCode]
genStmt st (AST.Assign _ _) = -- TODO
  do 
    return []
genStmt st (AST.Read _) = -- TODO
  do 
    return []
genStmt st (AST.Write e) =
  do 
    expr <- genExpr st e
    return $ expr 
          ++ [ Oz_call_builtin "print_string" ]
genStmt st (AST.Writeln e) =
  do 
    expr <- genExpr st e
    return $ expr
          ++ [ Oz_call_builtin "print_string"
             , Oz_call_builtin "print_newline"
             ]
genStmt st (AST.If _ _) = -- TODO
  do 
    return []
genStmt st (AST.IfElse _ _ _) = -- TODO
  do 
    return []
genStmt st (AST.While _ _) = -- TODO
  do 
    return []
genStmt st (AST.Call _ _) = -- TODO
  do 
    return []

genExpr :: ST.SymbolTable -> AST.Expr -> GenState [OzCode]
genExpr _ (AST.LVal _ _) = -- TODO
  do 
    return []
genExpr _ (AST.BoolConst _ b) = -- TODO
  do 
    r <- getRegister
    return [Oz_int_const r (if b then 1 else 0)]
genExpr _ (AST.IntConst _ i) =
  do 
    r <- getRegister
    return [Oz_int_const r (fromInteger i)]
genExpr _ (AST.StrConst _ s) =
  do 
    r <- getRegister
    return [Oz_string_const r s]
genExpr _ (AST.BinOpExpr _ _ _ _) = -- TODO
  do 
    return []
genExpr _ (AST.UnOpExpr _ _ _) = -- TODO
  do 
    return []

p = "procedure main () { writeln \"Hello, World!\"; }"
pr = AST.Program [] [] [ AST.Procedure "main" [] [] [AST.Writeln (AST.StrConst AST.StrT "Hello, World!")]
                       , AST.Procedure "b" [] [] [AST.Writeln (AST.StrConst AST.StrT "Hello, World!")] -- incomplete
                       ]
st = ST.SymbolTable {ST.unRecords = [], ST.unArrays = [], ST.unProcedures = [ ("main",ST.Procedure {ST.unParams = [], ST.unVars = [], ST.unStackSize = 0})
                                                                            , ("b",ST.Procedure {ST.unParams = [], ST.unVars = [], ST.unStackSize = 1})
                                                                            ]}
s = [AST.Writeln (AST.StrConst AST.StrT "Hello, World!"), AST.Writeln (AST.StrConst AST.StrT "Hello, World!")]

printCode :: Either String [OzCode] -> IO ()
printCode (Right code) = putStr . unlines $ map show code
