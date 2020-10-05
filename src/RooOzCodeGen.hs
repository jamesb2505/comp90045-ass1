module RooOzCodeGen where 

import RooAST as AST
import RooSymbolTable as ST

import Control.Monad.State
import Data.List (intercalate)

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
  show (ProcLabel l) = "proc_" ++ l ++ ":"
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
    = "  string_const" ++ show r ++ ", \"" ++ s ++ "\"" 
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
  show (Oz_call l)               
    = "  call " ++ show l
  show (Oz_call_builtin s)       
    = "  call_builtin " ++ show s
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
    (Gen ln rn) <- get
    put (Gen (ln + 1) rn)
    return ln

nextRegister :: GenState RegNum
nextRegister = 
  do
    (Gen ln r@(Reg rn)) <- get
    put (Gen ln (Reg $ rn + 1))
    return r

putRegister :: Int -> GenState ()
putRegister r =
  do
    (Gen ln _) <- get
    put (Gen ln (Reg r))

runCodeGen :: AST.Program -> ST.SymbolTable -> Either String [OzCode]
runCodeGen _ _ = Left "TODO"

-- genProg :: AST.Program -> GenState [OzCode]
-- etc.