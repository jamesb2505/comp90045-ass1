module OzCode where 

import qualified RooAST as AST

import Data.List (intercalate)
  
-- LabelNum, RegNum, SlotNum
-- Type aliases for Ints, specifying the use of an Int argument
type LabelNum = Int
type RegNum = Int
type SlotNum = Int

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
    = "\tpush_stack_frame " ++ show i
  show (Oz_pop_stack_frame i)    
    = "\tpop_stack_frame " ++ show i
  show (Oz_store s r)            
    = "\tstore " ++ intercalate ", " [ show s, fmtReg r ]
  show (Oz_load r s)             
    = "\tload " ++ intercalate ", " [ fmtReg r, show s ]
  show (Oz_load_address r s)     
    = "\tload_address " ++ intercalate ", " [ fmtReg r, show s ]
  show (Oz_load_indirect rI rJ)  
    = "\tload_indirect " ++ intercalate ", " (map fmtReg [ rI, rJ ])
  show (Oz_store_indirect rI rJ) 
    = "\tstore_indirect " ++ intercalate ", " (map fmtReg [ rI, rJ ])
  show (Oz_int_const r i)       
    = "\tint_const " ++ intercalate ", " [ fmtReg r, show i ]
  show (Oz_real_const r d)      
    = "\treal_const " ++ intercalate ", " [ fmtReg r, show d ]
  show (Oz_string_const r s)    
    = "\tstring_const " ++ fmtReg r ++ ", \"" ++ s ++ "\"" 
  show (Oz_add_int rI rJ rK)
    = "\tadd_int " ++ intercalate ", " (map fmtReg [ rI, rJ, rK ])
  show (Oz_add_real rI rJ rK)
    = "\tadd_real " ++ intercalate ", " (map fmtReg [ rI, rJ, rK ])
  show (Oz_add_offset rI rJ rK)
    = "\tadd_offset " ++ intercalate ", " (map fmtReg [ rI, rJ, rK ])
  show (Oz_sub_int rI rJ rK)
    = "\tsub_int " ++ intercalate ", " (map fmtReg [ rI, rJ, rK ])
  show (Oz_sub_real rI rJ rK)
    = "\tsub_real " ++ intercalate ", " (map fmtReg [ rI, rJ, rK ])
  show (Oz_sub_offset rI rJ rK)
    = "\tsub_offset " ++ intercalate ", " (map fmtReg [ rI, rJ, rK ])
  show (Oz_mul_int rI rJ rK)
    = "\tmul_int " ++ intercalate ", " (map fmtReg [ rI, rJ, rK ])
  show (Oz_mul_real rI rJ rK)
    = "\tmul_real " ++ intercalate ", " (map fmtReg [ rI, rJ, rK ])
  show (Oz_div_int rI rJ rK)
    = "\tdiv_int " ++ intercalate ", " (map fmtReg [ rI, rJ, rK ])
  show (Oz_div_real rI rJ rK)
    = "\tdiv_real " ++ intercalate ", " (map fmtReg [ rI, rJ, rK ])
  show (Oz_neg_int rI rJ)
    = "\tneg_int " ++ intercalate ", " (map fmtReg [ rI, rJ ])
  show (Oz_neg_real rI rJ)
    = "\tneg_real " ++ intercalate ", " (map fmtReg [ rI, rJ ])
  show (Oz_cmp_eq_int rI rJ rK)
    = "\tcmp_eq_int " ++ intercalate ", " (map fmtReg [ rI, rJ, rK ])
  show (Oz_cmp_ne_int rI rJ rK)
    = "\tcmp_ne_int " ++ intercalate ", " (map fmtReg [ rI, rJ, rK ])
  show (Oz_cmp_gt_int rI rJ rK)
    = "\tcmp_gt_int " ++ intercalate ", " (map fmtReg [ rI, rJ, rK ])
  show (Oz_cmp_ge_int rI rJ rK)
    = "\tcmp_ge_int " ++ intercalate ", " (map fmtReg [ rI, rJ, rK ])
  show (Oz_cmp_lt_int rI rJ rK)
    = "\tcmp_lt_int " ++ intercalate ", " (map fmtReg [ rI, rJ, rK ])
  show (Oz_cmp_le_int rI rJ rK)
    = "\tcmp_le_int " ++ intercalate ", " (map fmtReg [ rI, rJ, rK ])
  show (Oz_cmp_eq_real rI rJ rK)
    = "\tcmp_eq_real " ++ intercalate ", " (map fmtReg [ rI, rJ, rK ])
  show (Oz_cmp_ne_real rI rJ rK)
    = "\tcmp_ne_real " ++ intercalate ", " (map fmtReg [ rI, rJ, rK ])
  show (Oz_cmp_gt_real rI rJ rK)
    = "\tcmp_gt_real " ++ intercalate ", " (map fmtReg [ rI, rJ, rK ])
  show (Oz_cmp_ge_real rI rJ rK)
    = "\tcmp_ge_real " ++ intercalate ", " (map fmtReg [ rI, rJ, rK ])
  show (Oz_cmp_lt_real rI rJ rK)
    = "\tcmp_lt_real " ++ intercalate ", " (map fmtReg [ rI, rJ, rK ])
  show (Oz_cmp_le_real rI rJ rK)
    = "\tcmp_le_real " ++ intercalate ", " (map fmtReg [ rI, rJ, rK ])
  show (Oz_and rI rJ rK)
    = "\tand " ++ intercalate ", " (map fmtReg [ rI, rJ, rK ])
  show (Oz_or rI rJ rK)
    = "\tor " ++ intercalate ", " (map fmtReg [ rI, rJ, rK ])
  show (Oz_not rI rJ)            
    = "\tnot " ++ intercalate ", " (map fmtReg [ rI, rJ ])
  show (Oz_int_to_real rI rJ)    
    = "\tint_to_real " ++ intercalate ", " (map fmtReg [ rI, rJ ])
  show (Oz_move rI rJ)           
    = "\tmove " ++ intercalate ", " (map fmtReg [ rI, rJ ])
  show (Oz_branch_on_true r l)  
    = "\tbranch_on_true " ++ intercalate ", " [ fmtReg r, show l ]
  show (Oz_branch_on_false r l) 
    = "\tbranch_on_false " ++ intercalate ", " [ fmtReg r, show l ]
  show (Oz_branch_uncond l)      
    = "\tbranch_uncond " ++ show l
  show (Oz_label l)               
    = show l ++ ":"
  show (Oz_call l)               
    = "\tcall " ++ show l
  show (Oz_call_builtin s)       
    = "\tcall_builtin " ++ s
  show (Oz_return)               
    = "\treturn"
  show (Oz_halt)                 
    = "halt"
  show (Oz_debug_reg r)         
    = "\tdebug_reg " ++ fmtReg r
  show (Oz_debug_slot s)         
    = "\tdebug_slot " ++ show s
  show (Oz_debug_stack)          
    = "\tdebug_stack"

-- fmtReg
-- Formates a RegNum
fmtReg :: RegNum -> String
fmtReg n = "r" ++ show n

-- printOzCodes
-- Prints a given [OzCode] with appropiate formatting
printOzCodes :: [OzCode] -> IO ()
printOzCodes code = putStr . dropWhile (== '\t') . unlines $ map show code

getBuiltinSuffix :: AST.ExprType -> Either String String
getBuiltinSuffix AST.BoolT = Right "bool"
getBuiltinSuffix AST.IntT  = Right "int"
getBuiltinSuffix AST.StrT  = Right "string"
getBuiltinSuffix t         = Left $ "no builtin for " ++ show t

getPrintBuiltin :: AST.ExprType -> Either String String
getPrintBuiltin t = ("print_" ++) <$> getBuiltinSuffix t

getReadBuiltin :: AST.ExprType -> Either String  String
getReadBuiltin t = ("read_" ++) <$> getBuiltinSuffix t

getBinOpCode :: AST.BinOp -> (RegNum -> RegNum -> RegNum -> OzCode)
getBinOpCode AST.Op_or  = Oz_or
getBinOpCode AST.Op_and = Oz_and
getBinOpCode AST.Op_eq  = Oz_cmp_eq_int
getBinOpCode AST.Op_neq = Oz_cmp_ne_int
getBinOpCode AST.Op_lt  = Oz_cmp_lt_int
getBinOpCode AST.Op_leq = Oz_cmp_le_int
getBinOpCode AST.Op_gt  = Oz_cmp_gt_int
getBinOpCode AST.Op_geq = Oz_cmp_ge_int
getBinOpCode AST.Op_add = Oz_add_int
getBinOpCode AST.Op_sub = Oz_sub_int
getBinOpCode AST.Op_mul = Oz_mul_int
getBinOpCode AST.Op_div = Oz_div_int

getUnOpCode :: AST.UnOp -> (RegNum -> RegNum -> OzCode)
getUnOpCode AST.Op_not = Oz_not
getUnOpCode AST.Op_neg = Oz_neg_int
