module RooAST where

-----------------------------------
-- Specification of an AST for Roo
-----------------------------------

type Ident = String

data TypeName
  = BoolType
  | IntType
  | RecordType Record
  | ArrayType Array
  deriving (Show, Eq)

data Record
  = Record [Decl]
  deriving (Show, Eq)

data Array
  = Array TypeName Int
  deriving (Show, Eq)

data LValue
  = LId Ident
  | LIdComp Ident Ident
  | LIdInd Ident Exp
  | LIdIndComp Ident Exp Ident
  deriving (Show, Eq)

data BinOp
  = Op_or
  | Op_and
  | Op_equ
  | Op_neq
  | Op_lss
  | Op_leq
  | Op_gtr
  | Op_geq
  | Op_add
  | Op_sub
  | Op_mul
  | Op_div
  deriving (Show, Eq)

data UnOp
  = Op_not
  | Op_neg
  deriving (Show, Eq)    

data Exp
  = Lval LValue
  | BoolConst Bool
  | IntConst Int
  | StrConst String
  | BinOpExp BinOp Exp Exp
  | UnOpExp UnOp Exp
  deriving (Show, Eq)

data Decl
  = Decl TypeName Ident
  deriving (Show, Eq)

data Stmt
  = Assign LValue Exp
  | Read LValue
  | Write Exp
  deriving (Show, Eq)

data Program
  = Program [Decl] [Stmt]
  deriving (Show, Eq)
