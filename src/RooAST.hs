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
  | LField Ident Ident
  | LInd Ident Expr
  | LIndField Ident Expr Ident
  deriving (Show, Eq)

data BinOp
  = Op_or
  | Op_and
  | Op_eq
  | Op_neq
  | Op_ls
  | Op_leq
  | Op_gt
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

data Expr
  = Lval LValue
  | BoolConst Bool
  | IntConst Int
  | StrConst String
  | BinOpExpr BinOp Expr Expr
  | UnOpExpr UnOp Expr
  deriving (Show, Eq)

data Decl
  = Decl TypeName Ident
  deriving (Show, Eq)

data Stmt
  = Assign LValue Expr
  | Read LValue
  | Write Expr
  deriving (Show, Eq)

data Program
  = Program [Decl] [Stmt]
  deriving (Show, Eq)
