module RooAST where

-----------------------------------
-- Specification of an AST for Roo
-----------------------------------

type Ident = String

data BaseType
  = IntType | BoolType
  deriving (Show, Eq)
  
data TypeName
  = Base BaseType
  | Alias Ident
  deriving (Show, Eq)
  
data Var
  = Var TypeName [Ident]
  deriving (Show, Eq)

data Record
  = Record [Field] Ident
  deriving (Show, Eq)
  
data Field
  = Field BaseType Ident
  deriving (Show, Eq)

data Array
  = Array Int TypeName Ident
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

data Stmt
  = Assign LValue Expr
  | Read LValue
  | Write Expr
  | Writeln Expr
  | If Expr [Stmt]
  | IfElse Expr [Stmt] [Stmt]
  | While Expr [Stmt]
  | Call Ident [Expr]
  deriving (Show, Eq)

data Param
  = Param Mode TypeName Ident
  deriving (Show, Eq)
  
data Mode 
  = Val | Ref
  deriving (Show, Eq)

data Procedure
  = Procedure [Param] [Var] [Stmt] Ident
  deriving (Show, Eq)
  
data Program
  = Program [Record] [Array] [Procedure]
  deriving (Show, Eq)
