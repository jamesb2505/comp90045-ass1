module RooAST where

-----------------------------------
-- Specification of an AST for Roo
-----------------------------------

type Ident = String

-- root of AST
data Program
  = Program [Record] [Array] [Procedure]
  deriving (Show, Eq)

data Record
  = Record [Field] Ident
  deriving (Show, Eq)
  
data Field
  = Field BaseType Ident
  deriving (Show, Eq)

data Array
  = Array Integer TypeName Ident
  deriving (Show, Eq)

data Procedure
  = Procedure Ident [Param] [Var] [Stmt]
  deriving (Show, Eq)

data Param
  = ParamAlias Ident Ident
  | ParamBase BaseType Mode Ident
  deriving (Show, Eq)
  
data Mode 
  = Val 
  | Ref
  deriving (Show, Eq)

data BaseType
  = IntType 
  | BoolType
  deriving (Show, Eq)
  
data TypeName
  = Base BaseType
  | Alias Ident
  deriving (Show, Eq)
  
data Var
  = Var TypeName [Ident]
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

data LValue
  = LId Ident
  | LField Ident Ident
  | LInd Ident Expr
  | LIndField Ident Expr Ident
  deriving (Show, Eq)   

data Expr
  = LVal LValue
  | BoolConst Bool
  | IntConst Integer
  | StrConst String
  | BinOpExpr BinOp Expr Expr
  | UnOpExpr UnOp Expr
  deriving (Show, Eq)

data BinOp
  = Op_or
  | Op_and
  | Op_eq
  | Op_neq
  | Op_lt
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

class Precedence op where
  prec :: op -> Int

instance Precedence BinOp where
  prec Op_mul = 5
  prec Op_div = 5
  prec Op_add = 4
  prec Op_sub = 4
  prec Op_and = 1
  prec Op_or  = 0
  prec _      = 3

instance Precedence UnOp where
  prec Op_neg = 6
  prec Op_not = 2

instance Precedence Expr where
  prec (BinOpExpr op _ _) = prec op
  prec (UnOpExpr op _)    = prec op
  prec _                  = 7

isLAssoc, isRAssoc :: BinOp -> Bool
isLAssoc _      = True
isRAssoc Op_eq  = True
isRAssoc Op_neq = True
isRAssoc Op_lt  = True
isRAssoc Op_leq = True
isRAssoc Op_gt  = True
isRAssoc Op_geq = True
isRAssoc _      = False
