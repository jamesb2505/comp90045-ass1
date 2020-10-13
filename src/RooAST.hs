module RooAST where

-----------------------------------
-- Specification of an AST for Roo
-----------------------------------

type Ident = String

-- Program
-- Root of AST
data Program
  = Program [Record] [Array] [Procedure]
  deriving (Show, Eq)

-- Record
-- Represents a record definition
data Record
  = Record [Field] Ident
  deriving (Show, Eq)
  
-- Field
-- Represents a field of a record definition
data Field
  = Field AtomicType Ident
  deriving (Show, Eq)

-- Array
-- Represents an array definition
data Array
  = Array Integer TypeName Ident
  deriving (Show, Eq)

-- Prodecure
-- Represents a procedure definition
data Procedure
  = Procedure Ident [Param] [Var] [Stmt]
  deriving (Show, Eq)

-- Param
-- Represents a parameter of a procedure definition
data Param
  = ParamAlias Ident Ident
  | ParamAtomic AtomicType Mode Ident
  deriving (Show, Eq)
  
-- Mode
-- Represents a the mode of a parameter definition
data Mode 
  = Val 
  | Ref
  deriving (Show, Eq)

-- AtomicType
-- Represents a record definition
data AtomicType
  = IntType 
  | BoolType
  deriving (Show, Eq)
  
-- TypeName
-- Represents a type name/alias
data TypeName
  = Atomic AtomicType
  | Alias Ident
  deriving (Show, Eq)
  
-- Var
-- Represents a list of variable declarations
data Var
  = Var TypeName [Ident]
  deriving (Show, Eq)

-- Stmt
-- Represents a statement
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

-- LValue
-- Represents an lvalue
data LValue
  = LId Ident
  | LField Ident Ident
  | LInd Ident Expr
  | LIndField Ident Expr Ident
  deriving (Show, Eq)   

-- Expr
-- Represents an expression
data Expr
  = LVal ExprType LValue
  | BoolConst ExprType Bool
  | IntConst ExprType Integer
  | StrConst ExprType String
  | BinOpExpr ExprType BinOp Expr Expr
  | UnOpExpr ExprType UnOp Expr
  deriving (Show, Eq)

-- ExprType
-- Represents the type of an expresion
-- This is inferred by the expression and it's arguement's types
data ExprType
  = BoolT
  | IntT
  | StrT
  | ArrayT Ident ExprType
  | RecordT Ident
  | ErrorT
  deriving (Eq, Show)

-- BinOp
-- Represents a binary operaton
data BinOp
  = Op_or
  | Op_and
  | Op_eq
  | Op_ne
  | Op_lt
  | Op_le
  | Op_gt
  | Op_ge
  | Op_add
  | Op_sub
  | Op_mul
  | Op_div
  deriving (Show, Eq)

-- UnOp
-- Represents a unary operaton
data UnOp
  = Op_not
  | Op_neg
  deriving (Show, Eq) 

-- Precedence op
-- Type class used to get the operator precedence
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
  prec (BinOpExpr _ op _ _) = prec op
  prec (UnOpExpr _ op _)    = prec op
  prec _                    = 7

-- isLAssoc, isRAssoc
-- Gets the left or right associativity of a BinOp
isLAssoc, isRAssoc :: BinOp -> Bool
isLAssoc _     = True
isRAssoc Op_eq = True
isRAssoc Op_ne = True
isRAssoc Op_lt = True
isRAssoc Op_le = True
isRAssoc Op_gt = True
isRAssoc Op_ge = True
isRAssoc _     = False

-- isLVal
-- True if Expr is an LVal, else False
isLVal :: Expr -> Bool
isLVal (LVal _ _) = True
isLVal _          = False

-- getLId
-- Gets the Ident of a given LValue
getLId :: LValue -> Ident
getLId (LId i)           = i
getLId (LField i _)      = i
getLId (LInd i _)        = i
getLId (LIndField i _ _) = i

-- getLVal
-- Gets the LVal of a given Expr, else Nothing
getLVal :: Expr -> Maybe LValue
getLVal (LVal _ l) = Just l
getLVal _          = Nothing 

-- getExprType
-- Gets the ExprType of a given Expr
getExprType :: Expr -> ExprType
getExprType (LVal t _)          = t
getExprType (BoolConst t _)     = t
getExprType (IntConst t _)      = t
getExprType (StrConst t _)      = t
getExprType (BinOpExpr t _ _ _) = t
getExprType (UnOpExpr t _ _)    = t

-- isAssignableT
-- True if a ExprType can be assigned if in Val mode
isAssignableT :: ExprType -> Bool
isAssignableT BoolT = True
isAssignableT IntT  = True
isAssignableT _     = False

-- isWriteableT
-- True if a ExprType can be written, else False
isWriteableT :: ExprType -> Bool
isWriteableT BoolT = True
isWriteableT IntT  = True
isWriteableT StrT  = True
isWriteableT _     = False

-- isComparableT
-- True if a ExprType can be compared, else False
isComparableT :: ExprType -> Bool
isComparableT BoolT = True
isComparableT IntT  = True
isComparableT _     = False

-- isBoolT
-- True if a ExprType is a BoolT, else False
isBoolT :: ExprType -> Bool
isBoolT BoolT = True
isBoolT _     = False

-- isIntT
-- True if a ExprType is a IntT, else False
isIntT :: ExprType -> Bool
isIntT IntT = True
isIntT _    = False

-- isRecordT
-- True if a ExprType is a RecordT, else False
isRecordT :: ExprType -> Bool
isRecordT (RecordT _) = True
isRecordT _           = False

-- isArrayT
-- True if a ExprType is a ArrayT, else False
isArrayT :: ExprType -> Bool
isArrayT (ArrayT _ _) = True
isArrayT _            = False

-- getTypeName
-- Converts an ExprType into a TypeName
getTypeName :: ExprType -> TypeName
getTypeName (RecordT alias) = Alias alias
getTypeName (ArrayT alias _) = Alias alias
getTypeName BoolT = Atomic BoolType
getTypeName IntT = Atomic IntType
