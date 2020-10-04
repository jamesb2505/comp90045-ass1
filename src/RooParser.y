{
module RooParser (
  runParser
) where

import RooSymbolTable as ST
import RooLexer
import qualified RooAST as AST

import qualified Data.Containers.ListUtils as LU
import Control.Monad
import Data.Maybe

import Debug.Trace
}

%name runParser program
%tokentype { Lexeme }
%monad { Either String }
%error { parseError }

%attributetype { AttrTable a }
%attribute value   { a }
%attribute records { ST.Table ST.Record }
%attribute arrays  { ST.Table ST.Array }
%attribute procs   { ST.Table ST.Procedure }
%attribute symtab  { SymbolTable }
%attribute etype   { ExprType }

%token
  and        { (_, T_and) }
  array      { (_, T_array) }
  boolean    { (_, T_boolean) }
  call       { (_, T_call) }
  do         { (_, T_do) }
  else       { (_, T_else) }
  false      { (_, T_false) }
  fi         { (_, T_fi) }
  if         { (_, T_if) } 
  integer    { (_, T_integer) }
  not        { (_, T_not) }
  od         { (_, T_od) }
  or         { (_, T_or) }
  procedure  { (_, T_procedure) }
  read       { (_, T_read) }
  record     { (_, T_record) }
  then       { (_, T_then) }
  true       { (_, T_true) }
  val        { (_, T_val) }
  while      { (_, T_while) }
  write      { (_, T_write) }
  writeln    { (_, T_writeln) }
  '{'        { (_, T_lbrace) }
  '}'        { (_, T_rbrace) }
  '['        { (_, T_lbracket) }
  ']'        { (_, T_rbracket) }
  '('        { (_, T_lparen) }
  ')'        { (_, T_rparen) }
  ','        { (_, T_comma) }
  ';'        { (_, T_semi) }
  '.'        { (_, T_dot) }
  '<-'       { (_, T_assign) }
  '='        { (_, T_eq) }
  '!='       { (_, T_neq) }
  '<'        { (_, T_lt) }
  '<='       { (_, T_leq) }
  '>'        { (_, T_gt) }
  '>='       { (_, T_geq) }
  '+'        { (_, T_add) }
  '-'        { (_, T_sub) }
  '*'        { (_, T_mul) }
  '/'        { (_, T_div) }
  string     { (_, T_string $$) }
  number     { (_, T_number $$) }
  ident      { (_, T_ident $$) }

%left or
%left and
%left not
%nonassoc '=' '!=' '<' '<=' '>' '>='
%left '+' '-'
%left '*' '/'
%left NEG

%%

----------------------------
-- CFG for the Roo Language
----------------------------

program -- ~ :: { AST.Program }
  : records arrays procedures 
    { $$ = AST.Program $1 $2 $3
    ; $$.records = $1.records
    ; $2.records = $1.records
    ; $3.records = $1.records
    ; $$.arrays = $2.arrays
    ; $3.arrays = $2.arrays
    ; $$.procs = $3.procs
    }

records -- ~ :: { [AST.Record] }
  : records_ 
    { $$ = reverse $1
    ; $$.records = reverse $1.records
    }
records_ -- ~ :: { [AST.Record] } 
  : {- empty -}  
    { $$ = []
    ; $$.records = []
    }
  | records_ rec 
    { $$ = $2:$1
    ; $$.records = $2.records ++ $1.records
    ; where let alias = fst $ head $2.records in
            unless (not $ elem alias (ST.tableKeys $ $1.records))
                   (fail $ "dupicate record alias: `" 
                           ++ show alias ++ "`")
    }

rec -- ~ :: { AST.Record }
  : record '{' fields '}' ident ';' 
    { $$ = AST.Record $3 $5
    ; $$.records = [ entryRecord $$ ]
    ; where unless (ST.noDuplicates (map (\(AST.Field _ i) -> i) $3 ))
                   (fail $ "duplicate field name in record `" ++ $5 ++ "`")
    }

fields -- ~ :: { [AST.Field] }
  : fields_ { $$ = reverse $1 }
fields_ -- ~ :: { [AST.Field] }
  : field             { $$ = [$1] }
  | fields_ ';' field { $$ = $3:$1 }

field -- ~ :: { AST.Field }
  : basetype ident { $$ = AST.Field $1 $2 }

basetype -- ~ :: { AST.BaseType }
  : boolean { $$ = AST.BoolType }
  | integer { $$ = AST.IntType }

arrays -- ~ :: { [AST.Array] }
  : arrays_ 
    { $$ = reverse $1
    ; $1.records = $$.records
    ; $$.arrays = reverse $1.arrays
    }
arrays_ -- ~ :: { [AST.Array] }
  : {- empty -} 
    { $$ = []
    ; $$.arrays = []
    }
  | arrays_ arr 
    { $$ = $2:$1
    ; $1.records = $$.records
    ; $2.records = $$.records 
    ; $$.arrays = $2.arrays ++ $1.arrays
    ; where checkDuplicate (fst $ head $2.arrays) (ST.tableKeys $$.arrays)
                           "dupicate array alias"
    ; where checkDuplicate (fst $ head $2.arrays) (ST.tableKeys $$.records)
                           "dupicate record/array alias"
    }

arr -- ~ :: { AST.Array }
  : array '[' number ']' typename ident ';' 
    { $$ = AST.Array $3 $5 $6
    ; $$.arrays = [ entryArray $$ ]
    }

typename -- ~ :: { AST.TypeName }
  : basetype { $$ = AST.Base $1 }
  | ident    { $$ = AST.Alias $1 }

procedures -- ~ :: { [AST.Procedure] }
  : procedures_ 
    { $$ = reverse $1 
    ; $1.records = $$.records
    ; $1.arrays = $$.arrays
    ; $$.procs = reverse $1.procs
    }
procedures_ -- ~ :: { [AST.Procedure] }
  : proc             
    { $$ = [$1]
    ; $$.procs = [ entryProcedure (PartialTable $$.records $$.arrays) $1 ]
    ; $$.symtab = ST.SymbolTable $$.records $$.arrays $$.procs
    ; $1.symtab = $$.symtab
    }
  | procedures_ proc 
    { $$ = $2:$1
    ; $1.records = $$.records
    ; $2.records = $$.records 
    ; $1.arrays = $$.arrays
    ; $2.arrays = $$.arrays 
    ; $$.procs = $2.procs ++ $1.procs
    ; $2.symtab = $1.symtab { unProcedures = $2.procs }
    ; where checkDuplicate (fst $ head $2.procs) (ST.tableKeys $1.procs)
                           "dupicate procdure name"
    }

proc -- ~ :: { AST.Procedure }
  : procedure ident '(' params ')' vars '{' stmts '}' 
    { $$ = AST.Procedure $2 $4 $6 $8 
    ; $$.procs = [ entryProcedure (PartialTable $$.records $$.arrays) $$ ]
    ; $8.records = $$.records
    ; $8.arrays = $$.arrays
    ; $8.procs = $$.procs
    ; $8.symtab = $$.symtab
    ; where let proc = snd $ head $$.procs in
            checkDuplicates (map fst (unParams proc) ++ map fst (unVars proc))
                            ("duplicate variable/parmeter in: `" 
                             ++ (fst $ head $$.procs) ++ "`")
    }

params -- ~ :: { [AST.Param] }
  : {- empty -}       { $$ = [] }
  | params_           { $$ = reverse $1 }
params_ -- ~ :: { [AST.Param] }
  : param             { $$ = [$1] }
  | params_ ',' param { $$ = $3:$1 }

param -- ~ :: { AST.Param }
  : ident ident         { $$ = AST.ParamAlias $1 $2 }
  | basetype mode ident { $$ = AST.ParamBase $1 $2 $3 }

mode -- ~ :: { AST.Mode }
  : val         { $$ = AST.Val }
  | {- empty -} { $$ = AST.Ref }

vars -- ~ :: { [AST.Var] }
  : vars_ { $$ = reverse $1 }
vars_ -- ~ :: { [AST.Var] }
  : {- empty -} { $$ = [] }
  | vars_ var   { $$ = $2:$1 }

var -- ~ :: { AST.Var }
  : typename idents ';' { $$ = AST.Var $1 $2 }

idents -- ~ :: { [AST.Ident] }
  : idents_ { $$ = reverse $1 }
idents_ -- ~ :: { [AST.Ident] }
  : ident             { $$ = [$1] }
  | idents_ ',' ident { $$ = $3:$1 }

stmts -- ~ :: { [AST.Stmt] }
  : stmts_ 
    { $$ = reverse $1 
    ; $1.records = $$.records 
    ; $1.arrays = $$.arrays 
    ; $1.symtab = $$.symtab 
    } 
stmts_ -- ~ :: { [AST.Stmt] } 
  : stmt        
    { $$ = [$1] 
    ; $1.records = $$.records 
    ; $1.arrays = $$.arrays 
    ; $1.symtab = $$.symtab 
    }
  | stmts_ stmt 
    { $$ = $2:$1
    ; $1.records = $$.records 
    ; $2.records = $$.records 
    ; $1.arrays = $$.arrays 
    ; $2.arrays = $$.arrays 
    ; $1.symtab = $$.symtab
    ; $2.symtab = $$.symtab 
    }  

stmt -- ~ :: { AST.Stmt }
  : lval '<-' expr ';'               
    { $$ = AST.Assign $1 $3 
    ; $1.records = $$.records 
    ; $3.records = $$.records 
    ; $1.arrays = $$.arrays 
    ; $3.arrays = $$.arrays
    ; $1.symtab = $$.symtab
    ; $3.symtab = $$.symtab
    ; where unless ($1.etype == $3.etype && isAssignableT $1.etype)
                   (fail "bad assignment")
    }
  | read lval ';'                    
    { $$ = AST.Read $2
    ; $2.records = $$.records 
    ; $2.arrays = $$.arrays 
    ; $2.symtab = $$.symtab
    ; where unless (isAssignableT $2.etype)
                   (fail "bad read")
    }
  | write expr ';'                   
    { $$ = AST.Write $2
    ; $2.records = $$.records 
    ; $2.arrays = $$.arrays 
    ; $2.symtab = $$.symtab
    ; where unless (isWriteableT $2.etype)
                   (fail "bad write")
    }
  | writeln expr ';'                 
    { $$ = AST.Writeln $2 
    ; $2.records = $$.records 
    ; $2.arrays = $$.arrays 
    ; $2.symtab = $$.symtab
    ; where unless (isWriteableT $2.etype)
                   (fail "bad writeln")
    }
  | if expr then stmts else stmts fi 
    { $$ = AST.IfElse $2 $4 $6 
    ; $2.records = $$.records 
    ; $4.records = $$.records 
    ; $6.records = $$.records 
    ; $2.arrays = $$.arrays 
    ; $4.arrays = $$.arrays 
    ; $6.arrays = $$.arrays 
    ; $2.symtab = $$.symtab
    ; $4.symtab = $$.symtab
    ; $6.symtab = $$.symtab
    ; where unless (isIntT $2.etype)
                   (fail "bad bool")
    }
  | if expr then stmts fi            
    { $$ = AST.If $2 $4 
    ; $2.records = $$.records 
    ; $4.records = $$.records 
    ; $2.arrays = $$.arrays 
    ; $4.arrays = $$.arrays 
    ; $2.symtab = $$.symtab 
    ; $4.symtab = $$.symtab 
    ; where unless (isIntT $2.etype)
                   (fail "bad bool")
    }
  | while expr do stmts od           
    { $$ = AST.While $2 $4 
    ; $2.records = $$.records 
    ; $4.records = $$.records 
    ; $2.arrays = $$.arrays 
    ; $4.arrays = $$.arrays 
    ; $2.symtab = $$.symtab 
    ; $4.symtab = $$.symtab 
    }
  | call ident '(' exprs ')' ';' {- procedure calls checked later -}
    { $$ = AST.Call $2 $4 
    ; $4.records = $$.records
    ; $4.arrays = $$.arrays 
    ; $4.symtab = $$.symtab 
    }

lval -- ~ :: { AST.LValue }
  : ident                        
    { $$ = AST.LId $1
    ; $$.etype = getAliasType $$.symtab $1
    ; where unless (isAssignableT $$.etype)
                   (fail "ident")
    }
  | ident '.' ident              
    { $$ = AST.LField $1 $3
    ; $$.etype = getFieldType $$.records (getAliasType $$.symtab $1) $3
    ; where unless (isRecordT $ getAliasType $$.symtab $1)
                   (fail "record")
    }
  | ident '[' expr ']'           
    { $$ = AST.LInd $1 $3 
    ; $$.etype = getArrayType $ getAliasType $$.symtab $1
    ; $3.records = $$.records
    ; $3.arrays = $$.arrays 
    ; $3.symtab = $$.symtab 
    ; where unless (isIntT $3.etype)
                   (fail "index")
    ; where unless (isArrayT $ getAliasType $$.symtab $1)
                   (fail "record")
    }
  | ident '[' expr ']' '.' ident 
    { $$ = AST.LIndField $1 $3 $6 
    ; $$.etype = getFieldType $$.records (getArrayType $ getAliasType $$.symtab $1) $6
    ; $3.records = $$.records
    ; $3.arrays = $$.arrays 
    ; $3.symtab = $$.symtab 
    ; where unless (isArrayT $ getAliasType $$.symtab $1)
                   (fail "array record")
    ; where unless (isRecordT . getArrayType $ getAliasType $$.symtab $1)
                   (fail "record")
    ; where unless (isIntT $3.etype)
                   (fail "index")
    }

exprs -- ~ :: { [AST.Expr] }
  : {- empty -} { $$ = [] }
  | exprs_      
    { $$ = reverse $1 
    ; $1.records = $$.records 
    ; $1.arrays = $$.arrays 
    ; $1.symtab = $$.symtab 
    }
exprs_ -- ~ :: { [AST.Expr] }
  : expr            
    { $$ = [$1]
    ; $1.records = $$.records 
    ; $1.arrays = $$.arrays 
    ; $1.symtab = $$.symtab
    }
  | exprs_ ',' expr 
    { $$ = $3:$1
    ; $1.records = $$.records 
    ; $3.records = $$.records 
    ; $1.arrays = $$.arrays 
    ; $3.arrays = $$.arrays
    ; $1.symtab = $$.symtab
    ; $3.symtab = $$.symtab
    }

-- operator precendence is handled as defined above
expr -- ~ :: { AST.Expr }
  : expr or expr       
    { $$ = AST.BinOpExpr AST.Op_or $1 $3 
    ; $1.records = $$.records 
    ; $3.records = $$.records 
    ; $1.arrays = $$.arrays 
    ; $3.arrays = $$.arrays
    ; $1.symtab = $$.symtab
    ; $3.symtab = $$.symtab
    ; $$.etype = BoolT
    ; where unless ($1.etype == $3.etype && isBoolT $1.etype)
                   (fail "bad or")
    }
  | expr and expr      
    { $$ = AST.BinOpExpr AST.Op_and $1 $3 
    ; $1.records = $$.records 
    ; $3.records = $$.records 
    ; $1.arrays = $$.arrays 
    ; $3.arrays = $$.arrays
    ; $1.symtab = $$.symtab
    ; $3.symtab = $$.symtab
    ; $$.etype = BoolT
    ; where unless ($1.etype == $3.etype && isBoolT $1.etype)
                   (fail "bad and")
    }
  | not expr           
    { $$ = AST.UnOpExpr AST.Op_not $2
    ; $2.records = $$.records 
    ; $2.arrays = $$.arrays 
    ; $2.symtab = $$.symtab
    ; $$.etype = BoolT
    ; where unless (isBoolT $2.etype)
                   (fail "bad not")
    }
  | expr '=' expr      
    { $$ = AST.BinOpExpr AST.Op_eq $1 $3
    ; $1.records = $$.records 
    ; $3.records = $$.records 
    ; $1.arrays = $$.arrays 
    ; $3.arrays = $$.arrays
    ; $1.symtab = $$.symtab
    ; $3.symtab = $$.symtab
    ; $$.etype = $1.etype
    ; where unless ($1.etype == $3.etype && isComparableT $1.etype)
                   (fail "=")
    }
  | expr '!=' expr     
    { $$ = AST.BinOpExpr AST.Op_neq $1 $3
    ; $1.records = $$.records 
    ; $3.records = $$.records 
    ; $1.arrays = $$.arrays 
    ; $3.arrays = $$.arrays
    ; $1.symtab = $$.symtab
    ; $3.symtab = $$.symtab
    ; $$.etype = $1.etype
    ; where unless ($1.etype == $3.etype && isComparableT $1.etype)
                   (fail "!=")
    }
  | expr '<' expr      
    { $$ = AST.BinOpExpr AST.Op_lt $1 $3
    ; $1.records = $$.records 
    ; $3.records = $$.records 
    ; $1.arrays = $$.arrays 
    ; $3.arrays = $$.arrays
    ; $1.symtab = $$.symtab
    ; $3.symtab = $$.symtab
    ; $$.etype = $1.etype
    ; where unless ($1.etype == $3.etype && isComparableT $1.etype)
                   (fail "<")
    }
  | expr '<=' expr     
    { $$ = AST.BinOpExpr AST.Op_leq $1 $3
    ; $1.records = $$.records 
    ; $3.records = $$.records 
    ; $1.arrays = $$.arrays 
    ; $3.arrays = $$.arrays
    ; $1.symtab = $$.symtab
    ; $3.symtab = $$.symtab
    ; $$.etype = $1.etype
    ; where unless ($1.etype == $3.etype && isComparableT $1.etype)
                   (fail "<=")
    }
  | expr '>' expr      
    { $$ = AST.BinOpExpr AST.Op_gt $1 $3
    ; $1.records = $$.records 
    ; $3.records = $$.records 
    ; $1.arrays = $$.arrays 
    ; $3.arrays = $$.arrays
    ; $1.symtab = $$.symtab
    ; $3.symtab = $$.symtab
    ; $$.etype = $1.etype
    ; where unless ($1.etype == $3.etype && isComparableT $1.etype)
                   (fail ">")
    }
  | expr '>=' expr     
    { $$ = AST.BinOpExpr AST.Op_geq $1 $3
    ; $1.records = $$.records 
    ; $3.records = $$.records 
    ; $1.arrays = $$.arrays 
    ; $3.arrays = $$.arrays
    ; $1.symtab = $$.symtab
    ; $3.symtab = $$.symtab
    ; $$.etype = $1.etype
    ; where unless ($1.etype == $3.etype && isComparableT $1.etype)
                   (fail ">=")
    }
  | expr '+' expr      
    { $$ = AST.BinOpExpr AST.Op_add $1 $3
    ; $1.records = $$.records 
    ; $3.records = $$.records 
    ; $1.arrays = $$.arrays 
    ; $3.arrays = $$.arrays
    ; $1.symtab = $$.symtab
    ; $3.symtab = $$.symtab
    ; $$.etype = IntT
    ; where unless ($1.etype == $3.etype && isIntT $1.etype)
                   (fail "+")
    }
  | expr '-' expr      
    { $$ = AST.BinOpExpr AST.Op_sub $1 $3
    ; $1.records = $$.records 
    ; $3.records = $$.records 
    ; $1.arrays = $$.arrays 
    ; $3.arrays = $$.arrays
    ; $1.symtab = $$.symtab
    ; $3.symtab = $$.symtab
    ; $$.etype = IntT
    ; where unless ($1.etype == $3.etype && isIntT $1.etype)
                   (fail "-")
    }
  | expr '*' expr      
    { $$ = AST.BinOpExpr AST.Op_mul $1 $3
    ; $1.records = $$.records 
    ; $3.records = $$.records 
    ; $1.arrays = $$.arrays 
    ; $3.arrays = $$.arrays
    ; $1.symtab = $$.symtab
    ; $3.symtab = $$.symtab
    ; $$.etype = IntT
    ; where unless ($1.etype == $3.etype && isIntT $1.etype)
                   (fail "*")
    }
  | expr '/' expr      
    { $$ = AST.BinOpExpr AST.Op_div $1 $3 
    ; $1.records = $$.records 
    ; $3.records = $$.records 
    ; $1.arrays = $$.arrays 
    ; $3.arrays = $$.arrays
    ; $1.symtab = $$.symtab
    ; $3.symtab = $$.symtab
    ; $$.etype = IntT
    ; where unless ($1.etype == $3.etype && isIntT $1.etype)
                   (fail "/")
    } 
  | '-' expr %prec NEG 
    { $$ = AST.UnOpExpr AST.Op_neg $2
    ; $2.records = $$.records 
    ; $2.arrays = $$.arrays 
    ; $2.symtab = $$.symtab
    ; $$.etype = IntT
    ; where unless (isIntT $2.etype)
                   (fail "*")
    }
  | lval               
    { $$ = AST.LVal $1 
    ; $1.records = $$.records 
    ; $1.arrays = $$.arrays 
    ; $1.symtab = $$.symtab
    ; $$.etype = $1.etype
    }
  | false              
    { $$ = AST.BoolConst False 
    ; $$.etype = BoolT
    }
  | true               
    { $$ = AST.BoolConst True 
    ; $$.etype = BoolT
    }
  | number             
    { $$ = AST.IntConst $1 
    ; $$.etype = IntT
    }
  | string             
    { $$ = AST.StrConst $1 
    ; $$.etype = StrT
    }
  | '(' expr ')'       
    { $$ = $2 
    ; $2.records = $$.records 
    ; $2.arrays = $$.arrays 
    ; $2.symtab = $$.symtab
    ; $$.etype = $2.etype
    }

{

data ExprType
  = BoolT
  | IntT
  | StrT
  | ArrayT ExprType
  | RecordT AST.Ident
  | ErrorT
  deriving (Eq, Show)

isAssignableT :: ExprType -> Bool
isAssignableT BoolT = True
isAssignableT IntT  = True
isAssignableT _     = False

isWriteableT :: ExprType -> Bool
isWriteableT BoolT = True
isWriteableT IntT  = True
isWriteableT StrT  = True
isWriteableT _     = False

isComparableT :: ExprType -> Bool
isComparableT BoolT = True
isComparableT IntT  = True
isComparableT _     = False

isBoolT :: ExprType -> Bool
isBoolT BoolT = True
isBoolT _     = True

isIntT :: ExprType -> Bool
isIntT IntT = True
isIntT _     = True

isRecordT :: ExprType -> Bool
isRecordT (RecordT _) = True
isRecordT _           = False

isArrayT :: ExprType -> Bool
isArrayT (ArrayT _) = True
isArrayT _          = False

getType :: ST.SymbolTable -> AST.TypeName -> ExprType
getType st@(SymbolTable rs as ((_,(Procedure ps vs _)):_)) (AST.Alias alias)
  | elem alias (ST.tableKeys rs) 
    = RecordT alias
  | elem alias (ST.tableKeys as) 
    = case lookup alias as of
        Nothing          -> ErrorT
        Just (Array t _) -> ArrayT $ getType st t
  | elem alias (ST.tableKeys ps)
    = case lookup alias ps of
        Nothing            -> ErrorT
        Just (Param t _ _) -> getType st t
  | elem alias (ST.tableKeys vs)
    = case lookup alias vs of
        Nothing        -> ErrorT
        Just (Var t _) -> getType st t
getType _ (AST.Base AST.BoolType) = BoolT
getType _ (AST.Base AST.IntType) = IntT
getType _ _ = ErrorT

getAliasType :: ST.SymbolTable -> AST.Ident -> ExprType
getAliasType st ident = getType st (AST.Alias ident)

getFieldType :: ST.Table Record -> ExprType -> AST.Ident -> ExprType
getFieldType rs rt@(RecordT r) f
    = case lookup r rs >>= lookup f . ST.unFields of
        Nothing                       -> ErrorT
        (Just (ST.Field AST.BoolType _)) -> BoolT
        (Just (ST.Field AST.IntType _))  -> IntT
getFieldType _ _ _ = ErrorT 

getArrayType :: ExprType -> ExprType
getArrayType (ArrayT t) = t
getArrayType _          = ErrorT

checkDuplicate :: (Monad m) => [Char] -> [[Char]] -> String -> m ()
checkDuplicate name keys msg =
 unless (not $ elem name keys)
        (fail $ msg ++ ": `" ++ name ++ "`")

checkDuplicates :: (Ord a, Monad m) => [a] -> String -> m ()
checkDuplicates xs msg = 
 unless (length (LU.nubOrd xs) == length xs) (fail msg)

parseError :: [Lexeme] -> Either String a
parseError []                    = Left "Unxpected parse error at end of file"
parseError ((AlexPn _ l c, t):_) = Left $ "Unxpected " ++ show t 
                                          ++ " at line " ++ show l
                                          ++ ", column " ++ show c

str = "record {integer a; integer b} rec; procedure main() integer i; { i <- j;}"
}