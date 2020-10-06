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

%name runHappy program
%tokentype { Lexeme }
%monad { Either String }
%error { parseError }

%attributetype { AttrTable a }
%attribute value   { a }
%attribute records { ST.Table ST.Record }
%attribute arrays  { ST.Table ST.Array }
%attribute procs   { ST.Table ST.Procedure }
%attribute symtab  { ST.SymbolTable }
%attribute etype   { AST.ExprType }

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

-- parses a Roo Program
program -- ~ :: { (AST.Program, ST.SymbolTable) }
  : records arrays procedures 
    { $$ = (AST.Program $1 $2 $3, ST.SymbolTable $$.records $$.arrays $$.procs)
    ; $$.records = $1.records
    ; $2.records = $1.records
    ; $3.records = $1.records
    ; $$.arrays = $2.arrays
    ; $3.arrays = $2.arrays
    ; $$.procs = $3.procs
    ; where unless (isTableKey "main" $$.procs)
                   (Left "mising definition of `main` procedure")
    ; where unless ((not $ isTableKey "main" $$.procs)
                    || (null . unParams . fromJust $ lookup "main" $$.procs))
                   (Left "`main` procedure defintion contains parameters")
    }

-- parses a sequence of record declarations
records -- ~ :: { [AST.Record] }
  : records_ 
    { $$ = reverse $1
    ; $$.records = reverse $1.records
    }
-- parses a sequence of record declarations
-- WARNING: output in reverse
records_ -- ~ :: { [AST.Record] } 
  : {- empty -}  
    { $$ = []
    ; $$.records = []
    }
  | records_ rec 
    { $$ = $2:$1
    ; $$.records = $2.records ++ $1.records
    ; where checkDuplicate (fst $ head $2.records) (ST.tableKeys $ $1.records)
                           "duplicate record alias"
    }

-- parses a record declaration
rec -- ~ :: { AST.Record }
  : record '{' fields '}' ident ';' 
    { $$ = AST.Record $3 $5
    ; $$.records = [ entryRecord $$ ]
    ; where unless (noDuplicates (map (\(AST.Field _ i) -> i) $3 ))
                   (Left $ "duplicate field name in record `" ++ $5 ++ "`")
    }

-- parses a non-empty sequence of record fields
fields -- ~ :: { [AST.Field] }
  : fields_ { $$ = reverse $1 }
-- parses a non-empty sequence of record fields
-- WARNING: output in reverse
fields_ -- ~ :: { [AST.Field] }
  : field             { $$ = [$1] }
  | fields_ ';' field { $$ = $3:$1 }

-- parses a record field
field -- ~ :: { AST.Field }
  : basetype ident { $$ = AST.Field $1 $2 }

-- parses a basetype (integer or boolean)
basetype -- ~ :: { AST.AtomicType }
  : boolean { $$ = AST.BoolType }
  | integer { $$ = AST.IntType }

-- parses a sequence of array declarations
arrays -- ~ :: { [AST.Array] }
  : arrays_ 
    { $$ = reverse $1
    ; $1.records = $$.records
    ; $$.arrays = reverse $1.arrays
    }
-- parses a sequence of array declarations
-- WARNING: output in reverse
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
    ; where checkDuplicate (fst $ head $2.arrays) (ST.tableKeys $1.arrays)
                           "duplicate array alias"
    ; where checkDuplicate (fst $ head $2.arrays) (ST.tableKeys $$.records)
                           "duplicate record/array alias"
    }

-- parses an array declarations
arr -- ~ :: { AST.Array }
  : array '[' number ']' typename ident ';' 
    { $$ = AST.Array $3 $5 $6
    ; $$.arrays = [ entryArray $$ ]
    }

-- parses a typename (alias or atomic type)
typename -- ~ :: { AST.TypeName }
  : basetype { $$ = AST.Atomic $1 }
  | ident    { $$ = AST.Alias $1 }

-- parses a non-empty sequence of procedure definitions
procedures -- ~ :: { [AST.Procedure] }
  : procedures_ 
    { $$ = reverse $1 
    ; $1.records = $$.records
    ; $1.arrays = $$.arrays
    ; $$.procs = reverse $1.procs
    }
-- parses a non-empty sequence of procedure definitions
-- WARNING: output in reverse
procedures_ -- ~ :: { [AST.Procedure] }
  : proc             
    { $$ = [$1]
    ; $$.procs = [ entryProcedure (PartialTable $$.records $$.arrays) $1 ]
    ; $1.records = $$.records
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
                           "duplicate procedure name"
    }

-- parses a procedure definition
proc -- ~ :: { AST.Procedure }
  : procedure ident '(' params ')' vars '{' stmts '}' 
    { $$ = AST.Procedure $2 $4 $6 $8 
    ; $$.procs = [ entryProcedure (PartialTable $$.records $$.arrays) $$ ]
    ; $8.records = $$.records
    ; $8.procs = $$.procs
    ; $8.symtab = $$.symtab
    ; where let proc = snd $ head $$.procs in
            checkDuplicates (map fst (unParams proc) ++ map fst (unVars proc))
                            ("duplicate variable/parmeter in procedure definiton: `" 
                             ++ (fst $ head $$.procs) ++ "`")
    }

-- parses a sequence of parameter declarations
params -- ~ :: { [AST.Param] }
  : {- empty -}       { $$ = [] }
  | params_           { $$ = reverse $1 }
-- parses a non-empty sequence of parameter declarations
-- WARNING: output in reverse
params_ -- ~ :: { [AST.Param] }
  : param             { $$ = [$1] }
  | params_ ',' param { $$ = $3:$1 }

-- parses a parameter definition
param -- ~ :: { AST.Param }
  : ident ident         { $$ = AST.ParamAlias $1 $2 }
  | basetype mode ident { $$ = AST.ParamAtomic $1 $2 $3 }

-- parses a mode (ref or val)
mode -- ~ :: { AST.Mode }
  : val         { $$ = AST.Val }
  | {- empty -} { $$ = AST.Ref }

-- parses a sequence of variable declarations
vars -- ~ :: { [AST.Var] }
  : vars_ { $$ = reverse $1 }
-- parses a sequence of variable declarations
-- WARNING: output in reverse
vars_ -- ~ :: { [AST.Var] }
  : {- empty -} { $$ = [] }
  | vars_ var   { $$ = $2:$1 }

-- parses a variable declaration
var -- ~ :: { AST.Var }
  : typename idents ';' { $$ = AST.Var $1 $2 }

-- parses a (non-empty) sequence of identifiers
idents -- ~ :: { [AST.Ident] }
  : idents_ { $$ = reverse $1 }
-- parses a (non-empty) sequence of identifiers
-- WARNING: output in reverse
idents_ -- ~ :: { [AST.Ident] }
  : ident             { $$ = [$1] }
  | idents_ ',' ident { $$ = $3:$1 }

-- parses a (non-empty) sequence of statements
stmts -- ~ :: { [AST.Stmt] }
  : stmts_ 
    { $$ = reverse $1 
    ; $1.records = $$.records 
    ; $1.symtab = $$.symtab 
    } 
-- parses a (non-empty) sequence of statements
-- WARNING: output in reverse
stmts_ -- ~ :: { [AST.Stmt] } 
  : stmt        
    { $$ = [$1] 
    ; $1.records = $$.records 
    ; $1.symtab = $$.symtab 
    }
  | stmts_ stmt 
    { $$ = $2:$1
    ; $1.records = $$.records
    ; $2.records = $$.records 
    ; $1.symtab = $$.symtab
    ; $2.symtab = $$.symtab 
    }  

-- parses a statement
stmt -- ~ :: { AST.Stmt }
  : lval '<-' expr ';'               
    { $$ = AST.Assign $1 $3 
    ; $1.records = $$.records
    ; $3.records = $$.records 
    ; $1.symtab = $$.symtab
    ; $3.symtab = $$.symtab
    ; where unless (checkAssignRef $$.symtab $1 $3 $1.etype $3.etype
                    || ($1.etype == $3.etype && AST.isAssignableT $1.etype))
                   (Left $ fmtPos (fst $2) ++ ": bad assignment types")
    } 
  | read lval ';'                    
    { $$ = AST.Read $2
    ; $2.records = $$.records 
    ; $2.symtab = $$.symtab
    ; where unless (AST.isAssignableT $2.etype)
                   (Left $ fmtPos (fst $1) ++ ": bad read type")
    }
  | write expr ';'                   
    { $$ = AST.Write $2
    ; $2.records = $$.records 
    ; $2.symtab = $$.symtab
    ; where unless (AST.isWriteableT $2.etype)
                   (Left $ fmtPos (fst $1) ++ ": bad write type")
    }
  | writeln expr ';'                 
    { $$ = AST.Writeln $2 
    ; $2.records = $$.records 
    ; $2.symtab = $$.symtab
    ; where unless (AST.isWriteableT $2.etype)
                   (Left $ fmtPos (fst $1) ++ ": bad writeln type")
    }
  | if expr then stmts else stmts fi 
    { $$ = AST.IfElse $2 $4 $6 
    ; $2.records = $$.records
    ; $4.records = $$.records
    ; $6.records = $$.records 
    ; $2.symtab = $$.symtab
    ; $4.symtab = $$.symtab
    ; $6.symtab = $$.symtab
    ; where unless (AST.isBoolT $2.etype)
                   (Left $ fmtPos (fst $1) ++ ": bad if-then-else condition type")
    }
  | if expr then stmts fi            
    { $$ = AST.If $2 $4 
    ; $2.records = $$.records
    ; $4.records = $$.records 
    ; $2.symtab = $$.symtab
    ; $4.symtab = $$.symtab 
    ; where unless (AST.isIntT $2.etype)
                   (Left $ fmtPos (fst $1) ++ ": bad if-then condition type")
    }
  | while expr do stmts od           
    { $$ = AST.While $2 $4 
    ; $2.records = $$.records
    ; $4.records = $$.records 
    ; $2.symtab = $$.symtab
    ; $4.symtab = $$.symtab 
    ; where unless (AST.isBoolT $2.etype)
                   (Left $ fmtPos (fst $1) ++ ": bad while condition type")
    }
  | call ident '(' exprs ')' ';' {- procedure calls checked later -}
    { $$ = AST.Call $2 $4 
    ; $4.records = $$.records
    ; $4.symtab = $$.symtab 
    }

-- parses an lval
lval -- ~ :: { AST.LValue }
  : ident                        
    { $$ = AST.LId $1
    ; $$.etype = ST.getProcType $$.symtab $1
    }
  | ident '.' ident              
    { $$ = AST.LField $1 $3
    ; $$.etype = ST.getFieldType $$.records (ST.getProcType $$.symtab $1) $3
    ; where unless (AST.isRecordT $ ST.getProcType $$.symtab $1)
                   (Left $ fmtPos (fst $2) 
                          ++ ": unknown record alias `" ++ $1 ++ "`")
    ; where let identT = ST.getProcType $$.symtab $1 in
            let AST.RecordT alias = identT in
            unless (AST.isRecordT identT && not ($$.etype == AST.ErrorT))
                   (Left $ fmtPos (fst $2) 
                          ++ ": unknown field `" ++ $3 ++ "` of `" ++ $1 ++ "`")
    }
  | ident '[' expr ']'           
    { $$ = AST.LInd $1 $3 
    ; $$.etype = ST.getArrayType $ ST.getProcType $$.symtab $1
    ; $3.records = $$.records
    ; $3.symtab = $$.symtab 
    ; where unless (AST.isIntT $3.etype)
                   (Left $ fmtPos (fst $2) 
                          ++ ": non-integral expression in array element index")
    ; where unless (AST.isArrayT $ ST.getProcType $$.symtab $1)
                   (Left $ fmtPos (fst $2) 
                          ++ ": unknown array alias `" ++ $1 ++ "`")
    }
  | ident '[' expr ']' '.' ident 
    { $$ = AST.LIndField $1 $3 $6 
    ; $$.etype = ST.getFieldType $$.records (ST.getArrayType $ ST.getProcType $$.symtab $1) $6
    ; $3.records = $$.records
    ; $3.symtab = $$.symtab 
    ; where unless (AST.isArrayT $ ST.getProcType $$.symtab $1)
                   (Left $ fmtPos (fst $2) 
                          ++ ": unknown array alias `" ++ $1 ++ "`")
    ; where unless ((not . AST.isArrayT $ ST.getProcType $$.symtab $1)
                    || (AST.isRecordT . ST.getArrayType $ ST.getProcType $$.symtab $1))
                   (Left $ fmtPos (fst $2) 
                          ++ ": unknown array of records `" ++ $1 ++ "`")
    ; where unless (AST.isIntT $3.etype)
                   (Left $ fmtPos (fst $2) 
                          ++ ": non-integral expression in array element index")
    }

-- parses a sequence of expressions
exprs -- ~ :: { [AST.Expr] }
  : {- empty -} { $$ = [] }
  | exprs_      
    { $$ = reverse $1 
    ; $1.records = $$.records 
    ; $1.symtab = $$.symtab 
    }
-- parses a (non-empty) sequence of expressions
-- WARNING: output in reverse
exprs_ -- ~ :: { [AST.Expr] }
  : expr            
    { $$ = [$1]
    ; $1.records = $$.records 
    ; $1.symtab = $$.symtab
    }
  | exprs_ ',' expr 
    { $$ = $3:$1
    ; $1.records = $$.records
    ; $3.records = $$.records 
    ; $1.symtab = $$.symtab
    ; $3.symtab = $$.symtab
    }

-- parses an expression
-- operator precendence is handled as defined above
expr -- ~ :: { AST.Expr }
  : expr or expr       
    { $$ = AST.BinOpExpr $$.etype AST.Op_or $1 $3 
    ; $1.records = $$.records
    ; $3.records = $$.records 
    ; $1.symtab = $$.symtab
    ; $3.symtab = $$.symtab
    ; $$.etype = AST.BoolT
    ; where unless (AST.isBoolT $1.etype)
                   (Left $ fmtPos (fst $2) 
                          ++ ": non-boolean expression in left operand of `or` expression")
    ; where unless (AST.isBoolT $3.etype)
                   (Left $ fmtPos (fst $2) 
                          ++ ": non-boolean expression in right operand of `or` expression")
    }
  | expr and expr      
    { $$ = AST.BinOpExpr $$.etype AST.Op_and $1 $3 
    ; $1.records = $$.records
    ; $3.records = $$.records 
    ; $1.symtab = $$.symtab
    ; $3.symtab = $$.symtab
    ; $$.etype = AST.BoolT
    ; where unless (AST.isBoolT $1.etype)
                   (Left $ fmtPos (fst $2) 
                          ++ ": non-boolean expression in left operand of `and` expression")
    ; where unless (AST.isBoolT $3.etype)
                   (Left $ fmtPos (fst $2) 
                          ++ ": non-boolean expression in right operand of `and` expression")
    }
  | not expr           
    { $$ = AST.UnOpExpr $$.etype AST.Op_not $2
    ; $2.records = $$.records
    ; $2.symtab = $$.symtab
    ; $$.etype = AST.BoolT
    ; where unless (AST.isBoolT $2.etype)
                   (Left $ fmtPos (fst $1) 
                          ++ ": non-boolean expression in `not` expression")
    }
  | expr '=' expr      
    { $$ = AST.BinOpExpr $$.etype AST.Op_eq $1 $3
    ; $1.records = $$.records
    ; $3.records = $$.records 
    ; $1.symtab = $$.symtab
    ; $3.symtab = $$.symtab
    ; $$.etype = AST.BoolT
    ; where unless ($1.etype == $3.etype && AST.isComparableT $1.etype)
                   (Left $ fmtPos (fst $2) 
                          ++ ": unequal expression types in `=` expression")
    }
  | expr '!=' expr     
    { $$ = AST.BinOpExpr $$.etype AST.Op_neq $1 $3
    ; $1.records = $$.records
    ; $3.records = $$.records 
    ; $1.symtab = $$.symtab
    ; $3.symtab = $$.symtab
    ; $$.etype = AST.BoolT
    ; where unless ($1.etype == $3.etype && AST.isComparableT $1.etype)
                   (Left $ fmtPos (fst $2) 
                          ++ ": unequal expression types in `!=` expression")
    }
  | expr '<' expr      
    { $$ = AST.BinOpExpr $$.etype AST.Op_lt $1 $3
    ; $1.records = $$.records
    ; $3.records = $$.records 
    ; $1.symtab = $$.symtab
    ; $3.symtab = $$.symtab
    ; $$.etype = AST.BoolT
    ; where unless ($1.etype == $3.etype && AST.isComparableT $1.etype)
                   (Left $ fmtPos (fst $2) 
                          ++ ": unequal expression types in `<` expression")
    }
  | expr '<=' expr     
    { $$ = AST.BinOpExpr $$.etype AST.Op_leq $1 $3
    ; $1.records = $$.records
    ; $3.records = $$.records 
    ; $1.symtab = $$.symtab
    ; $3.symtab = $$.symtab
    ; $$.etype = AST.BoolT
    ; where unless ($1.etype == $3.etype && AST.isComparableT $1.etype)
                   (Left $ fmtPos (fst $2) 
                          ++ ": unequal expression types in `<=` expression")
    }
  | expr '>' expr      
    { $$ = AST.BinOpExpr $$.etype AST.Op_gt $1 $3
    ; $1.records = $$.records
    ; $3.records = $$.records 
    ; $1.symtab = $$.symtab
    ; $3.symtab = $$.symtab
    ; $$.etype = AST.BoolT
    ; where unless ($1.etype == $3.etype && AST.isComparableT $1.etype)
                   (Left $ fmtPos (fst $2) 
                          ++ ": unequal expression types in `>` expression")
    }
  | expr '>=' expr     
    { $$ = AST.BinOpExpr $$.etype AST.Op_geq $1 $3
    ; $1.records = $$.records
    ; $3.records = $$.records 
    ; $1.symtab = $$.symtab
    ; $3.symtab = $$.symtab
    ; $$.etype = AST.BoolT
    ; where unless ($1.etype == $3.etype && AST.isComparableT $1.etype)
                   (Left $ fmtPos (fst $2) 
                          ++ ": unequal expression types in `>=` expression")
    }
  | expr '+' expr      
    { $$ = AST.BinOpExpr $$.etype AST.Op_add $1 $3
    ; $1.records = $$.records
    ; $3.records = $$.records 
    ; $1.symtab = $$.symtab
    ; $3.symtab = $$.symtab
    ; $$.etype = AST.IntT
    ; where unless (AST.isIntT $1.etype)
                   (Left $ fmtPos (fst $2) 
                          ++ ": non-integral expression in left operand of `+` expression")
    ; where unless (AST.isIntT $3.etype)
                   (Left $ fmtPos (fst $2) 
                          ++ ": non-integral expression in right operand of `+` expression")
    }
  | expr '-' expr      
    { $$ = AST.BinOpExpr $$.etype AST.Op_sub $1 $3
    ; $1.records = $$.records
    ; $3.records = $$.records 
    ; $1.symtab = $$.symtab
    ; $3.symtab = $$.symtab
    ; $$.etype = AST.IntT
    ; where unless (AST.isIntT $1.etype)
                   (Left $ fmtPos (fst $2) 
                          ++ ": non-integral expression in left operand of `-` expression")
    ; where unless (AST.isIntT $3.etype)
                   (Left $ fmtPos (fst $2) 
                          ++ ": non-integral expression in right operand of `-` expression")
    }
  | expr '*' expr      
    { $$ = AST.BinOpExpr $$.etype AST.Op_mul $1 $3
    ; $1.records = $$.records
    ; $3.records = $$.records 
    ; $1.symtab = $$.symtab
    ; $3.symtab = $$.symtab
    ; $$.etype = AST.IntT
    ; where unless (AST.isIntT $1.etype)
                   (Left $ fmtPos (fst $2) 
                          ++ ": non-integral expression in left operand of `*` expression")
    ; where unless (AST.isIntT $3.etype)
                   (Left $ fmtPos (fst $2) 
                          ++ ": non-integral expression in right operand of `*` expression")
    }
  | expr '/' expr      
    { $$ = AST.BinOpExpr $$.etype AST.Op_div $1 $3 
    ; $1.records = $$.records
    ; $3.records = $$.records 
    ; $1.symtab = $$.symtab
    ; $3.symtab = $$.symtab
    ; $$.etype = AST.IntT
    ; where unless (AST.isIntT $1.etype)
                   (Left $ fmtPos (fst $2) 
                          ++ ": non-integral expression in left operand of `/` expression")
    ; where unless (AST.isIntT $3.etype)
                   (Left $ fmtPos (fst $2) 
                          ++ ": non-integral expression in right operand of `/` expression")
    } 
  | '-' expr %prec NEG 
    { $$ = AST.UnOpExpr $$.etype AST.Op_neg $2
    ; $2.records = $$.records 
    ; $2.symtab = $$.symtab
    ; $$.etype = AST.IntT
    ; where unless (AST.isIntT $2.etype)
                   (Left $ fmtPos (fst $1) 
                          ++ ": non-boolean expression in unary `-` expression")
    }
  | lval               
    { $$ = AST.LVal $$.etype $1 
    ; $1.records = $$.records 
    ; $1.symtab = $$.symtab
    ; $$.etype = $1.etype
    }
  | false              
    { $$ = AST.BoolConst $$.etype False 
    ; $$.etype = AST.BoolT
    }
  | true               
    { $$ = AST.BoolConst $$.etype True 
    ; $$.etype = AST.BoolT
    }
  | number             
    { $$ = AST.IntConst $$.etype $1 
    ; $$.etype = AST.IntT
    }
  | string             
    { $$ = AST.StrConst $$.etype $1 
    ; $$.etype = AST.StrT
    }
  | '(' expr ')'       
    { $$ = $2 
    ; $2.records = $$.records 
    ; $2.symtab = $$.symtab
    ; $$.etype = $2.etype
    }

{
  
data PartialTable 
  = PartialTable (Table ST.Record) (Table ST.Array)
  deriving (Show, Eq)

fmtPos :: AlexPosn -> String
fmtPos (AlexPn _ l c) = show l ++ ":" ++ show c 

-- runParser 
-- Parses a [Lexeme], validating it is a valid sentence in the Roo CFG
-- fails if the attribute gramma catches errors  
runParser :: [Lexeme] -> Either String (AST.Program, ST.SymbolTable)
runParser ls =
  do
    out@(prog, st) <- runHappy ls
    checkAllCalls st prog -- because happy code can't check validity of calls
    return out

-- parseError
-- Called when a error is found when parsing
parseError :: [Lexeme] -> Either String a
parseError []            = Left "EOF: Unxpected parse error"
parseError ((posn, t):_) = Left $ fmtPos posn ++ ": unxpected " ++ show t

-- checkDuplicate
-- Checks if a key is found in a list of keys
-- fail with msg if duplicate is found
checkDuplicate :: String -> [String] -> String -> Either String ()
checkDuplicate key keys msg =
 unless (not $ elem key keys)
        (Left $ msg ++ " `" ++ key ++ "`")

-- checkDuplicates
-- Checks if keys contains no duplicates
-- fail with msg if duplicate is found
checkDuplicates :: (Ord a) => [a] -> String -> Either String ()
checkDuplicates xs msg = 
 unless (length (LU.nubOrd xs) == length xs) (Left msg)
 
-- noDuplicates
-- Checks if a list contains no duplicate elements
noDuplicates :: (Ord a) => [a] -> Bool
noDuplicates xs = length (LU.nubOrd xs) == length xs 

-- checkAllCalls
-- Checks if all `call` statements in a Program are correct
-- fail if invalid call is found
checkAllCalls :: SymbolTable -> AST.Program -> Either String ()
checkAllCalls st (AST.Program _ _ ps) = sequence_ $ map (checkProcCalls st) ps

-- checkAllCalls
-- Checks if all `call` statements in a ST.Procedure are correct
-- fail if invalid call is found
checkProcCalls :: SymbolTable -> AST.Procedure -> Either String ()
checkProcCalls st (AST.Procedure ident _ _ ss) =
  do
    case lookup ident procs of
      Nothing                             -> Left "unknown procedure"
      Just (ST.Procedure params _ _) -> checkAll ss
  where
    procs :: Table ST.Procedure
    procs = ST.unProcedures st
    stNoProc :: SymbolTable
    stNoProc = st { unProcedures = [] }
    checkAll :: [AST.Stmt] -> Either String ()
    checkAll ss = sequence_ $ map check ss
    check :: AST.Stmt -> Either String ()
    check (AST.If _ ss)        = checkAll ss 
    check (AST.IfElse _ ts fs) = checkAll $ ts ++ fs
    check (AST.While _ ss)     = checkAll ss 
    check (AST.Call p args)    = unless (checkCall p args)
                                        (Left "bad call")
    check _                    = return () 
    checkCall :: AST.Ident -> [AST.Expr] -> Bool
    checkCall proc args = isTableKey proc procs 
                          && length args == length callParams 
                          && and (zipWith validArg callParams args)
      where 
        callParams = map snd . ST.unParams . fromJust $ lookup proc procs
        validArg :: ST.Param -> AST.Expr -> Bool
        validArg (ST.Param t m _) a = 
          let tType = ST.getType stNoProc t in
            AST.getExprType a == ST.getType stNoProc t
            && (AST.isLVal a ||  m == AST.Ref)
               

-- entryRecord
-- Converts a AST.Record into a [Entry ST.Record]
entryRecord :: AST.Record -> Entry ST.Record
entryRecord (AST.Record fs ident) = (ident, ST.Record $ entryFields fs)

-- entryFields
-- Converts a [AST.Field] into a [Entry ST.Field]
entryFields :: [AST.Field] -> [Entry ST.Field]
entryFields fs = zipWith entryField fs [0..] 

-- entryFields
-- Converts an AST.Field into an Entry ST.Field
entryField :: AST.Field -> Int -> Entry ST.Field
entryField (AST.Field t ident) offset = (ident, ST.Field t offset)

-- entryArray
-- Converts an AST.Array into an Entry ST.Array
entryArray :: AST.Array -> Entry ST.Array
entryArray (AST.Array size t ident) = (ident, ST.Array t (fromInteger size))

-- entryProcedure
-- Converts an AST.Procedure into an Entry ST.Procedure with a PartialTable
entryProcedure :: PartialTable -> AST.Procedure -> Entry ST.Procedure
entryProcedure pt (AST.Procedure ident params vars _) =
  (ident, ST.Procedure (entryParams params) fixedOffsetVars stackSize)
  where 
    vars' :: [Entry ST.Var]
    vars' = entryVars vars
    fixedOffsetVars :: [Entry ST.Var]
    fixedOffsetVars = zipWith fixOffset vars' offsets
      where fixOffset (ident, v) off = (ident, v { unVOffset = off })
    offsets :: [Int]
    offsets = scanl (+) (length params)
              $ map (lookupSize pt . unVType . snd) vars'
    stackSize :: Int
    stackSize = last offsets

-- entryParams
-- Converts a [AST.Param] into a [Entry ST.Param]
entryParams :: [AST.Param] -> [Entry ST.Param]
entryParams params = zipWith entryParam params [0..]

-- entryParam
-- Converts an AST.Param into an Entry ST.Param
entryParam :: AST.Param -> Int -> Entry ST.Param
entryParam (AST.ParamAlias t ident) offset = 
  (ident, ST.Param (AST.Alias t) AST.Ref offset)
entryParam (AST.ParamAtomic t m ident) offset = 
  (ident, ST.Param (AST.Atomic t) m offset)

-- entryVars
-- Converts a [AST.Var] into a [Entry ST.Var]
entryVars :: [AST.Var] -> [Entry ST.Var]
entryVars vars = concatMap entryVar vars

-- entryVar
-- Converts an AST.Var into an Entry ST.Var
-- unVOffset is erroneous in output
entryVar :: AST.Var -> [Entry ST.Var]
entryVar (AST.Var t is) = map (\i -> (i, ST.Var t (-1))) is

-- lookupSize
-- looks up the size of a given AST.TypeName in a PartialTable
lookupSize :: PartialTable -> AST.TypeName -> Int
lookupSize (PartialTable rs _) (AST.Alias ident)
  | elem ident (tableKeys rs)
    = length . unFields . fromJust $ lookup ident rs
lookupSize st@(PartialTable _ as) (AST.Alias ident)
  | elem ident (tableKeys as)
    = s * lookupSize st t
  where (ST.Array t s) = fromJust $ lookup ident as
lookupSize _ _ = 1

checkAssignRef :: SymbolTable -> AST.LValue -> AST.Expr 
                              -> AST.ExprType -> AST.ExprType -> Bool
checkAssignRef st lval e lType rType
  = lType == rType
    && (AST.isArrayT lType || AST.isRecordT lType) 
    && not (null procs) && ST.isTableKey rId params
    && AST.isLVal e && AST.isLId lval
    && (AST.isLId rval || AST.isLInd rval)
    && lMode == rMode && lMode == AST.Ref
  where 
    procs = unProcedures st
    params = unParams . snd $ head procs
    rval = fromJust $ AST.getLVal e
    rId = AST.getLId rval
    lId = AST.getLId lval
    getMode v = ST.unMode . fromJust $ lookup v params 
    rMode = getMode rId
    lMode = getMode lId

}
