{
module RooParser (
  runParser
) where

import RooSymbolTable as Sym
import RooLexer
import qualified RooAST as AST

import Control.Monad
}

%name runParser program
%tokentype { Lexeme }
%monad { Either String }
%error { parseError }

%attributetype { AttrTable a }
%attribute value   { a }
%attribute records { Sym.Table Sym.Record }
%attribute arrays  { Sym.Table Sym.Array }
%attribute procs   { Sym.Table Sym.Procedure }
%attribute type_   { Either () AST.TypeName }

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
    ; where unless (Sym.noDuplicates (Sym.tableKeys ($$.records)
                                      ++ Sym.tableKeys ($$.arrays)))
                   (fail "duplicate record/array alias")
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
    }

rec -- ~ :: { AST.Record }
  : record '{' fields '}' ident ';' 
    { $$ = AST.Record $3 $5
    ; $$.records = [ entryRecord $$ ]
    ; where unless (Sym.noDuplicates (map (\(AST.Field _ i) -> i) $3 ))
                   (fail $ "duplicate field in record `" ++ $5 ++ "`")
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
  : arrays_ { $$ = reverse $1
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
    ; $$.procs = []
    }
  | procedures_ proc 
    { $$ = $2:$1
    ; $1.records = $$.records
    ; $2.records = $$.records 
    ; $1.arrays = $$.arrays
    ; $2.arrays = $$.arrays 
    ; $$.procs = $2.procs ++ $1.procs
    }

proc -- ~ :: { AST.Procedure }
  : procedure ident '(' params ')' vars '{' stmts '}' 
    { $$ = AST.Procedure $2 $4 $6 $8 
    ; $$.procs = [ entryProcedure (PartialTable $$.records $$.arrays) $$ ]
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
  : stmts_ { $$ = reverse $1 } 
stmts_ -- ~ :: { [AST.Stmt] } 
  : stmt        { $$ = [$1] }
  | stmts_ stmt { $$ = $2:$1 }  

stmt -- ~ :: { AST.Stmt }
  : lval '<-' expr ';'               { $$ = AST.Assign $1 $3 }
  | read lval ';'                    { $$ = AST.Read $2 }
  | write expr ';'                   { $$ = AST.Write $2 }
  | writeln expr ';'                 { $$ = AST.Writeln $2 }
  | if expr then stmts else stmts fi { $$ = AST.IfElse $2 $4 $6 }
  | if expr then stmts fi            { $$ = AST.If $2 $4 }
  | while expr do stmts od           { $$ = AST.While $2 $4 }
  | call ident '(' exprs ')' ';'     { $$ = AST.Call $2 $4 }

lval -- ~ :: { AST.LValue }
  : ident                        { $$ = AST.LId $1 }
  | ident '.' ident              { $$ = AST.LField $1 $3 }
  | ident '[' expr ']'           { $$ = AST.LInd $1 $3 }
  | ident '[' expr ']' '.' ident { $$ = AST.LIndField $1 $3 $6 }

exprs -- ~ :: { [AST.Expr] }
  : {- empty -} { $$ = [] }
  | exprs_      { $$ = reverse $1 }
exprs_ -- ~ :: { [AST.Expr] }
  : expr            { $$ = [$1] }
  | exprs_ ',' expr { $$ = $3:$1 }

-- operator precendence is handled as defined above
expr -- ~ :: { AST.Expr }
  : expr or expr       { $$ = AST.BinOpExpr AST.Op_or $1 $3 } 
  | expr and expr      { $$ = AST.BinOpExpr AST.Op_and $1 $3 } 
  | not expr           { $$ = AST.UnOpExpr AST.Op_not $2 }
  | expr '=' expr      { $$ = AST.BinOpExpr AST.Op_eq $1 $3 }
  | expr '!=' expr     { $$ = AST.BinOpExpr AST.Op_neq $1 $3 }
  | expr '<' expr      { $$ = AST.BinOpExpr AST.Op_lt $1 $3 }
  | expr '<=' expr     { $$ = AST.BinOpExpr AST.Op_leq $1 $3 }
  | expr '>' expr      { $$ = AST.BinOpExpr AST.Op_gt $1 $3 }
  | expr '>=' expr     { $$ = AST.BinOpExpr AST.Op_geq $1 $3 }
  | expr '+' expr      { $$ = AST.BinOpExpr AST.Op_add $1 $3 }
  | expr '-' expr      { $$ = AST.BinOpExpr AST.Op_sub $1 $3 } 
  | expr '*' expr      { $$ = AST.BinOpExpr AST.Op_mul $1 $3 }
  | expr '/' expr      { $$ = AST.BinOpExpr AST.Op_div $1 $3 } 
  | '-' expr %prec NEG { $$ = AST.UnOpExpr AST.Op_neg $2 }
  | lval               { $$ = AST.LVal $1 }
  | false              { $$ = AST.BoolConst False }
  | true               { $$ = AST.BoolConst True }
  | number             { $$ = AST.IntConst $1 }
  | string             { $$ = AST.StrConst $1 }
  | '(' expr ')'       { $$ = $2 }

{

str = "record {integer a; integer b} rec; array [10] integer arr; procedure main() { i <- i;}"

parseError :: [Lexeme] -> Either String a
parseError []                    = Left "Unxpected parse error at end of file"
parseError ((AlexPn _ l c, t):_) = Left $ "Unxpected " ++ show t 
                                          ++ " at line " ++ show l
                                          ++ ", column " ++ show c
}