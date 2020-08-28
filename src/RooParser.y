{
module RooParser (
  runParser
) where

import RooLexer
import RooAST
}

%name runParser
%monad { Either String } { >>= } { return }
%tokentype { PosnToken }
%error { parseError }

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

program :: { Program }
  : records arrays procedures { Program $1 $2 $3 }

records :: { [Record] }
  : records_ { reverse $1 }
records_ :: { [Record] } 
  : {- empty -}  { [] }
  | records_ rec { $2:$1 }

rec :: { Record }
  : record '{' fields '}' ident ';' { Record $3 $5 }

fields :: { [Field] }
  : fields_ { reverse $1 }
fields_ :: { [Field] }
  : field             { [$1] }
  | fields_ ';' field { $3:$1 }

field :: { Field }
  : basetype ident { Field $1 $2 }

basetype :: { BaseType }
  : boolean { BoolType }
  | integer { IntType }

arrays :: { [Array] }
  : arrays_ { reverse $1 }
arrays_ :: { [Array] }
  : {- empty -} { [] }
  | arrays_ arr { $2:$1 }

arr :: { Array }
  : array '[' number ']' typename ident ';' { Array $3 $5 $6 }

typename :: { TypeName }
  : basetype { Base $1 }
  | ident    { Alias $1 }

procedures :: { [Procedure] }
  : procedures_ { reverse $1 }

procedures_ :: { [Procedure] }
  : proc             { [$1] }
  | procedures_ proc { $2:$1 }

proc :: { Procedure }
  : procedure ident '(' params ')' vars '{' stmts '}' { Procedure $4 $6 $8 $2 }

params :: { [Param] }
  : {- empty -}       { [] }
  | param             { [$1] }
  | params_ ',' param { reverse ($3:$1) }
params_ :: { [Param] }
  : param             { [$1] }
  | params_ ',' param { $3:$1 }

param :: { Param }
  : typename ident     { Param Ref $1 $2 }
  | typename val ident { Param Val $1 $3 }

vars :: { [Var] }
  : vars_ { reverse $1 }
vars_ :: { [Var] }
  : {- empty -} { [] }
  | vars_ var   { $2:$1 }

var :: { Var }
  : typename idents ';' { Var $1 $2 }

idents :: { [Ident] }
  : idents_ { reverse $1 }
idents_ :: { [Ident] }
  : ident             { [$1] }
  | idents_ ',' ident { $3:$1 }

stmts :: { [Stmt] }
  : stmts_ { reverse $1 } 
stmts_ :: { [Stmt] } 
  : stmt        { [$1] }
  | stmts_ stmt { $2:$1 }  

stmt :: { Stmt }
  : lval '<-' expr ';'               { Assign $1 $3 }
  | read lval ';'                    { Read $2 }
  | write expr ';'                   { Write $2 }
  | writeln expr ';'                 { Writeln $2 }
  | if expr then stmts else stmts fi { IfElse $2 $4 $6 }
  | if expr then stmts fi            { If $2 $4 }
  | while expr do stmts od           { While $2 $4 }
  | call ident '(' exprs ')' ';'     { Call $2 $4 }

lval :: { LValue }
  : ident                        { LId $1 }
  | ident '.' ident              { LField $1 $3 }
  | ident '[' expr ']'           { LInd $1 $3 }
  | ident '[' expr ']' '.' ident { LIndField $1 $3 $6 }

exprs :: { [Expr] }
  : {- empty -}     { [] }
  | expr            { [$1] }
  | exprs_ ',' expr { reverse ($3:$1) }
exprs_ :: { [Expr] }
  : expr            { [$1] }
  | exprs_ ',' expr { $3:$1 }

expr :: { Expr }
  : expr or expr        { BinOpExpr Op_or $1 $3 } 
  | expr and expr       { BinOpExpr Op_and $1 $3 } 
  | not expr            { UnOpExpr Op_not $2 }
  | expr '=' expr       { BinOpExpr Op_eq $1 $3 }
  | expr '!=' expr      { BinOpExpr Op_neq $1 $3 }
  | expr '<' expr       { BinOpExpr Op_ls $1 $3 }
  | expr '<=' expr      { BinOpExpr Op_leq $1 $3 }
  | expr '>' expr       { BinOpExpr Op_gt $1 $3 }
  | expr '>=' expr      { BinOpExpr Op_geq $1 $3 }
  | expr '+' expr       { BinOpExpr Op_add $1 $3 }
  | expr '-' expr       { BinOpExpr Op_sub $1 $3 } 
  | expr '*' expr       { BinOpExpr Op_mul $1 $3 }
  | expr '/' expr       { BinOpExpr Op_div $1 $3 } 
  | '-' expr %prec NEG  { UnOpExpr Op_neg $2 }
  | lval                { Lval $1 }
  | false               { BoolConst False }
  | true                { BoolConst True }
  | number              { IntConst $1 }
  | string              { StrConst $1 }
  | '(' expr ')'        { $2 }

{
parseError :: [PosnToken] -> Either String a
parseError []                    = Left "Unxpected parse error"
parseError ((AlexPn _ l c, t):_) = Left ("Unxpected " ++ (show t) 
                                          ++ " at line " ++ (show l)
                                          ++ ", column " ++ (show c))
}