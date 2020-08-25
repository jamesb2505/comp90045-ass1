{
module RooParser (
  runParser
) where

import RooLexer
import RooAST

}

%name runParser
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

program : records arrays procedures { Program $1 $2 $3 }

records : {- empty -} { [] }
        | records rec { $1 ++ [$2] }

rec : record '{' fields '}' ident ';' { Record $3 $5 }

fields : field            { [$1] }
       | fields ';' field { $1 ++ [$3] }

field : basetype ident { Field $1 $2 }

basetype : boolean { BoolType }
         | integer { IntType }

arrays : {- empty -} { [] }
       | arrays arr  { $1 ++ [$2] }

arr : array '[' number ']' typename ident ';' { Array $3 $5 $6 }

typename : basetype { Base $1 }
         | ident    { Alias $1 }

procedures : proc            { [$1] }
           | procedures proc { $1 ++ [$2] }

proc : procedure ident '(' params ')' vars '{' stmts '}' { Procedure $4 $6 $8 $2 }

params : {- empty -}       { [] }
       | param             { [$1] }
       | params1 ',' param { $1 ++ [$3] }

param : typename ident     { Param Ref $1 $2 }
      | typename val ident { Param Val $1 $3 }

params1 : param             { [$1] }
        | params1 ',' param { $1 ++ [$3] }

vars : {- empty -}  { [] }
     | vars var ';' { $1 ++ [$2] }

var : typename idents { Var $1 $2 }

idents : ident            { [$1] }
       | idents ',' ident { $1 ++ [$3] }

stmts : {- empty -} { [] }
      | stmts stmt  { $1 ++ [$2] }  

stmt : lval '<-' expr ';'               { Assign $1 $3 }
     | read lval ';'                    { Read $2 }
     | write expr ';'                   { Write $2 }
     | writeln expr ';'                 { Writeln $2 }
     | if expr then stmts else stmts fi { IfElse $2 $4 $6 }
     | if expr then stmts fi            { If $2 $4 }
     | while expr do stmts od           { While $2 $4 }
     | call ident '(' exprs ')' ';'     { Call $2 $4 }

lval : ident                        { LId $1 }
     | ident '.' ident              { LField $1 $3 }
     | ident '[' expr ']'           { LInd $1 $3 }
     | ident '[' expr ']' '.' ident { LIndField $1 $3 $6 }

exprs : {- empty -}     { [] }
      | expr            { [$1] }
      | exprs1 ',' expr { $1 ++ [$3] }

exprs1 : expr           { [$1] }
       | exprs1 ',' expr { $1 ++ [$3] }

expr : expr or expr        { BinOpExpr Op_or $1 $3 } 
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
parseError :: [PosnToken] -> a
parseError []                    = error ("Unxpected parse error")
parseError ((AlexPn _ l c, t):_) = error ("Unxpected " ++ (show $ show t) 
                                          ++ " at line " ++ show l
                                          ++ ", column " ++ show c)
}