{
module Grammar where
import RooLexer
import RooAST

}

%name parse
%tokentype { PosnToken }
%error { parseError }

%token
      and               { (pos, T_and) }
      array             { (pos, T_array) }
      boolean           { (pos, T_boolean) }
      call              { (pos, T_call) }
      do                { (pos, T_do) }
      else              { (pos, T_else) }
      false             { (pos, T_false) }
      fi                { (pos, T_fi) }
      if                { (pos, T_if) } 
      integer           { (pos, T_integer) }
      not               { (pos, T_not) }
      od                { (pos, T_od) }
      or                { (pos, T_or) }
      procedure         { (pos, T_procedure) }
      read              { (pos, T_read) }
      record            { (pos, T_record) }
      then              { (pos, T_then) }
      true              { (pos, T_true) }
      val               { (pos, T_val) }
      while             { (pos, T_while) }
      write             { (pos, T_write) }
      writeln           { (pos, T_writeln) }
      '{'               { (pos, T_lbrace) }
      '}'               { (pos, T_rbrace) }
      '['               { (pos, T_lbracket) }
      ']'               { (pos, T_rbracket) }
      '('               { (pos, T_lparen) }
      ')'               { (pos, T_rparen) }
      ','               { (pos, T_comma) }
      ';'               { (pos, T_semi) }
      '.'               { (pos, T_dot) }
      '='               { (pos, T_eq) }
      '!='              { (pos, T_neq) }
      '<'               { (pos, T_lt) }
      '<='              { (pos, T_leq) }
      assign            { (pos, T_assign) }
      '>'               { (pos, T_gt) }
      '>='              { (pos, T_geq) }
      '+'               { (pos, T_add) }
      '-'               { (pos, T_sub) }
      '*'               { (pos, T_mul) }
      '/'               { (pos, T_div) }
      string            { (pos, T_string $$) }
      number            { (pos, T_number $$) }
      ident             { (pos, T_ident $$) }

%nonassoc '>' '<' '<=' '>=' '='
%left '+' '-'
%left '*' '/'
%left NEG

%%

Stmt  : LValue assign Expr ';'            { Assign $1 $3 }
      | read LValue ';'                   { Read $2 }
      | write Expr ';'                    { Write $2 }
      | writeln Expr ';'                  { Writeln $2 }
      | if Expr then stmts else stmts fi  { IfElse $2 $4 $6 }
      | if Expr then stmts fi             { If $2 $4 }
      | while Expr do stmts od            { While $2 $4 }
      | call ident '(' sepExprs ')' ';'   { Call $2 $4 }

LValue      : ident                         { LId $1 }
            | ident '.' identr              { LField $1 $3 }
            | ident '[' Expr ']'            { LInd $1 $3 }
            | ident '[' Expr ']' '.' identr { LIndField $1 $3 $6 }

Expr  : LValue          { Lval $1 }    
      | false           { BoolConst False }
      | true            { BoolConst True }
      | number          { IntConst $1 }
      | string          { StrConst $1 }
      | '(' Expr ')'    { $2 }


Decl : TypeName ident ';'  { Decl $1 $2 }

TypeName    : boolean   { BoolType }
            | integer   { IntType }
            | ident     { TypeAlias $1 }

ldecl : Decl  { [$1] }
      | ldecl Decl { $2 : $1 }

identr : ident    { $1 }

stmts : {- empty -}     { [] }
      | Stmt            { [$1] }
      | stmts Stmt      { $2 : $1 }

sepExprs    : {- empty -}           { [] }
            | Expr                  { [$1] }
            | sepExprs ',' Expr     { $3 : $1 }


ArrayDef : array '[' number ']' Decl { Array $3 $5 }

RecordDef : record '{' ldecl '}' ident ';' { Record $3 $5 }

{

parseError :: [PosnToken] -> a
parseError x = error ("Not expecting " ++ token)
                  where
                        token = show x
}