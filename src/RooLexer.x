{
module RooLexer 
  ( runLexer
  , Token
  , AlexPosn
  , PosnToken
  , T_and
  , T_array
  , T_boolean
  , T_call
  , T_do
  , T_else
  , T_false
  , T_fi
  , T_if
  , T_integer
  , T_not
  , T_od
  , T_or
  , T_procedure
  , T_read
  , T_record
  , T_then
  , T_true
  , T_val
  , T_while
  , T_write
  , T_writeln
  , T_lbrace
  , T_rbrace
  , T_lbracket
  , T_rbracket
  , T_lparen
  , T_rparen
  , T_comma
  , T_semi
  , T_dot
  , T_assign
  , T_eq
  , T_neq
  , T_lt
  , T_leq
  , T_gt
  , T_geq
  , T_add
  , T_sub
  , T_mul
  , T_div
  , T_string
  , T_number
  , T_ident
) where
}

%wrapper "posn"

$digit   = 0-9 
$alpha   = [a-zA-Z] 
$alnum   = [ $alpha $digit ]
@ident   = $alpha [ $alnum \_ \' ]*
@string  = \" [^ \" \t \n ]* \"
@comment = \# .*
@number  = $digit+

rules :-
  $white+    ;
  and        { cTok T_and }
  array      { cTok T_array }
  boolean    { cTok T_boolean }
  call       { cTok T_call }
  do         { cTok T_do }
  else       { cTok T_else }
  false      { cTok T_false }
  fi         { cTok T_fi }
  if         { cTok T_if }
  integer    { cTok T_integer }
  not        { cTok T_not }
  od         { cTok T_od }
  or         { cTok T_or }
  procedure  { cTok T_procedure }
  read       { cTok T_read }
  record     { cTok T_record }
  then       { cTok T_then }
  true       { cTok T_true }
  val        { cTok T_val }
  while      { cTok T_while }
  write      { cTok T_write }
  writeln    { cTok T_writeln }
  \{         { cTok T_lbrace }
  \}         { cTok T_rbrace }
  \[         { cTok T_lbracket }
  \]         { cTok T_rbracket }
  \(         { cTok T_lparen }
  \)         { cTok T_rparen }
  \,         { cTok T_comma }
  \;         { cTok T_semi }
  \.         { cTok T_dot }
  \<\-       { cTok T_assign }
  \=         { cTok T_eq }
  \!\=       { cTok T_neq }
  \<         { cTok T_lt }
  \<\=       { cTok T_leq }
  \>         { cTok T_gt }
  \>\=       { cTok T_geq }
  \+         { cTok T_add }
  \-         { cTok T_sub }
  \*         { cTok T_mul }
  \/         { cTok T_div }
  @comment   ;
  @string    { tok (T_string . tail . init)}
  @number    { tok (T_number . read) }
  @ident     { tok T_ident }

{
data Token
  = T_and
  | T_array
  | T_boolean
  | T_call
  | T_do
  | T_else
  | T_false
  | T_fi
  | T_if
  | T_integer
  | T_not
  | T_od
  | T_or
  | T_procedure
  | T_read
  | T_record
  | T_then
  | T_true
  | T_val
  | T_while
  | T_write
  | T_writeln
  | T_lbrace
  | T_rbrace
  | T_lbracket
  | T_rbracket
  | T_lparen
  | T_rparen
  | T_comma
  | T_semi
  | T_dot
  | T_assign
  | T_eq
  | T_neq 
  | T_lt
  | T_leq
  | T_gt
  | T_geq
  | T_add
  | T_sub
  | T_mul
  | T_div
  | T_string String
  | T_number Int
  | T_ident String
  deriving (Show)

type PosnToken = (AlexPosn, Token)

runLexer :: String -> [PosnToken]
runLexer = alexScanTokens

tok :: (String -> Token) -> AlexPosn -> String -> PosnToken
tok f p s = (p, f s)

cTok :: Token -> AlexPosn -> String -> PosnToken
cTok t p _ = (p, t)
}