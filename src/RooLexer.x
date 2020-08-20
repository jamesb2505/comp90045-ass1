{
module Main (runLexer, RToken, AlexPosn, main) where
}

%wrapper "posn"

$lower = [a-z]
$alnum = [a-zA-Z0-9_]

rules :-
  $white+        ;
  and            { \_ _ -> RT_and }
  array          { \_ _ -> RT_array }
  boolean        { \_ _ -> RT_boolean }
  call           { \_ _ -> RT_call }
  do             { \_ _ -> RT_do }
  else           { \_ _ -> RT_else }
  false          { \_ _ -> RT_false }
  fi             { \_ _ -> RT_fi }
  if             { \_ _ -> RT_if }
  integer        { \_ _ -> RT_integer }
  not            { \_ _ -> RT_not }
  od             { \_ _ -> RT_od }
  or             { \_ _ -> RT_or }
  procedure      { \_ _ -> RT_procedure }
  read           { \_ _ -> RT_read }
  record         { \_ _ -> RT_record }
  then           { \_ _ -> RT_then }
  true           { \_ _ -> RT_true }
  val            { \_ _ -> RT_val }
  while          { \_ _ -> RT_while }
  write          { \_ _ -> RT_write }
  writeln        { \_ _ -> RT_writeln }
  \{             { \_ _ -> RT_lbrace }
  \}             { \_ _ -> RT_rbrace }
  \[             { \_ _ -> RT_lbracket }
  \]             { \_ _ -> RT_rbracket }
  \(             { \_ _ -> RT_lparen }
  \)             { \_ _ -> RT_rparen }
  \,             { \_ _ -> RT_comma }
  \;             { \_ _ -> RT_semi }
  \.             { \_ _ -> RT_dot }
  \=             { \_ _ -> RT_eq }
  \!\=           { \_ _ -> RT_neq }
  \<             { \_ _ -> RT_lt }
  \<=            { \_ _ -> RT_lte }
  \>             { \_ _ -> RT_gt }
  \>=            { \_ _ -> RT_gte }
  \+             { \_ _ -> RT_add }
  \-             { \_ _ -> RT_sub }
  \*             { \_ _ -> RT_mul }
  \/             { \_ _ -> RT_div }
  $lower $alnum* { \_ s -> RT_ident s }

{
data RToken
  = RT_and
  | RT_array
  | RT_boolean
  | RT_call
  | RT_do
  | RT_else
  | RT_false
  | RT_fi
  | RT_if
  | RT_integer
  | RT_not
  | RT_od
  | RT_or
  | RT_procedure
  | RT_read
  | RT_record
  | RT_then
  | RT_true
  | RT_val
  | RT_while
  | RT_write
  | RT_writeln
  | RT_lbrace
  | RT_rbrace
  | RT_lbracket
  | RT_rbracket
  | RT_lparen
  | RT_rparen
  | RT_comma
  | RT_semi
  | RT_dot
  | RT_eq
  | RT_neq 
  | RT_lt
  | RT_lte
  | RT_gt
  | RT_gte
  | RT_add
  | RT_sub
  | RT_mul
  | RT_div
  | RT_neg 
  | RT_ident String
  deriving (Show)

runLexer :: String -> [RToken]
runLexer = alexScanTokens

main = getContents >>= print . show . runLexer
}