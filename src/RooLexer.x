{
module RooLexer 
  ( runLexer
  , Token(..)
  , AlexPosn(..)
  , Lexeme
) where
}

%wrapper "posn"

$digit   = 0-9 
$alpha   = [a-zA-Z] 
$alnum   = [ $alpha $digit ]
@ident   = $alpha [ $alnum \_ \' ]*
@string  = \" ([^ \" \t \n \\ ] | \\ . )* \"
@comment = \# .*
@number  = $digit+

rules :-
  $white+   ;
  and       { cTok T_and }
  array     { cTok T_array }
  boolean   { cTok T_boolean }
  call      { cTok T_call }
  do        { cTok T_do }
  else      { cTok T_else }
  false     { cTok T_false }
  fi        { cTok T_fi }
  if        { cTok T_if }
  integer   { cTok T_integer }
  not       { cTok T_not }
  od        { cTok T_od }
  or        { cTok T_or }
  procedure { cTok T_procedure }
  read      { cTok T_read }
  record    { cTok T_record }
  then      { cTok T_then }
  true      { cTok T_true }
  val       { cTok T_val }
  while     { cTok T_while }
  write     { cTok T_write }
  writeln   { cTok T_writeln }
  \{        { cTok T_lbrace }
  \}        { cTok T_rbrace }
  \[        { cTok T_lbracket }
  \]        { cTok T_rbracket }
  \(        { cTok T_lparen }
  \)        { cTok T_rparen }
  \,        { cTok T_comma }
  \;        { cTok T_semi }
  \.        { cTok T_dot }
  \<\-      { cTok T_assign }
  \=        { cTok T_eq }
  \!\=      { cTok T_neq }
  \<        { cTok T_lt }
  \<\=      { cTok T_leq }
  \>        { cTok T_gt }
  \>\=      { cTok T_geq }
  \+        { cTok T_add }
  \-        { cTok T_sub }
  \*        { cTok T_mul }
  \/        { cTok T_div }
  @comment  ;
  @string   { tok (T_string . tail . init)}
  @number   { tok (T_number . read) }
  @ident    { tok T_ident }

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
  | T_number Integer
  | T_ident String
  deriving (Eq)

-- Show Token
-- Derived Show instance does not render well for end users.
-- This is more friendly.
instance Show Token where
  show T_and        = "`and`"
  show T_array      = "`array`"
  show T_boolean    = "`boolean`"
  show T_call       = "`call`"
  show T_do         = "`do`"
  show T_else       = "`else`"
  show T_false      = "`false`"
  show T_fi         = "`fi`"
  show T_if         = "`if`"
  show T_integer    = "`integer`"
  show T_not        = "`not`"
  show T_od         = "`od`"
  show T_or         = "`or`"
  show T_procedure  = "`procedure`"
  show T_read       = "`read`"
  show T_record     = "`record`"
  show T_then       = "`then`"
  show T_true       = "`true`"
  show T_val        = "`val`"
  show T_while      = "`while`"
  show T_write      = "`write`"
  show T_writeln    = "`writeln`"
  show T_lbrace     = "`{`"
  show T_rbrace     = "`}`"
  show T_lbracket   = "`[`"
  show T_rbracket   = "`]`"
  show T_lparen     = "`(`"
  show T_rparen     = "`)`"
  show T_comma      = "`,`"
  show T_semi       = "`;`"
  show T_dot        = "`.`"
  show T_assign     = "`<-`"
  show T_eq         = "`=`"
  show T_neq        = "`!=`" 
  show T_lt         = "`<`"
  show T_leq        = "`<=`"
  show T_gt         = "`>`"
  show T_geq        = "`>=`"
  show T_add        = "`+`"
  show T_sub        = "`-`"
  show T_mul        = "`*`"
  show T_div        = "`/`"
  show (T_string s) = "string \"" ++ s ++ "\""
  show (T_number n) = "number " ++ show n
  show (T_ident s)  = "identifier " ++ show s

type Lexeme = (AlexPosn, Token)

-- String -> Token, Token Builder
tok :: (String -> Token) -> AlexPosn -> String -> Lexeme
tok f p s = (p, f s)

-- Constant Token Builder
cTok :: Token -> AlexPosn -> String -> Lexeme
cTok t p _ = (p, t)

-- runLexer
-- Default `alexScanTokens` implementation does not gracefully handle errors,
-- instead calling `error`.
-- Returns Left on lexical error, Right on success.
runLexer :: String -> Either String [Lexeme]
runLexer str = go (alexStartPos,'\n',[],str)
  where 
    go inp@(pos,_,_,s) = 
      case alexScan inp 0 of
        AlexEOF 
          -> Right []
        AlexError ((AlexPn _ l c),_,_,_) 
          -> let ls = lines str in
             let err = if c > 0 && l > 0 && l <= length ls
                       then ":\n" ++ (ls !! (l - 1)) ++ "\n" 
                            ++ replicate (c - 1) ' ' ++ "^ here"
                       else ""
             in Left $ "Lexical error at line " ++ show l 
                       ++ ", column " ++ (show c) ++ err
        AlexSkip inp' len     
          -> go inp' 
        AlexToken inp' len act 
          -> go inp' >>= Right . (:) (act pos $ take len s)
}