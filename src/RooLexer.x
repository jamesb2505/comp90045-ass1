{
-------------------------
-- RooLexer
--
-- Written by James Barnes, Jack Macumber, & Isitha Subasinghe
--
-- A lexer written in alex for the Roo language
-------------------------

module RooLexer 
  ( runLexer
  , Token(..)
  , AlexPosn(..)
  , Lexeme
  , fmtPos
  ) where
}

%wrapper "posn"

$digit   = 0-9 
$alpha   = [a-zA-Z] 
$alnum   = [ $alpha $digit ]
@ident   = $alpha [ $alnum \_ \' ]*
@string  = \" ([^ \" \t \n \\ ] | \\ [^ \t \n] )* \"
@comment = \# .*
@number  = $digit+

rules :-
  $white+   ;
  and       { lexemeConst T_and }
  array     { lexemeConst T_array }
  boolean   { lexemeConst T_boolean }
  call      { lexemeConst T_call }
  do        { lexemeConst T_do }
  else      { lexemeConst T_else }
  false     { lexemeConst T_false }
  fi        { lexemeConst T_fi }
  if        { lexemeConst T_if }
  integer   { lexemeConst T_integer }
  not       { lexemeConst T_not }
  od        { lexemeConst T_od }
  or        { lexemeConst T_or }
  procedure { lexemeConst T_procedure }
  read      { lexemeConst T_read }
  record    { lexemeConst T_record }
  then      { lexemeConst T_then }
  true      { lexemeConst T_true }
  val       { lexemeConst T_val }
  while     { lexemeConst T_while }
  write     { lexemeConst T_write }
  writeln   { lexemeConst T_writeln }
  \{        { lexemeConst T_lbrace }
  \}        { lexemeConst T_rbrace }
  \[        { lexemeConst T_lbracket }
  \]        { lexemeConst T_rbracket }
  \(        { lexemeConst T_lparen }
  \)        { lexemeConst T_rparen }
  \,        { lexemeConst T_comma }
  \;        { lexemeConst T_semi }
  \.        { lexemeConst T_dot }
  \<\-      { lexemeConst T_assign }
  \=        { lexemeConst T_eq }
  \!\=      { lexemeConst T_ne }
  \<        { lexemeConst T_lt }
  \<\=      { lexemeConst T_le }
  \>        { lexemeConst T_gt }
  \>\=      { lexemeConst T_ge }
  \+        { lexemeConst T_add }
  \-        { lexemeConst T_sub }
  \*        { lexemeConst T_mul }
  \/        { lexemeConst T_div }
  @string   { lexeme $ T_string . read }
  @number   { lexeme $ T_number . read }
  @ident    { lexeme T_ident }
  @comment  ;

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
  | T_ne 
  | T_lt
  | T_le
  | T_gt
  | T_ge
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
  show T_ne        = "`!=`" 
  show T_lt         = "`<`"
  show T_le        = "`<=`"
  show T_gt         = "`>`"
  show T_ge        = "`>=`"
  show T_add        = "`+`"
  show T_sub        = "`-`"
  show T_mul        = "`*`"
  show T_div        = "`/`"
  show (T_string s) = "string " ++ show s
  show (T_number n) = "number " ++ show n
  show (T_ident s)  = "identifier " ++ show s

type Lexeme = (AlexPosn, Token)

-- fmtPos
-- Formats an AlexPosn nicely
fmtPos :: AlexPosn -> String
fmtPos (AlexPn _ l c) = show l ++ ":" ++ show c 

-- lexeme
-- String -> Token, Lexeme builder
lexeme :: (String -> Token) -> AlexPosn -> String -> Lexeme
lexeme f p s = (p, f s)

-- lexemeConst
-- Constant Lexeme Builder
lexemeConst :: Token -> AlexPosn -> String -> Lexeme
lexemeConst t = lexeme (const t)

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
        AlexError (p@(AlexPn _ l c),_,_,_) 
          -> let ls = lines str in
             let err = if c > 0 && l > 0 && l <= length ls
                       then "\n" ++ (ls !! (l - 1)) ++ "\n" 
                            ++ replicate (c - 1) ' ' ++ "^ here"
                       else ""
             in Left $ fmtPos p ++ ": lexical error" ++ err
        AlexSkip inp' len     
          -> go inp' 
        AlexToken inp' len act 
          -> go inp' >>= Right . (:) (act pos $ take len s)
}