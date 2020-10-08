module RooOzCodeGen where 

import qualified RooAST as AST
import qualified RooSymbolTable as ST
import OzCode

import Control.Monad.State (State, evalState, put, get)
import Control.Monad.Except (ExceptT, runExceptT, liftEither, liftM, lift)
import Data.List (intercalate)

-- Gen
-- Data type for a generator
data Gen = Gen LabelNum RegNum
  deriving (Show, Eq)

-- GenState
-- ExceptT wrapped (Stage Gen)
-- Used to propogate errors through a successive generators
type GenState = ExceptT String (State Gen)

-- nextLabel
-- Returns the next Label for a generator, post-incrementing the counter
nextLabel :: GenState Label
nextLabel =
  do
    Gen ln rn <- lift get
    lift . put $ Gen (ln + 1) rn
    return $ BranchLabel ln

-- getRegister
-- Returns the current RegNum for a generator
getRegister :: GenState RegNum
getRegister = 
  do
    Gen _ rn <- lift get
    return rn

-- nextRegister
-- Returns the next RegNum for a generator, post-incrementing the counter
nextRegister :: GenState RegNum
nextRegister = 
  do
    Gen ln rn <- lift get
    lift . put . Gen ln $ rn + 1
    return rn

-- putRegister 
-- Sets the RegNum inside the State
putRegister :: RegNum -> GenState ()
putRegister r =
  do
    Gen ln _ <- lift get
    lift . put $ Gen ln r

-- maybeErr
-- Lifts a Maybe a into a GenState, using the input String for the 
-- Left value on Nothing
maybeErr :: String -> Maybe a -> GenState a
maybeErr a Nothing  = liftEither $ Left a
maybeErr _ (Just b) = liftEither $ Right b

-- initGen
-- Initial Gen for a generator
initGen :: Gen
initGen = Gen 0 0

-- runCodeGen
-- Generates a list of OzCode of a given AST.Program
-- Right on success, Left on error
runCodeGen :: AST.Program -> ST.SymbolTable -> Either String [OzCode]
runCodeGen prog st = evalState (runExceptT $ genProgram st prog) initGen

-- repeatGen
-- Repeats a generator over a list, concatenating the results
repeatGen :: (a -> GenState [OzCode]) -> [a] -> GenState [OzCode]
repeatGen gen = liftM concat . mapM gen
    
-- genProgram
-- Generates a [OzCode] for a given AST.Program
genProgram :: ST.SymbolTable -> AST.Program -> GenState [OzCode]
genProgram st (AST.Program _ _ ps) =
  do 
    procs <- repeatGen (genProcedure st) ps
    return $ [ Oz_call $ ProcLabel "main" 
             , Oz_halt 
             ]
          ++ procs

-- genProcedure
-- Generates a [OzCode] for a given AST.Program
genProcedure :: ST.SymbolTable -> AST.Procedure -> GenState [OzCode]
genProcedure st@(ST.SymbolTable _ _ ps) (AST.Procedure name _ _ ss) = 
  do 
    proc@(ST.Procedure params _ stackSize) 
      <- maybeErr ("Unknown procedure `" ++ name ++ "`") 
                  $ lookup name ps
    let nParams = length params
    let pCode = [ Oz_store i i | i <- [0..nParams - 1] ]
    -- ensure current procedure is on the top of the SymbolTable
    stmts <- repeatGen (genStmt st { ST.unProcedures = (name, proc):ps }) ss
    return $ if stackSize > 0
            then Oz_label (ProcLabel name)
                 : Oz_push_stack_frame stackSize
                 : pCode
                ++ (if stackSize > nParams 
                    then [Oz_int_const 0 0]
                      ++ [ Oz_store (i + nParams) 0 
                         | i <- [0..stackSize - nParams - 1] 
                         ]
                    else [])
                ++ stmts 
                ++ [ Oz_pop_stack_frame stackSize
                   , Oz_return
                   ]
            else Oz_label (ProcLabel name) 
                 : pCode
                ++ stmts 
                ++ [ Oz_return ]

-- genStmt
-- Generates a [OzCode] for a given AST.Stmt
-- It is assumed that the current procedure is at the top of the 
-- procedures of the ST.SymbolTable
genStmt :: ST.SymbolTable -> AST.Stmt -> GenState [OzCode]
genStmt st (AST.Assign lval (AST.LVal _ rval))
  | ST.isRef st lAlias && ST.isRef st rAlias
  = do 
      lOffset <- getOffset lAlias
      rOffset <- getOffset rAlias
      putRegister 0
      lCode <- genLValue st lval
      putRegister 1
      rCode <- genLValue st rval
      let size = ST.lookupTotalSize st . AST.getTypeName 
                 $ ST.getLValueType st rval
      let naive = concat [ [ Oz_int_const 2 i
                           , Oz_sub_offset 3 1 2
                           , Oz_sub_offset 2 0 2
                           , Oz_load_indirect 3 3
                           , Oz_store_indirect 2 3
                           ] 
                         | i <- [0..size - 1]
                         ]
      startLabel <- nextLabel
      endLabel <- nextLabel
      let looped = [ Oz_int_const 2 0
                   , Oz_int_const 3 size
                   , Oz_int_const 4 1
                   , Oz_label startLabel
                   , Oz_cmp_lt_int 5 2 3
                   , Oz_branch_on_false 5 endLabel -- while r2 < r3 (= size)
                   , Oz_sub_offset 6 1 2 
                   , Oz_sub_offset 5 0 2
                   , Oz_load_indirect 6 6
                   , Oz_store_indirect 5 6         -- lval + r2 <- rval + r2
                   , Oz_add_int 2 2 4              -- r2 <- r2 + r4 (= 1)
                   , Oz_branch_uncond startLabel
                   , Oz_label endLabel
                   ]
      return $ lCode
            ++ rCode 
            ++ if length naive <= length looped -- choose shortest option
               then naive
               else looped
  where 
    lAlias = AST.getLId lval
    rAlias = AST.getLId rval
    getOffset alias 
      = getLocalOffsetErr st alias ("Unknown variable `" ++ alias ++ "`")
genStmt st (AST.Assign l e) =
  do 
    putRegister 0
    eCode <- genExpr st e
    putRegister 1
    lCode <- genLValue st l
    return $ eCode 
          ++ lCode
          ++ [ Oz_store_indirect 1 0 ]
genStmt st (AST.Read l) =
  do 
    putRegister 1
    lCode <- genLValue st l
    reader <- liftEither . getReadBuiltin $ ST.getLValueType st l
    return $ Oz_call_builtin reader
           : lCode
          ++ [ Oz_store_indirect 1 0 ]
genStmt st (AST.Write e) =
  do 
    putRegister 0
    expr <- genExpr st e
    printer <- liftEither . getPrintBuiltin $ AST.getExprType e
    return $ expr 
          ++ [ Oz_call_builtin printer ]
genStmt st (AST.Writeln e) =
  do 
    putRegister 0
    expr <- genExpr st e
    printer <- liftEither . getPrintBuiltin $ AST.getExprType e
    return $ expr
          ++ [ Oz_call_builtin printer 
             , Oz_call_builtin "print_newline"
             ]
genStmt st (AST.If e ss) =
  do 
    endLabel <- nextLabel
    putRegister 0
    eCode <- genExpr st e
    ssCode <- genStmts st ss
    return $ eCode
          ++ [ Oz_branch_on_false 0 endLabel ]
          ++ ssCode
          ++ [ Oz_label endLabel ]
genStmt st (AST.IfElse e ts fs) =
  do 
    falseLabel <- nextLabel
    endLabel <- nextLabel
    putRegister 0
    eCode <- genExpr st e
    tsCode <- genStmts st ts
    fsCode <- genStmts st fs
    return $ eCode
          ++ [ Oz_branch_on_false 0 falseLabel ]
          ++ tsCode
          ++ [ Oz_branch_uncond endLabel
             , Oz_label falseLabel 
             ]
          ++ fsCode
          ++ [ Oz_label endLabel ]
genStmt st (AST.While e ss) =
  do 
    startLabel <- nextLabel
    endLabel <- nextLabel
    putRegister 0
    eCode <- genExpr st e
    ssCode <- genStmts st ss
    return $ Oz_label startLabel
           : eCode
          ++ [ Oz_branch_on_false 0 endLabel ]
          ++ ssCode
          ++ [ Oz_branch_uncond startLabel
             , Oz_label endLabel
             ]
genStmt st@(ST.SymbolTable _ _ ps) (AST.Call name args) =
  do 
    proc@(ST.Procedure paramTable _ _) 
      <- maybeErr ("Unknown procedure `" ++ name ++ "`")
                  $ lookup name ps
    pCode <- repeatGen genParam' $ zip3 [0..] (map snd paramTable) args 
    return $ pCode
          ++ [ Oz_call $ ProcLabel name ]
  where genParam' (r, p, a) = genParam st r p a
           
-- genStmts
-- Generates a [OzCode] for a given [AST.Program]
-- It is assumed that the current procedure is at the top of the 
-- procedures of the ST.SymbolTable
genStmts :: ST.SymbolTable -> [AST.Stmt] -> GenState [OzCode]
genStmts st ss = repeatGen (genStmt st) ss

-- genLValue
-- Generates a [OzCode] for a given AST.LValue
-- It is assumed that the current procedure is at the top of the 
-- procedures of the ST.SymbolTable
genLValue :: ST.SymbolTable -> AST.LValue -> GenState [OzCode]
genLValue st (AST.LId alias) = 
  do 
    offset <- getLocalOffsetErr st alias 
                ("Unknown parameter/variable `" ++ alias ++ "`")
    r <- nextRegister
    if ST.isRef st alias 
    then return $ [ Oz_load r offset ]
    else return $ [ Oz_load_address r offset ]
genLValue st (AST.LField alias field) = 
  do 
    aOffset <- getLocalOffsetErr st alias 
                 ("Unknown parameter/variable `" ++ alias ++ "`")
    rAlias <- liftEither . getRecord $ ST.getProcType st alias
    record <- maybeErr ("Unknown record type `" ++ rAlias ++ "`")
              $ ST.getRecord st rAlias
    fOffset <- maybeErr ("Unknown field `" ++ field 
                         ++ "` of `" ++ rAlias ++"`")
               $ ST.unFOffset <$> ST.getField record field
    r <- nextRegister
    return $ if ST.isRef st alias 
             then let r' = r + 1 in
                  [ Oz_load r aOffset
                  , Oz_int_const r' fOffset
                  , Oz_sub_offset r r r'
                  ]
             else [ Oz_load_address r $ aOffset + fOffset ]
  where 
    getRecord (AST.RecordT rAlias) 
      = Right rAlias
    getRecord _                    
      = Left $ "Incorrect type for `" ++ alias ++ "`" 
genLValue st (AST.LInd alias e) =
  do 
    offset <- getLocalOffsetErr st alias 
                ("Unknown parameter/variable `" ++ alias ++ "`")
    let AST.ArrayT aAlias size = ST.getProcType st alias
    let size = ST.lookupElementSize st $ AST.Alias aAlias
    r <- getRegister
    eCode <- genExpr st e
    let r' = r + 1
    return $ eCode
          ++ [ Oz_int_const r' size
             , Oz_mul_int r r' r
             , (if ST.isRef st alias 
                then Oz_load else Oz_load_address) r' offset
             , Oz_sub_offset r r' r
             ]
genLValue st (AST.LIndField alias e field) =
  do 
    aOffset <- getLocalOffsetErr st alias 
                 ("Unknown parameter/variable `" ++ alias ++ "`")
    (aAlias, rAlias) <- liftEither . getAliases $ ST.getProcType st alias
    record <- maybeErr ("Unknown record type `" ++ rAlias ++ "`")
              $ ST.getRecord st rAlias
    fOffset <- maybeErr ("Unknown field `" ++ field 
                         ++ "` of `" ++ rAlias ++"`")
               $ ST.unFOffset <$> ST.getField record field
    let size = ST.lookupElementSize st $ AST.Alias aAlias
    r <- getRegister
    eCode <- genExpr st e
    let r' = r + 1
    return $ eCode
          ++ [ Oz_int_const r' size
             , Oz_mul_int r r' r
             , (if ST.isRef st alias 
                then Oz_load else Oz_load_address) r' aOffset
             , Oz_sub_offset r r' r
             , Oz_int_const r' fOffset
             , Oz_sub_offset r r r'
             ]
  where 
    getAliases (AST.ArrayT aAlias (AST.RecordT rAlias)) 
      = Right (aAlias, rAlias)
    getAliases _                    
      = Left $ "Incorrect type for `" ++ alias ++ "`" 
    
-- genExpr
-- Generates a [OzCode] for a given AST.Expr
-- It is assumed that the current procedure is at the top of the 
-- procedures of the ST.SymbolTable
genExpr :: ST.SymbolTable -> AST.Expr -> GenState [OzCode]
genExpr st (AST.LVal _ lval) = 
  do 
    r <- getRegister
    lCode <- genLValue st lval
    return $ lCode
          ++ [ Oz_load_indirect r r ]
genExpr _ (AST.BoolConst _ b) =
  do 
    r <- nextRegister
    return $ [ Oz_int_const r $ if b then 1 else 0 ]
genExpr _ (AST.IntConst _ i) =
  do 
    r <- nextRegister
    return $ [ Oz_int_const r $ fromInteger i ]
genExpr _ (AST.StrConst _ s) =
  do 
    r <- nextRegister
    return $ [ Oz_string_const r s ]
genExpr st (AST.BinOpExpr _ op a b) =
  do 
    r <- getRegister
    aCode <- genExpr st a
    bCode <- genExpr st b
    putRegister $ r + 1
    r' <- getRegister
    return $ aCode
          ++ bCode
          ++ [ getBinOpCode op r r r' ]
genExpr st (AST.UnOpExpr _ op a) =
  do 
    r <- getRegister
    aCode <- genExpr st a
    putRegister $ r + 1
    return $ aCode
          ++ [ getUnOpCode op r r ]

-- genParam
-- Generates a [OzCode] for a given ST.Param and AST.Expr at a given RegNum
-- It is assumed that the current procedure is at the top of the 
-- procedures of the ST.SymbolTable
genParam :: ST.SymbolTable -> RegNum -> ST.Param -> AST.Expr 
         -> GenState [OzCode]
genParam st r (ST.Param _ AST.Ref _) (AST.LVal _ lval)
 = putRegister r >> genLValue st lval
genParam st r _ e 
 = putRegister r >> genExpr st e

-- getLocalOffsetErr
-- Gets the local offset of an AST.Ident (variabl/parameter)
-- It is assumed that the current procedure is at the top of the 
-- procedures of the ST.SymbolTable
getLocalOffsetErr :: ST.SymbolTable -> AST.Ident -> String -> GenState Int
getLocalOffsetErr st alias err = maybeErr err $ ST.getLocalOffset st alias
