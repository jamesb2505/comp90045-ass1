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
runCodeGen prog st = 
  do
    case evalState (runExceptT $ genProgram st prog) initGen of
      l@(Left _) -> l
      Right code -> return $ foldr (.) id optimisations code

-- optimisations
-- List of optimisations to perform to generated code
optimisations :: [[OzCode] -> [OzCode]]
optimisations = [peephole, coerceLabels]

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
    -- ensure current procedure is on the top of the SymbolTable
    stmts <- repeatGen (genStmt st { ST.unProcedures = (name, proc):ps }) ss
    let nParams = length params
    -- store parameters on the stack
    let pCode = [ Oz_store i i | i <- [0..nParams - 1] ]
    return $ if stackSize > 0
             then Oz_label (ProcLabel name)
                  : Oz_push_stack_frame stackSize
                  : pCode
                 ++ (if stackSize > nParams -- initialise stack to 0
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
      -- store from right to left manually
      let unrolled = Oz_load_indirect 2 1
                   : Oz_store_indirect 0 2
                   : if size > 0
                     then Oz_int_const 2 1
                        : concat [ [ Oz_sub_offset 0 0 2 
                                   , Oz_sub_offset 1 1 2
                                   , Oz_load_indirect 3 1
                                   , Oz_store_indirect 0 3
                                   ] 
                                 | i <- [1..size - 1]
                                 ]
                     else []
      startLabel <- nextLabel
      endLabel <- nextLabel
      -- store from right to left via a loop
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
            ++ if length unrolled <= length looped -- choose shortest option
               then unrolled
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
    return $ eCode                     -- store e in r0
          ++ lCode                     -- store l in r1 (reference)
          ++ [ Oz_store_indirect 1 0 ]
genStmt st (AST.Read l) =
  do 
    putRegister 1
    lCode <- genLValue st l
    reader <- liftEither . getReadBuiltin $ ST.getLValueType st l
    return $ Oz_call_builtin reader    -- read into r0
           : lCode                     -- store l in r1 (reference)
          ++ [ Oz_store_indirect 1 0 ]
genStmt st (AST.Write e) =
  do 
    putRegister 0
    expr <- genExpr st e
    printer <- liftEither . getPrintBuiltin $ AST.getExprType e
    return $ expr                        -- store e in r0
          ++ [ Oz_call_builtin printer ]
genStmt st (AST.Writeln e) =
  do 
    putRegister 0
    expr <- genExpr st e
    printer <- liftEither . getPrintBuiltin $ AST.getExprType e
    return $ expr                              -- store e in r0
          ++ [ Oz_call_builtin printer 
             , Oz_call_builtin "print_newline"
             ]
genStmt st (AST.If e ss) =
  do 
    endLabel <- nextLabel
    putRegister 0
    eCode <- genExpr st e
    ssCode <- genStmts st ss
    return $ eCode                             -- store condition in r0
          ++ [ Oz_branch_on_false 0 endLabel ] -- go to endLabel if false
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
    return $ eCode                               -- store condition in r0
          ++ [ Oz_branch_on_false 0 falseLabel ] -- go to falseLabel if false
          ++ tsCode
          ++ [ Oz_branch_uncond endLabel         -- go to endLabel
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
           : eCode                             -- store condition in r0
          ++ [ Oz_branch_on_false 0 endLabel ] -- go to endLabel if false
          ++ ssCode                            -- do statements
          ++ [ Oz_branch_uncond startLabel     -- go to startLabel
             , Oz_label endLabel         
             ]
genStmt st@(ST.SymbolTable _ _ ps) (AST.Call name args) =
  do 
    proc@(ST.Procedure paramTable _ _) 
      <- maybeErr ("Unknown procedure `" ++ name ++ "`")
                  $ lookup name ps
    pCode <- repeatGen (\(i,p,a) -> putRegister i >> genParam p a) 
             $ zip3 [0..] (map snd paramTable) args 
    return $ pCode                        -- store parameters in r0..rn
          ++ [ Oz_call $ ProcLabel name ]
  where 
    genParam :: ST.Param -> AST.Expr -> GenState [OzCode]
    genParam (ST.Param _ AST.Ref _) (AST.LVal _ lval) 
      = genLValue st lval
    genParam _ e 
      = genExpr st e
           
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
    return $ [ if ST.isRef st alias 
               then Oz_load r offset         -- load stack slot (reference)
               else Oz_load_address r offset -- load slot address
             ] 
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
                  [ Oz_load r aOffset       -- load stack slot (reference)
                  , Oz_int_const r' fOffset -- sub field offset
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
             , Oz_mul_int r r' r              -- r  = total offset = e * size 
             , if ST.isRef st alias           -- r' = base address
               then Oz_load r' offset
               else Oz_load_address r' offset
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
             , Oz_mul_int r r' r               -- r = record offset = e * size
             , Oz_int_const r' fOffset
             , Oz_add_int r r r'               -- r = record offset + field 
             , if ST.isRef st alias 
               then Oz_load r' aOffset
               else Oz_load_address r' aOffset -- r' = base address
             , Oz_sub_offset r r' r
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

-- getLocalOffsetErr
-- Gets the local offset of an AST.Ident (variabl/parameter)
-- It is assumed that the current procedure is at the top of the 
-- procedures of the ST.SymbolTable
getLocalOffsetErr :: ST.SymbolTable -> AST.Ident -> String -> GenState Int
getLocalOffsetErr st alias err = maybeErr err $ ST.getLocalOffset st alias

-- peephole
-- Performs peephole optimisations
--  * load address, store inderect -> store
--  * load_address, load indirect  -> load
peephole :: [OzCode] -> [OzCode] 
peephole code = reverse $ peephole' [] code
  where
    peephole' res (Oz_load_address rLoad slot:Oz_store_indirect rStore rVal:cs)
      | rLoad == rStore = peephole' (Oz_store slot rVal:res) cs
    peephole' res (Oz_load_address rLoad slot:Oz_load_indirect rInd rVal:cs)
      | rLoad == rInd = peephole' (Oz_load rVal slot:res) cs
    peephole' res (c:cs) = peephole' (c:res) cs
    peephole' res [] = res

-- coerceLabels
-- Coerces concurrent labels into one, as branches to either 
-- label is equivalent
coerceLabels :: [OzCode] -> [OzCode]
coerceLabels code = coerceLabels' code code
  where
    coerceLabels' (aLabel@(Oz_label a):rest@(Oz_label b:cs)) toCoerce
      = coerceLabels' rest (filter (/= aLabel) $ map coerce toCoerce)
      where
        coerce (Oz_branch_on_true r a')
          | a == a' = Oz_branch_on_true r b
        coerce (Oz_branch_on_false r a')
          | a == a' = Oz_branch_on_false r b
        coerce (Oz_branch_uncond a')
          | a == a' = Oz_branch_uncond b
        coerce (Oz_call a')
          | a == a' = Oz_call b
        coerce code = code
    coerceLabels' (_:cs) coerced = coerceLabels' cs coerced
    coerceLabels' [] coerced = coerced
