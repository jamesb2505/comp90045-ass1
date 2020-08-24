{-# OPTIONS_GHC -w #-}
module Grammar where
import RooLexer
import RooAST
import qualified Data.Array as Happy_Data_Array
import qualified Data.Bits as Bits
import Control.Applicative(Applicative(..))
import Control.Monad (ap)

-- parser produced by Happy Version 1.19.11

data HappyAbsSyn t4 t5 t6 t7 t8 t9 t10 t11 t12 t13 t14
	= HappyTerminal (PosnToken)
	| HappyErrorToken Int
	| HappyAbsSyn4 t4
	| HappyAbsSyn5 t5
	| HappyAbsSyn6 t6
	| HappyAbsSyn7 t7
	| HappyAbsSyn8 t8
	| HappyAbsSyn9 t9
	| HappyAbsSyn10 t10
	| HappyAbsSyn11 t11
	| HappyAbsSyn12 t12
	| HappyAbsSyn13 t13
	| HappyAbsSyn14 t14

happyExpList :: Happy_Data_Array.Array Int Int
happyExpList = Happy_Data_Array.listArray (0,140) ([0,4162,14,1024,0,0,0,64,0,0,512,0,0,1024,1,0,0,0,0,0,0,0,64,4096,128,1,7,0,0,16384,0,32784,256,1792,0,2049,16,112,4096,128,1,7,0,0,0,0,0,2048,0,0,0,0,0,0,0,0,0,256,4104,28672,0,0,0,0,0,0,0,0,0,0,8,0,64,0,0,0,0,2048,0,0,1024,0,0,0,0,1,0,256,4104,28672,0,0,0,1024,0,2049,16,112,0,0,8,0,0,0,0,0,0,0,0,0,0,8,0,4096,128,1,7,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,8192,0,0,0,0,0,0,0,0,0,0,0,0,0,9248,225,16384,0,4202,14,1024,0,0,0,0,0,0,6,0,0,0,1,0,0,0,0,0,0,0,64,0,0,8,0,256,4104,28672,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,4194,14,1024,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0
	])

{-# NOINLINE happyExpListPerState #-}
happyExpListPerState st =
    token_strs_expected
  where token_strs = ["error","%dummy","%start_parse","Stmt","LValue","Expr","Decl","TypeName","ldecl","identr","stmts","sepExprs","ArrayDef","RecordDef","and","array","boolean","call","do","else","false","fi","if","integer","not","od","or","procedure","read","record","then","true","val","while","write","writeln","'{'","'}'","'['","']'","'('","')'","','","';'","'.'","'='","'!='","'<'","'<='","assign","'>'","'>='","'+'","'-'","'*'","'/'","string","number","ident","%eof"]
        bit_start = st * 60
        bit_end = (st + 1) * 60
        read_bit = readArrayBit happyExpList
        bits = map read_bit [bit_start..bit_end - 1]
        bits_indexed = zip bits [0..59]
        token_strs_expected = concatMap f bits_indexed
        f (False, _) = []
        f (True, nr) = [token_strs !! nr]

action_0 (18) = happyShift action_5
action_0 (23) = happyShift action_6
action_0 (29) = happyShift action_7
action_0 (34) = happyShift action_8
action_0 (35) = happyShift action_9
action_0 (36) = happyShift action_10
action_0 (59) = happyShift action_3
action_0 (4) = happyGoto action_4
action_0 (5) = happyGoto action_2
action_0 _ = happyFail (happyExpListPerState 0)

action_1 (59) = happyShift action_3
action_1 (5) = happyGoto action_2
action_1 _ = happyFail (happyExpListPerState 1)

action_2 (50) = happyShift action_25
action_2 _ = happyFail (happyExpListPerState 2)

action_3 (39) = happyShift action_23
action_3 (45) = happyShift action_24
action_3 _ = happyReduce_9

action_4 (60) = happyAccept
action_4 _ = happyFail (happyExpListPerState 4)

action_5 (59) = happyShift action_22
action_5 _ = happyFail (happyExpListPerState 5)

action_6 (21) = happyShift action_13
action_6 (32) = happyShift action_14
action_6 (41) = happyShift action_15
action_6 (57) = happyShift action_16
action_6 (58) = happyShift action_17
action_6 (59) = happyShift action_3
action_6 (5) = happyGoto action_11
action_6 (6) = happyGoto action_21
action_6 _ = happyFail (happyExpListPerState 6)

action_7 (59) = happyShift action_3
action_7 (5) = happyGoto action_20
action_7 _ = happyFail (happyExpListPerState 7)

action_8 (21) = happyShift action_13
action_8 (32) = happyShift action_14
action_8 (41) = happyShift action_15
action_8 (57) = happyShift action_16
action_8 (58) = happyShift action_17
action_8 (59) = happyShift action_3
action_8 (5) = happyGoto action_11
action_8 (6) = happyGoto action_19
action_8 _ = happyFail (happyExpListPerState 8)

action_9 (21) = happyShift action_13
action_9 (32) = happyShift action_14
action_9 (41) = happyShift action_15
action_9 (57) = happyShift action_16
action_9 (58) = happyShift action_17
action_9 (59) = happyShift action_3
action_9 (5) = happyGoto action_11
action_9 (6) = happyGoto action_18
action_9 _ = happyFail (happyExpListPerState 9)

action_10 (21) = happyShift action_13
action_10 (32) = happyShift action_14
action_10 (41) = happyShift action_15
action_10 (57) = happyShift action_16
action_10 (58) = happyShift action_17
action_10 (59) = happyShift action_3
action_10 (5) = happyGoto action_11
action_10 (6) = happyGoto action_12
action_10 _ = happyFail (happyExpListPerState 10)

action_11 _ = happyReduce_13

action_12 (44) = happyShift action_36
action_12 _ = happyFail (happyExpListPerState 12)

action_13 _ = happyReduce_14

action_14 _ = happyReduce_15

action_15 (21) = happyShift action_13
action_15 (32) = happyShift action_14
action_15 (41) = happyShift action_15
action_15 (57) = happyShift action_16
action_15 (58) = happyShift action_17
action_15 (59) = happyShift action_3
action_15 (5) = happyGoto action_11
action_15 (6) = happyGoto action_35
action_15 _ = happyFail (happyExpListPerState 15)

action_16 _ = happyReduce_17

action_17 _ = happyReduce_16

action_18 (44) = happyShift action_34
action_18 _ = happyFail (happyExpListPerState 18)

action_19 (19) = happyShift action_33
action_19 _ = happyFail (happyExpListPerState 19)

action_20 (44) = happyShift action_32
action_20 _ = happyFail (happyExpListPerState 20)

action_21 (31) = happyShift action_31
action_21 _ = happyFail (happyExpListPerState 21)

action_22 (41) = happyShift action_30
action_22 _ = happyFail (happyExpListPerState 22)

action_23 (21) = happyShift action_13
action_23 (32) = happyShift action_14
action_23 (41) = happyShift action_15
action_23 (57) = happyShift action_16
action_23 (58) = happyShift action_17
action_23 (59) = happyShift action_3
action_23 (5) = happyGoto action_11
action_23 (6) = happyGoto action_29
action_23 _ = happyFail (happyExpListPerState 23)

action_24 (59) = happyShift action_28
action_24 (10) = happyGoto action_27
action_24 _ = happyFail (happyExpListPerState 24)

action_25 (21) = happyShift action_13
action_25 (32) = happyShift action_14
action_25 (41) = happyShift action_15
action_25 (57) = happyShift action_16
action_25 (58) = happyShift action_17
action_25 (59) = happyShift action_3
action_25 (5) = happyGoto action_11
action_25 (6) = happyGoto action_26
action_25 _ = happyFail (happyExpListPerState 25)

action_26 (44) = happyShift action_44
action_26 _ = happyFail (happyExpListPerState 26)

action_27 _ = happyReduce_10

action_28 _ = happyReduce_25

action_29 (40) = happyShift action_43
action_29 _ = happyFail (happyExpListPerState 29)

action_30 (21) = happyShift action_13
action_30 (32) = happyShift action_14
action_30 (41) = happyShift action_15
action_30 (57) = happyShift action_16
action_30 (58) = happyShift action_17
action_30 (59) = happyShift action_3
action_30 (5) = happyGoto action_11
action_30 (6) = happyGoto action_41
action_30 (12) = happyGoto action_42
action_30 _ = happyReduce_29

action_31 (18) = happyShift action_5
action_31 (23) = happyShift action_6
action_31 (29) = happyShift action_7
action_31 (34) = happyShift action_8
action_31 (35) = happyShift action_9
action_31 (36) = happyShift action_10
action_31 (59) = happyShift action_3
action_31 (4) = happyGoto action_38
action_31 (5) = happyGoto action_2
action_31 (11) = happyGoto action_40
action_31 _ = happyReduce_26

action_32 _ = happyReduce_2

action_33 (18) = happyShift action_5
action_33 (23) = happyShift action_6
action_33 (29) = happyShift action_7
action_33 (34) = happyShift action_8
action_33 (35) = happyShift action_9
action_33 (36) = happyShift action_10
action_33 (59) = happyShift action_3
action_33 (4) = happyGoto action_38
action_33 (5) = happyGoto action_2
action_33 (11) = happyGoto action_39
action_33 _ = happyReduce_26

action_34 _ = happyReduce_3

action_35 (42) = happyShift action_37
action_35 _ = happyFail (happyExpListPerState 35)

action_36 _ = happyReduce_4

action_37 _ = happyReduce_18

action_38 _ = happyReduce_27

action_39 (18) = happyShift action_5
action_39 (23) = happyShift action_6
action_39 (26) = happyShift action_51
action_39 (29) = happyShift action_7
action_39 (34) = happyShift action_8
action_39 (35) = happyShift action_9
action_39 (36) = happyShift action_10
action_39 (59) = happyShift action_3
action_39 (4) = happyGoto action_48
action_39 (5) = happyGoto action_2
action_39 _ = happyFail (happyExpListPerState 39)

action_40 (18) = happyShift action_5
action_40 (20) = happyShift action_49
action_40 (22) = happyShift action_50
action_40 (23) = happyShift action_6
action_40 (29) = happyShift action_7
action_40 (34) = happyShift action_8
action_40 (35) = happyShift action_9
action_40 (36) = happyShift action_10
action_40 (59) = happyShift action_3
action_40 (4) = happyGoto action_48
action_40 (5) = happyGoto action_2
action_40 _ = happyFail (happyExpListPerState 40)

action_41 _ = happyReduce_30

action_42 (42) = happyShift action_46
action_42 (43) = happyShift action_47
action_42 _ = happyFail (happyExpListPerState 42)

action_43 (45) = happyShift action_45
action_43 _ = happyReduce_11

action_44 _ = happyReduce_1

action_45 (59) = happyShift action_28
action_45 (10) = happyGoto action_55
action_45 _ = happyFail (happyExpListPerState 45)

action_46 (44) = happyShift action_54
action_46 _ = happyFail (happyExpListPerState 46)

action_47 (21) = happyShift action_13
action_47 (32) = happyShift action_14
action_47 (41) = happyShift action_15
action_47 (57) = happyShift action_16
action_47 (58) = happyShift action_17
action_47 (59) = happyShift action_3
action_47 (5) = happyGoto action_11
action_47 (6) = happyGoto action_53
action_47 _ = happyFail (happyExpListPerState 47)

action_48 _ = happyReduce_28

action_49 (18) = happyShift action_5
action_49 (23) = happyShift action_6
action_49 (29) = happyShift action_7
action_49 (34) = happyShift action_8
action_49 (35) = happyShift action_9
action_49 (36) = happyShift action_10
action_49 (59) = happyShift action_3
action_49 (4) = happyGoto action_38
action_49 (5) = happyGoto action_2
action_49 (11) = happyGoto action_52
action_49 _ = happyReduce_26

action_50 _ = happyReduce_6

action_51 _ = happyReduce_7

action_52 (18) = happyShift action_5
action_52 (22) = happyShift action_56
action_52 (23) = happyShift action_6
action_52 (29) = happyShift action_7
action_52 (34) = happyShift action_8
action_52 (35) = happyShift action_9
action_52 (36) = happyShift action_10
action_52 (59) = happyShift action_3
action_52 (4) = happyGoto action_48
action_52 (5) = happyGoto action_2
action_52 _ = happyFail (happyExpListPerState 52)

action_53 _ = happyReduce_31

action_54 _ = happyReduce_8

action_55 _ = happyReduce_12

action_56 _ = happyReduce_5

happyReduce_1 = happyReduce 4 4 happyReduction_1
happyReduction_1 (_ `HappyStk`
	(HappyAbsSyn6  happy_var_3) `HappyStk`
	_ `HappyStk`
	(HappyAbsSyn5  happy_var_1) `HappyStk`
	happyRest)
	 = HappyAbsSyn4
		 (Assign happy_var_1 happy_var_3
	) `HappyStk` happyRest

happyReduce_2 = happySpecReduce_3  4 happyReduction_2
happyReduction_2 _
	(HappyAbsSyn5  happy_var_2)
	_
	 =  HappyAbsSyn4
		 (Read happy_var_2
	)
happyReduction_2 _ _ _  = notHappyAtAll 

happyReduce_3 = happySpecReduce_3  4 happyReduction_3
happyReduction_3 _
	(HappyAbsSyn6  happy_var_2)
	_
	 =  HappyAbsSyn4
		 (Write happy_var_2
	)
happyReduction_3 _ _ _  = notHappyAtAll 

happyReduce_4 = happySpecReduce_3  4 happyReduction_4
happyReduction_4 _
	(HappyAbsSyn6  happy_var_2)
	_
	 =  HappyAbsSyn4
		 (Writeln happy_var_2
	)
happyReduction_4 _ _ _  = notHappyAtAll 

happyReduce_5 = happyReduce 7 4 happyReduction_5
happyReduction_5 (_ `HappyStk`
	(HappyAbsSyn11  happy_var_6) `HappyStk`
	_ `HappyStk`
	(HappyAbsSyn11  happy_var_4) `HappyStk`
	_ `HappyStk`
	(HappyAbsSyn6  happy_var_2) `HappyStk`
	_ `HappyStk`
	happyRest)
	 = HappyAbsSyn4
		 (IfElse happy_var_2 happy_var_4 happy_var_6
	) `HappyStk` happyRest

happyReduce_6 = happyReduce 5 4 happyReduction_6
happyReduction_6 (_ `HappyStk`
	(HappyAbsSyn11  happy_var_4) `HappyStk`
	_ `HappyStk`
	(HappyAbsSyn6  happy_var_2) `HappyStk`
	_ `HappyStk`
	happyRest)
	 = HappyAbsSyn4
		 (If happy_var_2 happy_var_4
	) `HappyStk` happyRest

happyReduce_7 = happyReduce 5 4 happyReduction_7
happyReduction_7 (_ `HappyStk`
	(HappyAbsSyn11  happy_var_4) `HappyStk`
	_ `HappyStk`
	(HappyAbsSyn6  happy_var_2) `HappyStk`
	_ `HappyStk`
	happyRest)
	 = HappyAbsSyn4
		 (While happy_var_2 happy_var_4
	) `HappyStk` happyRest

happyReduce_8 = happyReduce 6 4 happyReduction_8
happyReduction_8 (_ `HappyStk`
	_ `HappyStk`
	(HappyAbsSyn12  happy_var_4) `HappyStk`
	_ `HappyStk`
	(HappyTerminal ((pos, T_ident happy_var_2))) `HappyStk`
	_ `HappyStk`
	happyRest)
	 = HappyAbsSyn4
		 (Call happy_var_2 happy_var_4
	) `HappyStk` happyRest

happyReduce_9 = happySpecReduce_1  5 happyReduction_9
happyReduction_9 (HappyTerminal ((pos, T_ident happy_var_1)))
	 =  HappyAbsSyn5
		 (LId happy_var_1
	)
happyReduction_9 _  = notHappyAtAll 

happyReduce_10 = happySpecReduce_3  5 happyReduction_10
happyReduction_10 (HappyAbsSyn10  happy_var_3)
	_
	(HappyTerminal ((pos, T_ident happy_var_1)))
	 =  HappyAbsSyn5
		 (LField happy_var_1 happy_var_3
	)
happyReduction_10 _ _ _  = notHappyAtAll 

happyReduce_11 = happyReduce 4 5 happyReduction_11
happyReduction_11 (_ `HappyStk`
	(HappyAbsSyn6  happy_var_3) `HappyStk`
	_ `HappyStk`
	(HappyTerminal ((pos, T_ident happy_var_1))) `HappyStk`
	happyRest)
	 = HappyAbsSyn5
		 (LInd happy_var_1 happy_var_3
	) `HappyStk` happyRest

happyReduce_12 = happyReduce 6 5 happyReduction_12
happyReduction_12 ((HappyAbsSyn10  happy_var_6) `HappyStk`
	_ `HappyStk`
	_ `HappyStk`
	(HappyAbsSyn6  happy_var_3) `HappyStk`
	_ `HappyStk`
	(HappyTerminal ((pos, T_ident happy_var_1))) `HappyStk`
	happyRest)
	 = HappyAbsSyn5
		 (LIndField happy_var_1 happy_var_3 happy_var_6
	) `HappyStk` happyRest

happyReduce_13 = happySpecReduce_1  6 happyReduction_13
happyReduction_13 (HappyAbsSyn5  happy_var_1)
	 =  HappyAbsSyn6
		 (Lval happy_var_1
	)
happyReduction_13 _  = notHappyAtAll 

happyReduce_14 = happySpecReduce_1  6 happyReduction_14
happyReduction_14 _
	 =  HappyAbsSyn6
		 (BoolConst False
	)

happyReduce_15 = happySpecReduce_1  6 happyReduction_15
happyReduction_15 _
	 =  HappyAbsSyn6
		 (BoolConst True
	)

happyReduce_16 = happySpecReduce_1  6 happyReduction_16
happyReduction_16 (HappyTerminal ((pos, T_number happy_var_1)))
	 =  HappyAbsSyn6
		 (IntConst happy_var_1
	)
happyReduction_16 _  = notHappyAtAll 

happyReduce_17 = happySpecReduce_1  6 happyReduction_17
happyReduction_17 (HappyTerminal ((pos, T_string happy_var_1)))
	 =  HappyAbsSyn6
		 (StrConst happy_var_1
	)
happyReduction_17 _  = notHappyAtAll 

happyReduce_18 = happySpecReduce_3  6 happyReduction_18
happyReduction_18 _
	(HappyAbsSyn6  happy_var_2)
	_
	 =  HappyAbsSyn6
		 (happy_var_2
	)
happyReduction_18 _ _ _  = notHappyAtAll 

happyReduce_19 = happySpecReduce_3  7 happyReduction_19
happyReduction_19 _
	(HappyTerminal ((pos, T_ident happy_var_2)))
	(HappyAbsSyn8  happy_var_1)
	 =  HappyAbsSyn7
		 (Decl happy_var_1 happy_var_2
	)
happyReduction_19 _ _ _  = notHappyAtAll 

happyReduce_20 = happySpecReduce_1  8 happyReduction_20
happyReduction_20 _
	 =  HappyAbsSyn8
		 (BoolType
	)

happyReduce_21 = happySpecReduce_1  8 happyReduction_21
happyReduction_21 _
	 =  HappyAbsSyn8
		 (IntType
	)

happyReduce_22 = happySpecReduce_1  8 happyReduction_22
happyReduction_22 (HappyTerminal ((pos, T_ident happy_var_1)))
	 =  HappyAbsSyn8
		 (TypeAlias happy_var_1
	)
happyReduction_22 _  = notHappyAtAll 

happyReduce_23 = happySpecReduce_1  9 happyReduction_23
happyReduction_23 (HappyAbsSyn7  happy_var_1)
	 =  HappyAbsSyn9
		 ([happy_var_1]
	)
happyReduction_23 _  = notHappyAtAll 

happyReduce_24 = happySpecReduce_2  9 happyReduction_24
happyReduction_24 (HappyAbsSyn7  happy_var_2)
	(HappyAbsSyn9  happy_var_1)
	 =  HappyAbsSyn9
		 (happy_var_2 : happy_var_1
	)
happyReduction_24 _ _  = notHappyAtAll 

happyReduce_25 = happySpecReduce_1  10 happyReduction_25
happyReduction_25 (HappyTerminal ((pos, T_ident happy_var_1)))
	 =  HappyAbsSyn10
		 (happy_var_1
	)
happyReduction_25 _  = notHappyAtAll 

happyReduce_26 = happySpecReduce_0  11 happyReduction_26
happyReduction_26  =  HappyAbsSyn11
		 ([]
	)

happyReduce_27 = happySpecReduce_1  11 happyReduction_27
happyReduction_27 (HappyAbsSyn4  happy_var_1)
	 =  HappyAbsSyn11
		 ([happy_var_1]
	)
happyReduction_27 _  = notHappyAtAll 

happyReduce_28 = happySpecReduce_2  11 happyReduction_28
happyReduction_28 (HappyAbsSyn4  happy_var_2)
	(HappyAbsSyn11  happy_var_1)
	 =  HappyAbsSyn11
		 (happy_var_2 : happy_var_1
	)
happyReduction_28 _ _  = notHappyAtAll 

happyReduce_29 = happySpecReduce_0  12 happyReduction_29
happyReduction_29  =  HappyAbsSyn12
		 ([]
	)

happyReduce_30 = happySpecReduce_1  12 happyReduction_30
happyReduction_30 (HappyAbsSyn6  happy_var_1)
	 =  HappyAbsSyn12
		 ([happy_var_1]
	)
happyReduction_30 _  = notHappyAtAll 

happyReduce_31 = happySpecReduce_3  12 happyReduction_31
happyReduction_31 (HappyAbsSyn6  happy_var_3)
	_
	(HappyAbsSyn12  happy_var_1)
	 =  HappyAbsSyn12
		 (happy_var_3 : happy_var_1
	)
happyReduction_31 _ _ _  = notHappyAtAll 

happyReduce_32 = happyReduce 5 13 happyReduction_32
happyReduction_32 ((HappyAbsSyn7  happy_var_5) `HappyStk`
	_ `HappyStk`
	(HappyTerminal ((pos, T_number happy_var_3))) `HappyStk`
	_ `HappyStk`
	_ `HappyStk`
	happyRest)
	 = HappyAbsSyn13
		 (Array happy_var_3 happy_var_5
	) `HappyStk` happyRest

happyReduce_33 = happyReduce 6 14 happyReduction_33
happyReduction_33 (_ `HappyStk`
	(HappyTerminal ((pos, T_ident happy_var_5))) `HappyStk`
	_ `HappyStk`
	(HappyAbsSyn9  happy_var_3) `HappyStk`
	_ `HappyStk`
	_ `HappyStk`
	happyRest)
	 = HappyAbsSyn14
		 (Record happy_var_3 happy_var_5
	) `HappyStk` happyRest

happyNewToken action sts stk [] =
	action 60 60 notHappyAtAll (HappyState action) sts stk []

happyNewToken action sts stk (tk:tks) =
	let cont i = action i i tk (HappyState action) sts stk tks in
	case tk of {
	(pos, T_and) -> cont 15;
	(pos, T_array) -> cont 16;
	(pos, T_boolean) -> cont 17;
	(pos, T_call) -> cont 18;
	(pos, T_do) -> cont 19;
	(pos, T_else) -> cont 20;
	(pos, T_false) -> cont 21;
	(pos, T_fi) -> cont 22;
	(pos, T_if) -> cont 23;
	(pos, T_integer) -> cont 24;
	(pos, T_not) -> cont 25;
	(pos, T_od) -> cont 26;
	(pos, T_or) -> cont 27;
	(pos, T_procedure) -> cont 28;
	(pos, T_read) -> cont 29;
	(pos, T_record) -> cont 30;
	(pos, T_then) -> cont 31;
	(pos, T_true) -> cont 32;
	(pos, T_val) -> cont 33;
	(pos, T_while) -> cont 34;
	(pos, T_write) -> cont 35;
	(pos, T_writeln) -> cont 36;
	(pos, T_lbrace) -> cont 37;
	(pos, T_rbrace) -> cont 38;
	(pos, T_lbracket) -> cont 39;
	(pos, T_rbracket) -> cont 40;
	(pos, T_lparen) -> cont 41;
	(pos, T_rparen) -> cont 42;
	(pos, T_comma) -> cont 43;
	(pos, T_semi) -> cont 44;
	(pos, T_dot) -> cont 45;
	(pos, T_eq) -> cont 46;
	(pos, T_neq) -> cont 47;
	(pos, T_lt) -> cont 48;
	(pos, T_leq) -> cont 49;
	(pos, T_assign) -> cont 50;
	(pos, T_gt) -> cont 51;
	(pos, T_geq) -> cont 52;
	(pos, T_add) -> cont 53;
	(pos, T_sub) -> cont 54;
	(pos, T_mul) -> cont 55;
	(pos, T_div) -> cont 56;
	(pos, T_string happy_dollar_dollar) -> cont 57;
	(pos, T_number happy_dollar_dollar) -> cont 58;
	(pos, T_ident happy_dollar_dollar) -> cont 59;
	_ -> happyError' ((tk:tks), [])
	}

happyError_ explist 60 tk tks = happyError' (tks, explist)
happyError_ explist _ tk tks = happyError' ((tk:tks), explist)

newtype HappyIdentity a = HappyIdentity a
happyIdentity = HappyIdentity
happyRunIdentity (HappyIdentity a) = a

instance Functor HappyIdentity where
    fmap f (HappyIdentity a) = HappyIdentity (f a)

instance Applicative HappyIdentity where
    pure  = HappyIdentity
    (<*>) = ap
instance Monad HappyIdentity where
    return = pure
    (HappyIdentity p) >>= q = q p

happyThen :: () => HappyIdentity a -> (a -> HappyIdentity b) -> HappyIdentity b
happyThen = (>>=)
happyReturn :: () => a -> HappyIdentity a
happyReturn = (return)
happyThen1 m k tks = (>>=) m (\a -> k a tks)
happyReturn1 :: () => a -> b -> HappyIdentity a
happyReturn1 = \a tks -> (return) a
happyError' :: () => ([(PosnToken)], [String]) -> HappyIdentity a
happyError' = HappyIdentity . (\(tokens, _) -> parseError tokens)
parse tks = happyRunIdentity happySomeParser where
 happySomeParser = happyThen (happyParse action_0 tks) (\x -> case x of {HappyAbsSyn4 z -> happyReturn z; _other -> notHappyAtAll })

happySeq = happyDontSeq


parseError :: [PosnToken] -> a
parseError x = error ("Not expecting " ++ token)
                  where
                        token = show x
{-# LINE 1 "templates/GenericTemplate.hs" #-}
{-# LINE 1 "templates/GenericTemplate.hs" #-}
{-# LINE 1 "<built-in>" #-}
{-# LINE 1 "<command-line>" #-}







# 1 "/usr/include/stdc-predef.h" 1 3 4

# 17 "/usr/include/stdc-predef.h" 3 4











































{-# LINE 7 "<command-line>" #-}
{-# LINE 1 "/usr/lib/ghc/include/ghcversion.h" #-}















{-# LINE 7 "<command-line>" #-}
{-# LINE 1 "/tmp/ghc8336_0/ghc_2.h" #-}
































































































































































































{-# LINE 7 "<command-line>" #-}
{-# LINE 1 "templates/GenericTemplate.hs" #-}
-- Id: GenericTemplate.hs,v 1.26 2005/01/14 14:47:22 simonmar Exp 









{-# LINE 43 "templates/GenericTemplate.hs" #-}

data Happy_IntList = HappyCons Int Happy_IntList







{-# LINE 65 "templates/GenericTemplate.hs" #-}

{-# LINE 75 "templates/GenericTemplate.hs" #-}

{-# LINE 84 "templates/GenericTemplate.hs" #-}

infixr 9 `HappyStk`
data HappyStk a = HappyStk a (HappyStk a)

-----------------------------------------------------------------------------
-- starting the parse

happyParse start_state = happyNewToken start_state notHappyAtAll notHappyAtAll

-----------------------------------------------------------------------------
-- Accepting the parse

-- If the current token is (1), it means we've just accepted a partial
-- parse (a %partial parser).  We must ignore the saved token on the top of
-- the stack in this case.
happyAccept (1) tk st sts (_ `HappyStk` ans `HappyStk` _) =
        happyReturn1 ans
happyAccept j tk st sts (HappyStk ans _) = 
         (happyReturn1 ans)

-----------------------------------------------------------------------------
-- Arrays only: do the next action

{-# LINE 137 "templates/GenericTemplate.hs" #-}

{-# LINE 147 "templates/GenericTemplate.hs" #-}
indexShortOffAddr arr off = arr Happy_Data_Array.! off


{-# INLINE happyLt #-}
happyLt x y = (x < y)






readArrayBit arr bit =
    Bits.testBit (indexShortOffAddr arr (bit `div` 16)) (bit `mod` 16)






-----------------------------------------------------------------------------
-- HappyState data type (not arrays)



newtype HappyState b c = HappyState
        (Int ->                    -- token number
         Int ->                    -- token number (yes, again)
         b ->                           -- token semantic value
         HappyState b c ->              -- current state
         [HappyState b c] ->            -- state stack
         c)



-----------------------------------------------------------------------------
-- Shifting a token

happyShift new_state (1) tk st sts stk@(x `HappyStk` _) =
     let i = (case x of { HappyErrorToken (i) -> i }) in
--     trace "shifting the error token" $
     new_state i i tk (HappyState (new_state)) ((st):(sts)) (stk)

happyShift new_state i tk st sts stk =
     happyNewToken new_state ((st):(sts)) ((HappyTerminal (tk))`HappyStk`stk)

-- happyReduce is specialised for the common cases.

happySpecReduce_0 i fn (1) tk st sts stk
     = happyFail [] (1) tk st sts stk
happySpecReduce_0 nt fn j tk st@((HappyState (action))) sts stk
     = action nt j tk st ((st):(sts)) (fn `HappyStk` stk)

happySpecReduce_1 i fn (1) tk st sts stk
     = happyFail [] (1) tk st sts stk
happySpecReduce_1 nt fn j tk _ sts@(((st@(HappyState (action))):(_))) (v1`HappyStk`stk')
     = let r = fn v1 in
       happySeq r (action nt j tk st sts (r `HappyStk` stk'))

happySpecReduce_2 i fn (1) tk st sts stk
     = happyFail [] (1) tk st sts stk
happySpecReduce_2 nt fn j tk _ ((_):(sts@(((st@(HappyState (action))):(_))))) (v1`HappyStk`v2`HappyStk`stk')
     = let r = fn v1 v2 in
       happySeq r (action nt j tk st sts (r `HappyStk` stk'))

happySpecReduce_3 i fn (1) tk st sts stk
     = happyFail [] (1) tk st sts stk
happySpecReduce_3 nt fn j tk _ ((_):(((_):(sts@(((st@(HappyState (action))):(_))))))) (v1`HappyStk`v2`HappyStk`v3`HappyStk`stk')
     = let r = fn v1 v2 v3 in
       happySeq r (action nt j tk st sts (r `HappyStk` stk'))

happyReduce k i fn (1) tk st sts stk
     = happyFail [] (1) tk st sts stk
happyReduce k nt fn j tk st sts stk
     = case happyDrop (k - ((1) :: Int)) sts of
         sts1@(((st1@(HappyState (action))):(_))) ->
                let r = fn stk in  -- it doesn't hurt to always seq here...
                happyDoSeq r (action nt j tk st1 sts1 r)

happyMonadReduce k nt fn (1) tk st sts stk
     = happyFail [] (1) tk st sts stk
happyMonadReduce k nt fn j tk st sts stk =
      case happyDrop k ((st):(sts)) of
        sts1@(((st1@(HappyState (action))):(_))) ->
          let drop_stk = happyDropStk k stk in
          happyThen1 (fn stk tk) (\r -> action nt j tk st1 sts1 (r `HappyStk` drop_stk))

happyMonad2Reduce k nt fn (1) tk st sts stk
     = happyFail [] (1) tk st sts stk
happyMonad2Reduce k nt fn j tk st sts stk =
      case happyDrop k ((st):(sts)) of
        sts1@(((st1@(HappyState (action))):(_))) ->
         let drop_stk = happyDropStk k stk





             _ = nt :: Int
             new_state = action

          in
          happyThen1 (fn stk tk) (\r -> happyNewToken new_state sts1 (r `HappyStk` drop_stk))

happyDrop (0) l = l
happyDrop n ((_):(t)) = happyDrop (n - ((1) :: Int)) t

happyDropStk (0) l = l
happyDropStk n (x `HappyStk` xs) = happyDropStk (n - ((1)::Int)) xs

-----------------------------------------------------------------------------
-- Moving to a new state after a reduction

{-# LINE 267 "templates/GenericTemplate.hs" #-}
happyGoto action j tk st = action j j tk (HappyState action)


-----------------------------------------------------------------------------
-- Error recovery ((1) is the error token)

-- parse error if we are in recovery and we fail again
happyFail explist (1) tk old_st _ stk@(x `HappyStk` _) =
     let i = (case x of { HappyErrorToken (i) -> i }) in
--      trace "failing" $ 
        happyError_ explist i tk

{-  We don't need state discarding for our restricted implementation of
    "error".  In fact, it can cause some bogus parses, so I've disabled it
    for now --SDM

-- discard a state
happyFail  (1) tk old_st (((HappyState (action))):(sts)) 
                                                (saved_tok `HappyStk` _ `HappyStk` stk) =
--      trace ("discarding state, depth " ++ show (length stk))  $
        action (1) (1) tk (HappyState (action)) sts ((saved_tok`HappyStk`stk))
-}

-- Enter error recovery: generate an error token,
--                       save the old token and carry on.
happyFail explist i tk (HappyState (action)) sts stk =
--      trace "entering error recovery" $
        action (1) (1) tk (HappyState (action)) sts ( (HappyErrorToken (i)) `HappyStk` stk)

-- Internal happy errors:

notHappyAtAll :: a
notHappyAtAll = error "Internal Happy error\n"

-----------------------------------------------------------------------------
-- Hack to get the typechecker to accept our action functions







-----------------------------------------------------------------------------
-- Seq-ing.  If the --strict flag is given, then Happy emits 
--      happySeq = happyDoSeq
-- otherwise it emits
--      happySeq = happyDontSeq

happyDoSeq, happyDontSeq :: a -> b -> b
happyDoSeq   a b = a `seq` b
happyDontSeq a b = b

-----------------------------------------------------------------------------
-- Don't inline any functions from the template.  GHC has a nasty habit
-- of deciding to inline happyGoto everywhere, which increases the size of
-- the generated parser quite a bit.

{-# LINE 333 "templates/GenericTemplate.hs" #-}
{-# NOINLINE happyShift #-}
{-# NOINLINE happySpecReduce_0 #-}
{-# NOINLINE happySpecReduce_1 #-}
{-# NOINLINE happySpecReduce_2 #-}
{-# NOINLINE happySpecReduce_3 #-}
{-# NOINLINE happyReduce #-}
{-# NOINLINE happyMonadReduce #-}
{-# NOINLINE happyGoto #-}
{-# NOINLINE happyFail #-}

-- end of Happy Template.
