-- parser produced by Happy Version 1.13

module Parfolp where
import Absfolp
import Lexfolp
import ErrM

data HappyAbsSyn t5 t6 t7
	= HappyTerminal Token
	| HappyErrorToken Int
	| HappyAbsSyn5 t5
	| HappyAbsSyn6 t6
	| HappyAbsSyn7 t7
	| HappyAbsSyn8 (Ltm)
	| HappyAbsSyn9 (MemSegment)
	| HappyAbsSyn10 (Exp)
	| HappyAbsSyn16 (Pred)
	| HappyAbsSyn17 (Atom)
	| HappyAbsSyn18 (ODom)
	| HappyAbsSyn19 (VCtx)
	| HappyAbsSyn20 ([Exp])
	| HappyAbsSyn21 ([MemSegment])
	| HappyAbsSyn22 ([Integer])
	| HappyAbsSyn23 ([VCtx])

action_0 (8) = happyGoto action_23
action_0 (21) = happyGoto action_24
action_0 _ = happyReduce_39

action_1 (32) = happyShift action_12
action_1 (33) = happyShift action_13
action_1 (34) = happyShift action_14
action_1 (35) = happyShift action_15
action_1 (43) = happyShift action_16
action_1 (44) = happyShift action_17
action_1 (45) = happyShift action_18
action_1 (46) = happyShift action_19
action_1 (47) = happyShift action_20
action_1 (48) = happyShift action_21
action_1 (51) = happyShift action_22
action_1 (7) = happyGoto action_4
action_1 (10) = happyGoto action_5
action_1 (11) = happyGoto action_6
action_1 (12) = happyGoto action_7
action_1 (13) = happyGoto action_8
action_1 (14) = happyGoto action_9
action_1 (15) = happyGoto action_10
action_1 (16) = happyGoto action_11
action_1 _ = happyFail

action_2 (49) = happyShift action_3
action_2 _ = happyFail

action_3 _ = happyReduce_2

action_4 _ = happyReduce_20

action_5 (53) = happyAccept
action_5 _ = happyFail

action_6 (28) = happyShift action_39
action_6 (29) = happyShift action_40
action_6 (30) = happyShift action_41
action_6 (31) = happyShift action_42
action_6 _ = happyReduce_12

action_7 _ = happyReduce_15

action_8 _ = happyReduce_17

action_9 _ = happyReduce_22

action_10 _ = happyReduce_25

action_11 _ = happyReduce_21

action_12 (32) = happyShift action_12
action_12 (33) = happyShift action_13
action_12 (34) = happyShift action_14
action_12 (35) = happyShift action_15
action_12 (43) = happyShift action_16
action_12 (44) = happyShift action_17
action_12 (45) = happyShift action_18
action_12 (46) = happyShift action_19
action_12 (47) = happyShift action_20
action_12 (48) = happyShift action_21
action_12 (51) = happyShift action_22
action_12 (7) = happyGoto action_4
action_12 (12) = happyGoto action_38
action_12 (13) = happyGoto action_8
action_12 (14) = happyGoto action_9
action_12 (15) = happyGoto action_10
action_12 (16) = happyGoto action_11
action_12 _ = happyFail

action_13 (50) = happyShift action_36
action_13 (6) = happyGoto action_37
action_13 _ = happyFail

action_14 (50) = happyShift action_36
action_14 (6) = happyGoto action_35
action_14 _ = happyFail

action_15 (32) = happyShift action_12
action_15 (33) = happyShift action_13
action_15 (34) = happyShift action_14
action_15 (35) = happyShift action_15
action_15 (43) = happyShift action_16
action_15 (44) = happyShift action_17
action_15 (45) = happyShift action_18
action_15 (46) = happyShift action_19
action_15 (47) = happyShift action_20
action_15 (48) = happyShift action_21
action_15 (51) = happyShift action_22
action_15 (7) = happyGoto action_4
action_15 (10) = happyGoto action_34
action_15 (11) = happyGoto action_6
action_15 (12) = happyGoto action_7
action_15 (13) = happyGoto action_8
action_15 (14) = happyGoto action_9
action_15 (15) = happyGoto action_10
action_15 (16) = happyGoto action_11
action_15 _ = happyFail

action_16 (35) = happyShift action_33
action_16 _ = happyFail

action_17 (35) = happyShift action_32
action_17 _ = happyFail

action_18 (35) = happyShift action_31
action_18 _ = happyFail

action_19 (35) = happyShift action_30
action_19 _ = happyFail

action_20 _ = happyReduce_18

action_21 _ = happyReduce_19

action_22 _ = happyReduce_4

action_23 (53) = happyAccept
action_23 _ = happyFail

action_24 (24) = happyShift action_26
action_24 (25) = happyShift action_27
action_24 (26) = happyShift action_28
action_24 (27) = happyShift action_29
action_24 (9) = happyGoto action_25
action_24 _ = happyReduce_5

action_25 _ = happyReduce_40

action_26 (20) = happyGoto action_60
action_26 _ = happyReduce_37

action_27 (20) = happyGoto action_59
action_27 _ = happyReduce_37

action_28 (20) = happyGoto action_58
action_28 _ = happyReduce_37

action_29 (20) = happyGoto action_57
action_29 _ = happyReduce_37

action_30 (49) = happyShift action_3
action_30 (50) = happyShift action_36
action_30 (5) = happyGoto action_51
action_30 (6) = happyGoto action_52
action_30 (17) = happyGoto action_56
action_30 _ = happyFail

action_31 (49) = happyShift action_3
action_31 (50) = happyShift action_36
action_31 (5) = happyGoto action_51
action_31 (6) = happyGoto action_52
action_31 (17) = happyGoto action_55
action_31 _ = happyFail

action_32 (49) = happyShift action_3
action_32 (50) = happyShift action_36
action_32 (5) = happyGoto action_51
action_32 (6) = happyGoto action_52
action_32 (17) = happyGoto action_54
action_32 _ = happyFail

action_33 (49) = happyShift action_3
action_33 (50) = happyShift action_36
action_33 (5) = happyGoto action_51
action_33 (6) = happyGoto action_52
action_33 (17) = happyGoto action_53
action_33 _ = happyFail

action_34 (36) = happyShift action_50
action_34 _ = happyFail

action_35 (40) = happyShift action_48
action_35 (18) = happyGoto action_49
action_35 _ = happyReduce_35

action_36 _ = happyReduce_3

action_37 (40) = happyShift action_48
action_37 (18) = happyGoto action_47
action_37 _ = happyReduce_35

action_38 _ = happyReduce_16

action_39 (32) = happyShift action_12
action_39 (33) = happyShift action_13
action_39 (34) = happyShift action_14
action_39 (35) = happyShift action_15
action_39 (43) = happyShift action_16
action_39 (44) = happyShift action_17
action_39 (45) = happyShift action_18
action_39 (46) = happyShift action_19
action_39 (47) = happyShift action_20
action_39 (48) = happyShift action_21
action_39 (51) = happyShift action_22
action_39 (7) = happyGoto action_4
action_39 (10) = happyGoto action_46
action_39 (11) = happyGoto action_6
action_39 (12) = happyGoto action_7
action_39 (13) = happyGoto action_8
action_39 (14) = happyGoto action_9
action_39 (15) = happyGoto action_10
action_39 (16) = happyGoto action_11
action_39 _ = happyFail

action_40 (32) = happyShift action_12
action_40 (33) = happyShift action_13
action_40 (34) = happyShift action_14
action_40 (35) = happyShift action_15
action_40 (43) = happyShift action_16
action_40 (44) = happyShift action_17
action_40 (45) = happyShift action_18
action_40 (46) = happyShift action_19
action_40 (47) = happyShift action_20
action_40 (48) = happyShift action_21
action_40 (51) = happyShift action_22
action_40 (7) = happyGoto action_4
action_40 (10) = happyGoto action_45
action_40 (11) = happyGoto action_6
action_40 (12) = happyGoto action_7
action_40 (13) = happyGoto action_8
action_40 (14) = happyGoto action_9
action_40 (15) = happyGoto action_10
action_40 (16) = happyGoto action_11
action_40 _ = happyFail

action_41 (32) = happyShift action_12
action_41 (33) = happyShift action_13
action_41 (34) = happyShift action_14
action_41 (35) = happyShift action_15
action_41 (43) = happyShift action_16
action_41 (44) = happyShift action_17
action_41 (45) = happyShift action_18
action_41 (46) = happyShift action_19
action_41 (47) = happyShift action_20
action_41 (48) = happyShift action_21
action_41 (51) = happyShift action_22
action_41 (7) = happyGoto action_4
action_41 (12) = happyGoto action_44
action_41 (13) = happyGoto action_8
action_41 (14) = happyGoto action_9
action_41 (15) = happyGoto action_10
action_41 (16) = happyGoto action_11
action_41 _ = happyFail

action_42 (32) = happyShift action_12
action_42 (33) = happyShift action_13
action_42 (34) = happyShift action_14
action_42 (35) = happyShift action_15
action_42 (43) = happyShift action_16
action_42 (44) = happyShift action_17
action_42 (45) = happyShift action_18
action_42 (46) = happyShift action_19
action_42 (47) = happyShift action_20
action_42 (48) = happyShift action_21
action_42 (51) = happyShift action_22
action_42 (7) = happyGoto action_4
action_42 (12) = happyGoto action_43
action_42 (13) = happyGoto action_8
action_42 (14) = happyGoto action_9
action_42 (15) = happyGoto action_10
action_42 (16) = happyGoto action_11
action_42 _ = happyFail

action_43 _ = happyReduce_14

action_44 _ = happyReduce_13

action_45 _ = happyReduce_11

action_46 _ = happyReduce_10

action_47 (33) = happyShift action_13
action_47 (34) = happyShift action_14
action_47 (35) = happyShift action_15
action_47 (14) = happyGoto action_70
action_47 (15) = happyGoto action_10
action_47 _ = happyFail

action_48 (49) = happyShift action_3
action_48 (5) = happyGoto action_68
action_48 (22) = happyGoto action_69
action_48 _ = happyReduce_41

action_49 (33) = happyShift action_13
action_49 (34) = happyShift action_14
action_49 (35) = happyShift action_15
action_49 (14) = happyGoto action_67
action_49 (15) = happyGoto action_10
action_49 _ = happyFail

action_50 (37) = happyShift action_66
action_50 _ = happyReduce_27

action_51 _ = happyReduce_33

action_52 _ = happyReduce_32

action_53 (36) = happyShift action_65
action_53 _ = happyFail

action_54 (39) = happyShift action_64
action_54 _ = happyFail

action_55 (36) = happyShift action_63
action_55 _ = happyFail

action_56 (36) = happyShift action_62
action_56 _ = happyFail

action_57 (32) = happyShift action_12
action_57 (33) = happyShift action_13
action_57 (34) = happyShift action_14
action_57 (35) = happyShift action_15
action_57 (43) = happyShift action_16
action_57 (44) = happyShift action_17
action_57 (45) = happyShift action_18
action_57 (46) = happyShift action_19
action_57 (47) = happyShift action_20
action_57 (48) = happyShift action_21
action_57 (51) = happyShift action_22
action_57 (7) = happyGoto action_4
action_57 (10) = happyGoto action_61
action_57 (11) = happyGoto action_6
action_57 (12) = happyGoto action_7
action_57 (13) = happyGoto action_8
action_57 (14) = happyGoto action_9
action_57 (15) = happyGoto action_10
action_57 (16) = happyGoto action_11
action_57 _ = happyReduce_9

action_58 (32) = happyShift action_12
action_58 (33) = happyShift action_13
action_58 (34) = happyShift action_14
action_58 (35) = happyShift action_15
action_58 (43) = happyShift action_16
action_58 (44) = happyShift action_17
action_58 (45) = happyShift action_18
action_58 (46) = happyShift action_19
action_58 (47) = happyShift action_20
action_58 (48) = happyShift action_21
action_58 (51) = happyShift action_22
action_58 (7) = happyGoto action_4
action_58 (10) = happyGoto action_61
action_58 (11) = happyGoto action_6
action_58 (12) = happyGoto action_7
action_58 (13) = happyGoto action_8
action_58 (14) = happyGoto action_9
action_58 (15) = happyGoto action_10
action_58 (16) = happyGoto action_11
action_58 _ = happyReduce_8

action_59 (32) = happyShift action_12
action_59 (33) = happyShift action_13
action_59 (34) = happyShift action_14
action_59 (35) = happyShift action_15
action_59 (43) = happyShift action_16
action_59 (44) = happyShift action_17
action_59 (45) = happyShift action_18
action_59 (46) = happyShift action_19
action_59 (47) = happyShift action_20
action_59 (48) = happyShift action_21
action_59 (51) = happyShift action_22
action_59 (7) = happyGoto action_4
action_59 (10) = happyGoto action_61
action_59 (11) = happyGoto action_6
action_59 (12) = happyGoto action_7
action_59 (13) = happyGoto action_8
action_59 (14) = happyGoto action_9
action_59 (15) = happyGoto action_10
action_59 (16) = happyGoto action_11
action_59 _ = happyReduce_7

action_60 (32) = happyShift action_12
action_60 (33) = happyShift action_13
action_60 (34) = happyShift action_14
action_60 (35) = happyShift action_15
action_60 (43) = happyShift action_16
action_60 (44) = happyShift action_17
action_60 (45) = happyShift action_18
action_60 (46) = happyShift action_19
action_60 (47) = happyShift action_20
action_60 (48) = happyShift action_21
action_60 (51) = happyShift action_22
action_60 (7) = happyGoto action_4
action_60 (10) = happyGoto action_61
action_60 (11) = happyGoto action_6
action_60 (12) = happyGoto action_7
action_60 (13) = happyGoto action_8
action_60 (14) = happyGoto action_9
action_60 (15) = happyGoto action_10
action_60 (16) = happyGoto action_11
action_60 _ = happyReduce_6

action_61 _ = happyReduce_38

action_62 _ = happyReduce_30

action_63 _ = happyReduce_29

action_64 (49) = happyShift action_3
action_64 (50) = happyShift action_36
action_64 (5) = happyGoto action_51
action_64 (6) = happyGoto action_52
action_64 (17) = happyGoto action_76
action_64 _ = happyFail

action_65 _ = happyReduce_28

action_66 (50) = happyShift action_36
action_66 (6) = happyGoto action_73
action_66 (19) = happyGoto action_74
action_66 (23) = happyGoto action_75
action_66 _ = happyFail

action_67 _ = happyReduce_24

action_68 (39) = happyShift action_72
action_68 _ = happyReduce_42

action_69 (41) = happyShift action_71
action_69 _ = happyFail

action_70 _ = happyReduce_23

action_71 _ = happyReduce_34

action_72 (49) = happyShift action_3
action_72 (5) = happyGoto action_68
action_72 (22) = happyGoto action_81
action_72 _ = happyReduce_41

action_73 (42) = happyShift action_80
action_73 _ = happyFail

action_74 (39) = happyShift action_79
action_74 _ = happyReduce_44

action_75 (38) = happyShift action_78
action_75 _ = happyFail

action_76 (36) = happyShift action_77
action_76 _ = happyFail

action_77 _ = happyReduce_31

action_78 _ = happyReduce_26

action_79 (50) = happyShift action_36
action_79 (6) = happyGoto action_73
action_79 (19) = happyGoto action_74
action_79 (23) = happyGoto action_83
action_79 _ = happyFail

action_80 (49) = happyShift action_3
action_80 (5) = happyGoto action_82
action_80 _ = happyFail

action_81 _ = happyReduce_43

action_82 _ = happyReduce_36

action_83 _ = happyReduce_45

happyReduce_2 = happySpecReduce_1 5 happyReduction_2
happyReduction_2 (HappyTerminal (PT _ (TI happy_var_1)))
	 =  HappyAbsSyn5
		 ((read happy_var_1) :: Integer
	)
happyReduction_2 _  = notHappyAtAll 

happyReduce_3 = happySpecReduce_1 6 happyReduction_3
happyReduction_3 (HappyTerminal (PT _ (T_Var happy_var_1)))
	 =  HappyAbsSyn6
		 (Var happy_var_1
	)
happyReduction_3 _  = notHappyAtAll 

happyReduce_4 = happySpecReduce_1 7 happyReduction_4
happyReduction_4 (HappyTerminal (PT _ (T_Pattern happy_var_1)))
	 =  HappyAbsSyn7
		 (Pattern happy_var_1
	)
happyReduction_4 _  = notHappyAtAll 

happyReduce_5 = happySpecReduce_1 8 happyReduction_5
happyReduction_5 (HappyAbsSyn21  happy_var_1)
	 =  HappyAbsSyn8
		 (LTMC (reverse happy_var_1)
	)
happyReduction_5 _  = notHappyAtAll 

happyReduce_6 = happySpecReduce_2 9 happyReduction_6
happyReduction_6 (HappyAbsSyn20  happy_var_2)
	_
	 =  HappyAbsSyn9
		 (Taut (reverse happy_var_2)
	)
happyReduction_6 _ _  = notHappyAtAll 

happyReduce_7 = happySpecReduce_2 9 happyReduction_7
happyReduction_7 (HappyAbsSyn20  happy_var_2)
	_
	 =  HappyAbsSyn9
		 (Nontaut (reverse happy_var_2)
	)
happyReduction_7 _ _  = notHappyAtAll 

happyReduce_8 = happySpecReduce_2 9 happyReduction_8
happyReduction_8 (HappyAbsSyn20  happy_var_2)
	_
	 =  HappyAbsSyn9
		 (Contr (reverse happy_var_2)
	)
happyReduction_8 _ _  = notHappyAtAll 

happyReduce_9 = happySpecReduce_2 9 happyReduction_9
happyReduction_9 (HappyAbsSyn20  happy_var_2)
	_
	 =  HappyAbsSyn9
		 (Noncontr (reverse happy_var_2)
	)
happyReduction_9 _ _  = notHappyAtAll 

happyReduce_10 = happySpecReduce_3 10 happyReduction_10
happyReduction_10 (HappyAbsSyn10  happy_var_3)
	_
	(HappyAbsSyn10  happy_var_1)
	 =  HappyAbsSyn10
		 (EEquiv happy_var_1 happy_var_3
	)
happyReduction_10 _ _ _  = notHappyAtAll 

happyReduce_11 = happySpecReduce_3 10 happyReduction_11
happyReduction_11 (HappyAbsSyn10  happy_var_3)
	_
	(HappyAbsSyn10  happy_var_1)
	 =  HappyAbsSyn10
		 (EImplies happy_var_1 happy_var_3
	)
happyReduction_11 _ _ _  = notHappyAtAll 

happyReduce_12 = happySpecReduce_1 10 happyReduction_12
happyReduction_12 (HappyAbsSyn10  happy_var_1)
	 =  HappyAbsSyn10
		 (happy_var_1
	)
happyReduction_12 _  = notHappyAtAll 

happyReduce_13 = happySpecReduce_3 11 happyReduction_13
happyReduction_13 (HappyAbsSyn10  happy_var_3)
	_
	(HappyAbsSyn10  happy_var_1)
	 =  HappyAbsSyn10
		 (EOr happy_var_1 happy_var_3
	)
happyReduction_13 _ _ _  = notHappyAtAll 

happyReduce_14 = happySpecReduce_3 11 happyReduction_14
happyReduction_14 (HappyAbsSyn10  happy_var_3)
	_
	(HappyAbsSyn10  happy_var_1)
	 =  HappyAbsSyn10
		 (EAnd happy_var_1 happy_var_3
	)
happyReduction_14 _ _ _  = notHappyAtAll 

happyReduce_15 = happySpecReduce_1 11 happyReduction_15
happyReduction_15 (HappyAbsSyn10  happy_var_1)
	 =  HappyAbsSyn10
		 (happy_var_1
	)
happyReduction_15 _  = notHappyAtAll 

happyReduce_16 = happySpecReduce_2 12 happyReduction_16
happyReduction_16 (HappyAbsSyn10  happy_var_2)
	_
	 =  HappyAbsSyn10
		 (ENot happy_var_2
	)
happyReduction_16 _ _  = notHappyAtAll 

happyReduce_17 = happySpecReduce_1 12 happyReduction_17
happyReduction_17 (HappyAbsSyn10  happy_var_1)
	 =  HappyAbsSyn10
		 (happy_var_1
	)
happyReduction_17 _  = notHappyAtAll 

happyReduce_18 = happySpecReduce_1 13 happyReduction_18
happyReduction_18 _
	 =  HappyAbsSyn10
		 (EFalse
	)

happyReduce_19 = happySpecReduce_1 13 happyReduction_19
happyReduction_19 _
	 =  HappyAbsSyn10
		 (ETrue
	)

happyReduce_20 = happySpecReduce_1 13 happyReduction_20
happyReduction_20 (HappyAbsSyn7  happy_var_1)
	 =  HappyAbsSyn10
		 (EPattern happy_var_1
	)
happyReduction_20 _  = notHappyAtAll 

happyReduce_21 = happySpecReduce_1 13 happyReduction_21
happyReduction_21 (HappyAbsSyn16  happy_var_1)
	 =  HappyAbsSyn10
		 (EPred happy_var_1
	)
happyReduction_21 _  = notHappyAtAll 

happyReduce_22 = happySpecReduce_1 13 happyReduction_22
happyReduction_22 (HappyAbsSyn10  happy_var_1)
	 =  HappyAbsSyn10
		 (happy_var_1
	)
happyReduction_22 _  = notHappyAtAll 

happyReduce_23 = happyReduce 4 14 happyReduction_23
happyReduction_23 ((HappyAbsSyn10  happy_var_4) `HappyStk`
	(HappyAbsSyn18  happy_var_3) `HappyStk`
	(HappyAbsSyn6  happy_var_2) `HappyStk`
	_ `HappyStk`
	happyRest)
	 = HappyAbsSyn10
		 (EForAll happy_var_2 happy_var_3 happy_var_4
	) `HappyStk` happyRest

happyReduce_24 = happyReduce 4 14 happyReduction_24
happyReduction_24 ((HappyAbsSyn10  happy_var_4) `HappyStk`
	(HappyAbsSyn18  happy_var_3) `HappyStk`
	(HappyAbsSyn6  happy_var_2) `HappyStk`
	_ `HappyStk`
	happyRest)
	 = HappyAbsSyn10
		 (EExists happy_var_2 happy_var_3 happy_var_4
	) `HappyStk` happyRest

happyReduce_25 = happySpecReduce_1 14 happyReduction_25
happyReduction_25 (HappyAbsSyn10  happy_var_1)
	 =  HappyAbsSyn10
		 (happy_var_1
	)
happyReduction_25 _  = notHappyAtAll 

happyReduce_26 = happyReduce 6 15 happyReduction_26
happyReduction_26 (_ `HappyStk`
	(HappyAbsSyn23  happy_var_5) `HappyStk`
	_ `HappyStk`
	_ `HappyStk`
	(HappyAbsSyn10  happy_var_2) `HappyStk`
	_ `HappyStk`
	happyRest)
	 = HappyAbsSyn10
		 (ECtx happy_var_2 happy_var_5
	) `HappyStk` happyRest

happyReduce_27 = happySpecReduce_3 15 happyReduction_27
happyReduction_27 _
	(HappyAbsSyn10  happy_var_2)
	_
	 =  HappyAbsSyn10
		 (happy_var_2
	)
happyReduction_27 _ _ _  = notHappyAtAll 

happyReduce_28 = happyReduce 4 16 happyReduction_28
happyReduction_28 (_ `HappyStk`
	(HappyAbsSyn17  happy_var_3) `HappyStk`
	_ `HappyStk`
	_ `HappyStk`
	happyRest)
	 = HappyAbsSyn16
		 (EPBlue happy_var_3
	) `HappyStk` happyRest

happyReduce_29 = happyReduce 4 16 happyReduction_29
happyReduction_29 (_ `HappyStk`
	(HappyAbsSyn17  happy_var_3) `HappyStk`
	_ `HappyStk`
	_ `HappyStk`
	happyRest)
	 = HappyAbsSyn16
		 (EPRed happy_var_3
	) `HappyStk` happyRest

happyReduce_30 = happyReduce 4 16 happyReduction_30
happyReduction_30 (_ `HappyStk`
	(HappyAbsSyn17  happy_var_3) `HappyStk`
	_ `HappyStk`
	_ `HappyStk`
	happyRest)
	 = HappyAbsSyn16
		 (EPYellow happy_var_3
	) `HappyStk` happyRest

happyReduce_31 = happyReduce 6 16 happyReduction_31
happyReduction_31 (_ `HappyStk`
	(HappyAbsSyn17  happy_var_5) `HappyStk`
	_ `HappyStk`
	(HappyAbsSyn17  happy_var_3) `HappyStk`
	_ `HappyStk`
	_ `HappyStk`
	happyRest)
	 = HappyAbsSyn16
		 (EPEdge happy_var_3 happy_var_5
	) `HappyStk` happyRest

happyReduce_32 = happySpecReduce_1 17 happyReduction_32
happyReduction_32 (HappyAbsSyn6  happy_var_1)
	 =  HappyAbsSyn17
		 (AVar happy_var_1
	)
happyReduction_32 _  = notHappyAtAll 

happyReduce_33 = happySpecReduce_1 17 happyReduction_33
happyReduction_33 (HappyAbsSyn5  happy_var_1)
	 =  HappyAbsSyn17
		 (AConst happy_var_1
	)
happyReduction_33 _  = notHappyAtAll 

happyReduce_34 = happySpecReduce_3 18 happyReduction_34
happyReduction_34 _
	(HappyAbsSyn22  happy_var_2)
	_
	 =  HappyAbsSyn18
		 (OptDom happy_var_2
	)
happyReduction_34 _ _ _  = notHappyAtAll 

happyReduce_35 = happySpecReduce_0 18 happyReduction_35
happyReduction_35  =  HappyAbsSyn18
		 (EmptyOptDom
	)

happyReduce_36 = happySpecReduce_3 19 happyReduction_36
happyReduction_36 (HappyAbsSyn5  happy_var_3)
	_
	(HappyAbsSyn6  happy_var_1)
	 =  HappyAbsSyn19
		 (Ctxt happy_var_1 happy_var_3
	)
happyReduction_36 _ _ _  = notHappyAtAll 

happyReduce_37 = happySpecReduce_0 20 happyReduction_37
happyReduction_37  =  HappyAbsSyn20
		 ([]
	)

happyReduce_38 = happySpecReduce_2 20 happyReduction_38
happyReduction_38 (HappyAbsSyn10  happy_var_2)
	(HappyAbsSyn20  happy_var_1)
	 =  HappyAbsSyn20
		 (flip (:) happy_var_1 happy_var_2
	)
happyReduction_38 _ _  = notHappyAtAll 

happyReduce_39 = happySpecReduce_0 21 happyReduction_39
happyReduction_39  =  HappyAbsSyn21
		 ([]
	)

happyReduce_40 = happySpecReduce_2 21 happyReduction_40
happyReduction_40 (HappyAbsSyn9  happy_var_2)
	(HappyAbsSyn21  happy_var_1)
	 =  HappyAbsSyn21
		 (flip (:) happy_var_1 happy_var_2
	)
happyReduction_40 _ _  = notHappyAtAll 

happyReduce_41 = happySpecReduce_0 22 happyReduction_41
happyReduction_41  =  HappyAbsSyn22
		 ([]
	)

happyReduce_42 = happySpecReduce_1 22 happyReduction_42
happyReduction_42 (HappyAbsSyn5  happy_var_1)
	 =  HappyAbsSyn22
		 ((:[]) happy_var_1
	)
happyReduction_42 _  = notHappyAtAll 

happyReduce_43 = happySpecReduce_3 22 happyReduction_43
happyReduction_43 (HappyAbsSyn22  happy_var_3)
	_
	(HappyAbsSyn5  happy_var_1)
	 =  HappyAbsSyn22
		 ((:) happy_var_1 happy_var_3
	)
happyReduction_43 _ _ _  = notHappyAtAll 

happyReduce_44 = happySpecReduce_1 23 happyReduction_44
happyReduction_44 (HappyAbsSyn19  happy_var_1)
	 =  HappyAbsSyn23
		 ((:[]) happy_var_1
	)
happyReduction_44 _  = notHappyAtAll 

happyReduce_45 = happySpecReduce_3 23 happyReduction_45
happyReduction_45 (HappyAbsSyn23  happy_var_3)
	_
	(HappyAbsSyn19  happy_var_1)
	 =  HappyAbsSyn23
		 ((:) happy_var_1 happy_var_3
	)
happyReduction_45 _ _ _  = notHappyAtAll 

happyNewToken action sts stk [] =
	action 53 53 (error "reading EOF!") (HappyState action) sts stk []

happyNewToken action sts stk (tk:tks) =
	let cont i = action i i tk (HappyState action) sts stk tks in
	case tk of {
	PT _ (TS "Tautologies:") -> cont 24;
	PT _ (TS "Non-tautologies:") -> cont 25;
	PT _ (TS "Contradictions:") -> cont 26;
	PT _ (TS "Non-contradictions:") -> cont 27;
	PT _ (TS "<->") -> cont 28;
	PT _ (TS "->") -> cont 29;
	PT _ (TS "|") -> cont 30;
	PT _ (TS "&") -> cont 31;
	PT _ (TS "~") -> cont 32;
	PT _ (TS ">") -> cont 33;
	PT _ (TS "<") -> cont 34;
	PT _ (TS "(") -> cont 35;
	PT _ (TS ")") -> cont 36;
	PT _ (TS "[") -> cont 37;
	PT _ (TS "]") -> cont 38;
	PT _ (TS ",") -> cont 39;
	PT _ (TS "{") -> cont 40;
	PT _ (TS "}") -> cont 41;
	PT _ (TS "=") -> cont 42;
	PT _ (TS "Blue") -> cont 43;
	PT _ (TS "E") -> cont 44;
	PT _ (TS "Red") -> cont 45;
	PT _ (TS "Yellow") -> cont 46;
	PT _ (TS "false") -> cont 47;
	PT _ (TS "true") -> cont 48;
	PT _ (TI happy_dollar_dollar) -> cont 49;
	PT _ (T_Var happy_dollar_dollar) -> cont 50;
	PT _ (T_Pattern happy_dollar_dollar) -> cont 51;
	_ -> cont 52;
	_ -> happyError tks
	}

happyThen :: Err a -> (a -> Err b) -> Err b
happyThen = (thenM)
happyReturn :: a -> Err a
happyReturn = (returnM)
happyThen1 m k tks = (thenM) m (\a -> k a tks)
happyReturn1 = \a tks -> (returnM) a

pLtm tks = happyThen (happyParse action_0 tks) (\x -> case x of {HappyAbsSyn8 z -> happyReturn z; _other -> notHappyAtAll })

pExp tks = happyThen (happyParse action_1 tks) (\x -> case x of {HappyAbsSyn10 z -> happyReturn z; _other -> notHappyAtAll })

happySeq = happyDontSeq

returnM :: a -> Err a
returnM = return

thenM :: Err a -> (a -> Err b) -> Err b
thenM = (>>=)

happyError :: [Token] -> Err a
happyError ts =
  Bad $ "syntax error at " ++ tokenPos ts ++ if null ts then [] else (" before " ++ unwords (map prToken (take 4 ts)))

myLexer = tokens
{-# LINE 1 "GenericTemplate.hs" #-}
-- $Id: GenericTemplate.hs,v 1.23 2002/05/23 09:24:27 simonmar Exp $

{-# LINE 15 "GenericTemplate.hs" #-}






















































infixr 9 `HappyStk`
data HappyStk a = HappyStk a (HappyStk a)

-----------------------------------------------------------------------------
-- starting the parse

happyParse start_state = happyNewToken start_state notHappyAtAll notHappyAtAll

-----------------------------------------------------------------------------
-- Accepting the parse

happyAccept j tk st sts (HappyStk ans _) = 

					   (happyReturn1 ans)

-----------------------------------------------------------------------------
-- Arrays only: do the next action

{-# LINE 150 "GenericTemplate.hs" #-}


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
     = happyFail (1) tk st sts stk
happySpecReduce_0 nt fn j tk st@((HappyState (action))) sts stk
     = action nt j tk st ((st):(sts)) (fn `HappyStk` stk)

happySpecReduce_1 i fn (1) tk st sts stk
     = happyFail (1) tk st sts stk
happySpecReduce_1 nt fn j tk _ sts@(((st@(HappyState (action))):(_))) (v1`HappyStk`stk')
     = let r = fn v1 in
       happySeq r (action nt j tk st sts (r `HappyStk` stk'))

happySpecReduce_2 i fn (1) tk st sts stk
     = happyFail (1) tk st sts stk
happySpecReduce_2 nt fn j tk _ ((_):(sts@(((st@(HappyState (action))):(_))))) (v1`HappyStk`v2`HappyStk`stk')
     = let r = fn v1 v2 in
       happySeq r (action nt j tk st sts (r `HappyStk` stk'))

happySpecReduce_3 i fn (1) tk st sts stk
     = happyFail (1) tk st sts stk
happySpecReduce_3 nt fn j tk _ ((_):(((_):(sts@(((st@(HappyState (action))):(_))))))) (v1`HappyStk`v2`HappyStk`v3`HappyStk`stk')
     = let r = fn v1 v2 v3 in
       happySeq r (action nt j tk st sts (r `HappyStk` stk'))

happyReduce k i fn (1) tk st sts stk
     = happyFail (1) tk st sts stk
happyReduce k nt fn j tk st sts stk
     = case happyDrop (k - ((1) :: Int)) sts of
	 sts1@(((st1@(HappyState (action))):(_))) ->
        	let r = fn stk in  -- it doesn't hurt to always seq here...
       		happyDoSeq r (action nt j tk st1 sts1 r)

happyMonadReduce k nt fn (1) tk st sts stk
     = happyFail (1) tk st sts stk
happyMonadReduce k nt fn j tk st sts stk =
        happyThen1 (fn stk) (\r -> action nt j tk st1 sts1 (r `HappyStk` drop_stk))
       where sts1@(((st1@(HappyState (action))):(_))) = happyDrop k ((st):(sts))
             drop_stk = happyDropStk k stk

happyDrop (0) l = l
happyDrop n ((_):(t)) = happyDrop (n - ((1) :: Int)) t

happyDropStk (0) l = l
happyDropStk n (x `HappyStk` xs) = happyDropStk (n - ((1)::Int)) xs

-----------------------------------------------------------------------------
-- Moving to a new state after a reduction









happyGoto action j tk st = action j j tk (HappyState action)


-----------------------------------------------------------------------------
-- Error recovery ((1) is the error token)

-- parse error if we are in recovery and we fail again
happyFail  (1) tk old_st _ stk =
--	trace "failing" $ 
    	happyError


{-  We don't need state discarding for our restricted implementation of
    "error".  In fact, it can cause some bogus parses, so I've disabled it
    for now --SDM

-- discard a state
happyFail  (1) tk old_st (((HappyState (action))):(sts)) 
						(saved_tok `HappyStk` _ `HappyStk` stk) =
--	trace ("discarding state, depth " ++ show (length stk))  $
	action (1) (1) tk (HappyState (action)) sts ((saved_tok`HappyStk`stk))
-}

-- Enter error recovery: generate an error token,
--                       save the old token and carry on.
happyFail  i tk (HappyState (action)) sts stk =
--      trace "entering error recovery" $
	action (1) (1) tk (HappyState (action)) sts ( (HappyErrorToken (i)) `HappyStk` stk)

-- Internal happy errors:

notHappyAtAll = error "Internal Happy error\n"

-----------------------------------------------------------------------------
-- Hack to get the typechecker to accept our action functions







-----------------------------------------------------------------------------
-- Seq-ing.  If the --strict flag is given, then Happy emits 
--	happySeq = happyDoSeq
-- otherwise it emits
-- 	happySeq = happyDontSeq

happyDoSeq, happyDontSeq :: a -> b -> b
happyDoSeq   a b = a `seq` b
happyDontSeq a b = b

-----------------------------------------------------------------------------
-- Don't inline any functions from the template.  GHC has a nasty habit
-- of deciding to inline happyGoto everywhere, which increases the size of
-- the generated parser quite a bit.









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
