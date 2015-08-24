{-# OPTIONS_GHC -w #-}
{-# LANGUAGE BangPatterns #-} -- required for versions of Happy before 1.18.6
{-# OPTIONS -w -Wwarn #-}
-- The above warning supression flag is a temporary kludge.
-- While working on this module you are encouraged to remove it and fix
-- any warnings in the module. See
--     http://ghc.haskell.org/trac/ghc/wiki/Commentary/CodingStyle#Warnings
-- for details

module Parser (parse) where

import Lexer (lex_tok)
import ParserM (Token(..), ParserM, run_parser, get_pos, show_pos,
                happyError)
import Syntax

-- parser produced by Happy Version 1.19.3

data HappyAbsSyn 
	= HappyTerminal (Token)
	| HappyErrorToken Int
	| HappyAbsSyn4 (Info)
	| HappyAbsSyn5 ([Option])
	| HappyAbsSyn7 (Option)
	| HappyAbsSyn8 (Maybe Fixity)
	| HappyAbsSyn9 ([Entry])
	| HappyAbsSyn10 (Entry)
	| HappyAbsSyn16 (Category)
	| HappyAbsSyn17 (String)
	| HappyAbsSyn21 ([(String, String, Int)])
	| HappyAbsSyn23 ((String, String, Int))
	| HappyAbsSyn24 (Ty)
	| HappyAbsSyn27 ([Ty])
	| HappyAbsSyn30 (TyCon)

{- to allow type-synonyms as our monads (likely
 - with explicitly-specified bind and return)
 - in Haskell98, it seems that with
 - /type M a = .../, then /(HappyReduction M)/
 - is not allowed.  But Happy is a
 - code-generator that can just substitute it.
type HappyReduction m = 
	   Int 
	-> (Token)
	-> HappyState (Token) (HappyStk HappyAbsSyn -> m HappyAbsSyn)
	-> [HappyState (Token) (HappyStk HappyAbsSyn -> m HappyAbsSyn)] 
	-> HappyStk HappyAbsSyn 
	-> m HappyAbsSyn
-}

action_0,
 action_1,
 action_2,
 action_3,
 action_4,
 action_5,
 action_6,
 action_7,
 action_8,
 action_9,
 action_10,
 action_11,
 action_12,
 action_13,
 action_14,
 action_15,
 action_16,
 action_17,
 action_18,
 action_19,
 action_20,
 action_21,
 action_22,
 action_23,
 action_24,
 action_25,
 action_26,
 action_27,
 action_28,
 action_29,
 action_30,
 action_31,
 action_32,
 action_33,
 action_34,
 action_35,
 action_36,
 action_37,
 action_38,
 action_39,
 action_40,
 action_41,
 action_42,
 action_43,
 action_44,
 action_45,
 action_46,
 action_47,
 action_48,
 action_49,
 action_50,
 action_51,
 action_52,
 action_53,
 action_54,
 action_55,
 action_56,
 action_57,
 action_58,
 action_59,
 action_60,
 action_61,
 action_62,
 action_63,
 action_64,
 action_65,
 action_66,
 action_67,
 action_68,
 action_69,
 action_70,
 action_71,
 action_72,
 action_73,
 action_74,
 action_75,
 action_76,
 action_77,
 action_78,
 action_79,
 action_80,
 action_81,
 action_82,
 action_83,
 action_84,
 action_85,
 action_86,
 action_87,
 action_88,
 action_89,
 action_90,
 action_91,
 action_92,
 action_93,
 action_94,
 action_95,
 action_96,
 action_97,
 action_98,
 action_99,
 action_100,
 action_101,
 action_102,
 action_103,
 action_104,
 action_105,
 action_106,
 action_107,
 action_108,
 action_109,
 action_110 :: () => Int -> ({-HappyReduction (ParserM) = -}
	   Int 
	-> (Token)
	-> HappyState (Token) (HappyStk HappyAbsSyn -> (ParserM) HappyAbsSyn)
	-> [HappyState (Token) (HappyStk HappyAbsSyn -> (ParserM) HappyAbsSyn)] 
	-> HappyStk HappyAbsSyn 
	-> (ParserM) HappyAbsSyn)

happyReduce_1,
 happyReduce_2,
 happyReduce_3,
 happyReduce_4,
 happyReduce_5,
 happyReduce_6,
 happyReduce_7,
 happyReduce_8,
 happyReduce_9,
 happyReduce_10,
 happyReduce_11,
 happyReduce_12,
 happyReduce_13,
 happyReduce_14,
 happyReduce_15,
 happyReduce_16,
 happyReduce_17,
 happyReduce_18,
 happyReduce_19,
 happyReduce_20,
 happyReduce_21,
 happyReduce_22,
 happyReduce_23,
 happyReduce_24,
 happyReduce_25,
 happyReduce_26,
 happyReduce_27,
 happyReduce_28,
 happyReduce_29,
 happyReduce_30,
 happyReduce_31,
 happyReduce_32,
 happyReduce_33,
 happyReduce_34,
 happyReduce_35,
 happyReduce_36,
 happyReduce_37,
 happyReduce_38,
 happyReduce_39,
 happyReduce_40,
 happyReduce_41,
 happyReduce_42,
 happyReduce_43,
 happyReduce_44,
 happyReduce_45,
 happyReduce_46,
 happyReduce_47,
 happyReduce_48,
 happyReduce_49,
 happyReduce_50,
 happyReduce_51,
 happyReduce_52,
 happyReduce_53,
 happyReduce_54,
 happyReduce_55,
 happyReduce_56,
 happyReduce_57,
 happyReduce_58,
 happyReduce_59,
 happyReduce_60,
 happyReduce_61 :: () => ({-HappyReduction (ParserM) = -}
	   Int 
	-> (Token)
	-> HappyState (Token) (HappyStk HappyAbsSyn -> (ParserM) HappyAbsSyn)
	-> [HappyState (Token) (HappyStk HappyAbsSyn -> (ParserM) HappyAbsSyn)] 
	-> HappyStk HappyAbsSyn 
	-> (ParserM) HappyAbsSyn)

action_0 (50) = happyShift action_3
action_0 (4) = happyGoto action_4
action_0 (5) = happyGoto action_2
action_0 _ = happyFail

action_1 (50) = happyShift action_3
action_1 (5) = happyGoto action_2
action_1 _ = happyFail

action_2 (45) = happyShift action_16
action_2 (46) = happyShift action_17
action_2 (47) = happyShift action_18
action_2 (48) = happyShift action_19
action_2 (9) = happyGoto action_10
action_2 (10) = happyGoto action_11
action_2 (11) = happyGoto action_12
action_2 (12) = happyGoto action_13
action_2 (13) = happyGoto action_14
action_2 (14) = happyGoto action_15
action_2 _ = happyReduce_16

action_3 (57) = happyShift action_7
action_3 (62) = happyShift action_8
action_3 (67) = happyShift action_9
action_3 (6) = happyGoto action_5
action_3 (7) = happyGoto action_6
action_3 _ = happyReduce_4

action_4 (72) = happyAccept
action_4 _ = happyFail

action_5 _ = happyReduce_2

action_6 (57) = happyShift action_7
action_6 (62) = happyShift action_8
action_6 (67) = happyShift action_9
action_6 (6) = happyGoto action_39
action_6 (7) = happyGoto action_6
action_6 _ = happyReduce_4

action_7 (33) = happyShift action_38
action_7 _ = happyFail

action_8 (33) = happyShift action_37
action_8 _ = happyFail

action_9 (33) = happyShift action_36
action_9 _ = happyFail

action_10 (66) = happyShift action_35
action_10 _ = happyFail

action_11 (45) = happyShift action_16
action_11 (46) = happyShift action_17
action_11 (47) = happyShift action_18
action_11 (48) = happyShift action_19
action_11 (9) = happyGoto action_34
action_11 (10) = happyGoto action_11
action_11 (11) = happyGoto action_12
action_11 (12) = happyGoto action_13
action_11 (13) = happyGoto action_14
action_11 (14) = happyGoto action_15
action_11 _ = happyReduce_16

action_12 _ = happyReduce_17

action_13 _ = happyReduce_18

action_14 _ = happyReduce_19

action_15 _ = happyReduce_20

action_16 (69) = happyShift action_33
action_16 _ = happyFail

action_17 (68) = happyShift action_32
action_17 _ = happyFail

action_18 (69) = happyShift action_31
action_18 _ = happyFail

action_19 (35) = happyShift action_24
action_19 (37) = happyShift action_25
action_19 (63) = happyShift action_26
action_19 (64) = happyShift action_27
action_19 (65) = happyShift action_28
action_19 (67) = happyShift action_29
action_19 (68) = happyShift action_30
action_19 (24) = happyGoto action_20
action_19 (25) = happyGoto action_21
action_19 (26) = happyGoto action_22
action_19 (30) = happyGoto action_23
action_19 _ = happyFail

action_20 (39) = happyShift action_48
action_20 (17) = happyGoto action_67
action_20 (18) = happyGoto action_53
action_20 _ = happyReduce_32

action_21 (31) = happyShift action_65
action_21 (32) = happyShift action_66
action_21 _ = happyReduce_45

action_22 _ = happyReduce_47

action_23 (35) = happyShift action_63
action_23 (63) = happyShift action_26
action_23 (64) = happyShift action_27
action_23 (65) = happyShift action_28
action_23 (67) = happyShift action_64
action_23 (68) = happyShift action_30
action_23 (28) = happyGoto action_60
action_23 (29) = happyGoto action_61
action_23 (30) = happyGoto action_62
action_23 _ = happyReduce_54

action_24 (35) = happyShift action_24
action_24 (36) = happyShift action_59
action_24 (37) = happyShift action_25
action_24 (63) = happyShift action_26
action_24 (64) = happyShift action_27
action_24 (65) = happyShift action_28
action_24 (67) = happyShift action_29
action_24 (68) = happyShift action_30
action_24 (24) = happyGoto action_58
action_24 (25) = happyGoto action_21
action_24 (26) = happyGoto action_22
action_24 (30) = happyGoto action_23
action_24 _ = happyFail

action_25 (35) = happyShift action_24
action_25 (37) = happyShift action_25
action_25 (63) = happyShift action_26
action_25 (64) = happyShift action_27
action_25 (65) = happyShift action_28
action_25 (67) = happyShift action_29
action_25 (68) = happyShift action_30
action_25 (24) = happyGoto action_56
action_25 (25) = happyGoto action_21
action_25 (26) = happyGoto action_22
action_25 (27) = happyGoto action_57
action_25 (30) = happyGoto action_23
action_25 _ = happyFail

action_26 _ = happyReduce_59

action_27 _ = happyReduce_60

action_28 _ = happyReduce_61

action_29 _ = happyReduce_49

action_30 _ = happyReduce_57

action_31 (35) = happyShift action_24
action_31 (37) = happyShift action_25
action_31 (63) = happyShift action_26
action_31 (64) = happyShift action_27
action_31 (65) = happyShift action_28
action_31 (67) = happyShift action_29
action_31 (68) = happyShift action_30
action_31 (24) = happyGoto action_55
action_31 (25) = happyGoto action_21
action_31 (26) = happyGoto action_22
action_31 (30) = happyGoto action_23
action_31 _ = happyFail

action_32 (69) = happyShift action_54
action_32 _ = happyFail

action_33 (39) = happyShift action_48
action_33 (17) = happyGoto action_52
action_33 (18) = happyGoto action_53
action_33 _ = happyReduce_32

action_34 _ = happyReduce_15

action_35 _ = happyReduce_1

action_36 (39) = happyShift action_48
action_36 (51) = happyShift action_49
action_36 (52) = happyShift action_50
action_36 (70) = happyShift action_51
action_36 (18) = happyGoto action_47
action_36 _ = happyFail

action_37 (41) = happyShift action_46
action_37 (21) = happyGoto action_45
action_37 _ = happyFail

action_38 (58) = happyShift action_41
action_38 (59) = happyShift action_42
action_38 (60) = happyShift action_43
action_38 (61) = happyShift action_44
action_38 (8) = happyGoto action_40
action_38 _ = happyFail

action_39 _ = happyReduce_3

action_40 _ = happyReduce_10

action_41 (70) = happyShift action_91
action_41 _ = happyFail

action_42 (70) = happyShift action_90
action_42 _ = happyFail

action_43 (70) = happyShift action_89
action_43 _ = happyFail

action_44 _ = happyReduce_14

action_45 _ = happyReduce_9

action_46 (43) = happyShift action_88
action_46 (22) = happyGoto action_86
action_46 (23) = happyGoto action_87
action_46 _ = happyReduce_41

action_47 _ = happyReduce_7

action_48 (39) = happyShift action_84
action_48 (71) = happyShift action_85
action_48 (19) = happyGoto action_82
action_48 (20) = happyGoto action_83
action_48 _ = happyReduce_35

action_49 _ = happyReduce_6

action_50 _ = happyReduce_5

action_51 _ = happyReduce_8

action_52 _ = happyReduce_24

action_53 _ = happyReduce_31

action_54 (53) = happyShift action_78
action_54 (54) = happyShift action_79
action_54 (55) = happyShift action_80
action_54 (56) = happyShift action_81
action_54 (16) = happyGoto action_77
action_54 _ = happyFail

action_55 (39) = happyShift action_48
action_55 (17) = happyGoto action_76
action_55 (18) = happyGoto action_53
action_55 _ = happyReduce_32

action_56 (34) = happyShift action_75
action_56 _ = happyReduce_52

action_57 (38) = happyShift action_74
action_57 _ = happyFail

action_58 (36) = happyShift action_73
action_58 _ = happyFail

action_59 _ = happyReduce_58

action_60 _ = happyReduce_46

action_61 (35) = happyShift action_63
action_61 (63) = happyShift action_26
action_61 (64) = happyShift action_27
action_61 (65) = happyShift action_28
action_61 (67) = happyShift action_64
action_61 (68) = happyShift action_30
action_61 (28) = happyGoto action_72
action_61 (29) = happyGoto action_61
action_61 (30) = happyGoto action_62
action_61 _ = happyReduce_54

action_62 _ = happyReduce_56

action_63 (36) = happyShift action_59
action_63 _ = happyFail

action_64 _ = happyReduce_55

action_65 (35) = happyShift action_24
action_65 (37) = happyShift action_25
action_65 (63) = happyShift action_26
action_65 (64) = happyShift action_27
action_65 (65) = happyShift action_28
action_65 (67) = happyShift action_29
action_65 (68) = happyShift action_30
action_65 (24) = happyGoto action_71
action_65 (25) = happyGoto action_21
action_65 (26) = happyGoto action_22
action_65 (30) = happyGoto action_23
action_65 _ = happyFail

action_66 (35) = happyShift action_24
action_66 (37) = happyShift action_25
action_66 (63) = happyShift action_26
action_66 (64) = happyShift action_27
action_66 (65) = happyShift action_28
action_66 (67) = happyShift action_29
action_66 (68) = happyShift action_30
action_66 (24) = happyGoto action_70
action_66 (25) = happyGoto action_21
action_66 (26) = happyGoto action_22
action_66 (30) = happyGoto action_23
action_66 _ = happyFail

action_67 (49) = happyShift action_69
action_67 (15) = happyGoto action_68
action_67 _ = happyReduce_26

action_68 _ = happyReduce_22

action_69 (57) = happyShift action_7
action_69 (62) = happyShift action_8
action_69 (67) = happyShift action_9
action_69 (6) = happyGoto action_101
action_69 (7) = happyGoto action_6
action_69 _ = happyReduce_4

action_70 _ = happyReduce_44

action_71 _ = happyReduce_43

action_72 _ = happyReduce_53

action_73 _ = happyReduce_48

action_74 _ = happyReduce_50

action_75 (35) = happyShift action_24
action_75 (37) = happyShift action_25
action_75 (63) = happyShift action_26
action_75 (64) = happyShift action_27
action_75 (65) = happyShift action_28
action_75 (67) = happyShift action_29
action_75 (68) = happyShift action_30
action_75 (24) = happyGoto action_56
action_75 (25) = happyGoto action_21
action_75 (26) = happyGoto action_22
action_75 (27) = happyGoto action_100
action_75 (30) = happyGoto action_23
action_75 _ = happyFail

action_76 (49) = happyShift action_69
action_76 (15) = happyGoto action_99
action_76 _ = happyReduce_26

action_77 (35) = happyShift action_24
action_77 (37) = happyShift action_25
action_77 (63) = happyShift action_26
action_77 (64) = happyShift action_27
action_77 (65) = happyShift action_28
action_77 (67) = happyShift action_29
action_77 (68) = happyShift action_30
action_77 (24) = happyGoto action_98
action_77 (25) = happyGoto action_21
action_77 (26) = happyGoto action_22
action_77 (30) = happyGoto action_23
action_77 _ = happyFail

action_78 _ = happyReduce_27

action_79 _ = happyReduce_28

action_80 _ = happyReduce_29

action_81 _ = happyReduce_30

action_82 (40) = happyShift action_97
action_82 _ = happyFail

action_83 (39) = happyShift action_84
action_83 (71) = happyShift action_85
action_83 (19) = happyGoto action_96
action_83 (20) = happyGoto action_83
action_83 _ = happyReduce_35

action_84 (39) = happyShift action_84
action_84 (71) = happyShift action_85
action_84 (19) = happyGoto action_95
action_84 (20) = happyGoto action_83
action_84 _ = happyReduce_35

action_85 _ = happyReduce_37

action_86 (42) = happyShift action_94
action_86 _ = happyFail

action_87 (34) = happyShift action_93
action_87 _ = happyReduce_40

action_88 (68) = happyShift action_92
action_88 _ = happyFail

action_89 _ = happyReduce_13

action_90 _ = happyReduce_12

action_91 _ = happyReduce_11

action_92 (34) = happyShift action_105
action_92 _ = happyFail

action_93 (43) = happyShift action_88
action_93 (22) = happyGoto action_104
action_93 (23) = happyGoto action_87
action_93 _ = happyReduce_41

action_94 _ = happyReduce_38

action_95 (40) = happyShift action_103
action_95 _ = happyFail

action_96 _ = happyReduce_34

action_97 _ = happyReduce_33

action_98 (39) = happyShift action_48
action_98 (17) = happyGoto action_102
action_98 (18) = happyGoto action_53
action_98 _ = happyReduce_32

action_99 _ = happyReduce_23

action_100 _ = happyReduce_51

action_101 _ = happyReduce_25

action_102 (49) = happyShift action_69
action_102 (15) = happyGoto action_107
action_102 _ = happyReduce_26

action_103 _ = happyReduce_36

action_104 _ = happyReduce_39

action_105 (68) = happyShift action_106
action_105 _ = happyFail

action_106 (34) = happyShift action_108
action_106 _ = happyFail

action_107 _ = happyReduce_21

action_108 (70) = happyShift action_109
action_108 _ = happyFail

action_109 (44) = happyShift action_110
action_109 _ = happyFail

action_110 _ = happyReduce_42

happyReduce_1 = happySpecReduce_3  4 happyReduction_1
happyReduction_1 _
	(HappyAbsSyn9  happy_var_2)
	(HappyAbsSyn5  happy_var_1)
	 =  HappyAbsSyn4
		 (Info happy_var_1 happy_var_2
	)
happyReduction_1 _ _ _  = notHappyAtAll 

happyReduce_2 = happySpecReduce_2  5 happyReduction_2
happyReduction_2 (HappyAbsSyn5  happy_var_2)
	_
	 =  HappyAbsSyn5
		 (happy_var_2
	)
happyReduction_2 _ _  = notHappyAtAll 

happyReduce_3 = happySpecReduce_2  6 happyReduction_3
happyReduction_3 (HappyAbsSyn5  happy_var_2)
	(HappyAbsSyn7  happy_var_1)
	 =  HappyAbsSyn5
		 (happy_var_1 : happy_var_2
	)
happyReduction_3 _ _  = notHappyAtAll 

happyReduce_4 = happySpecReduce_0  6 happyReduction_4
happyReduction_4  =  HappyAbsSyn5
		 ([]
	)

happyReduce_5 = happySpecReduce_3  7 happyReduction_5
happyReduction_5 _
	_
	(HappyTerminal (TLowerName happy_var_1))
	 =  HappyAbsSyn7
		 (OptionFalse  happy_var_1
	)
happyReduction_5 _ _ _  = notHappyAtAll 

happyReduce_6 = happySpecReduce_3  7 happyReduction_6
happyReduction_6 _
	_
	(HappyTerminal (TLowerName happy_var_1))
	 =  HappyAbsSyn7
		 (OptionTrue   happy_var_1
	)
happyReduction_6 _ _ _  = notHappyAtAll 

happyReduce_7 = happySpecReduce_3  7 happyReduction_7
happyReduction_7 (HappyAbsSyn17  happy_var_3)
	_
	(HappyTerminal (TLowerName happy_var_1))
	 =  HappyAbsSyn7
		 (OptionString happy_var_1 happy_var_3
	)
happyReduction_7 _ _ _  = notHappyAtAll 

happyReduce_8 = happySpecReduce_3  7 happyReduction_8
happyReduction_8 (HappyTerminal (TInteger happy_var_3))
	_
	(HappyTerminal (TLowerName happy_var_1))
	 =  HappyAbsSyn7
		 (OptionInteger happy_var_1 happy_var_3
	)
happyReduction_8 _ _ _  = notHappyAtAll 

happyReduce_9 = happySpecReduce_3  7 happyReduction_9
happyReduction_9 (HappyAbsSyn21  happy_var_3)
	_
	_
	 =  HappyAbsSyn7
		 (OptionVector happy_var_3
	)
happyReduction_9 _ _ _  = notHappyAtAll 

happyReduce_10 = happySpecReduce_3  7 happyReduction_10
happyReduction_10 (HappyAbsSyn8  happy_var_3)
	_
	_
	 =  HappyAbsSyn7
		 (OptionFixity happy_var_3
	)
happyReduction_10 _ _ _  = notHappyAtAll 

happyReduce_11 = happySpecReduce_2  8 happyReduction_11
happyReduction_11 (HappyTerminal (TInteger happy_var_2))
	_
	 =  HappyAbsSyn8
		 (Just $ Fixity happy_var_2 InfixN
	)
happyReduction_11 _ _  = notHappyAtAll 

happyReduce_12 = happySpecReduce_2  8 happyReduction_12
happyReduction_12 (HappyTerminal (TInteger happy_var_2))
	_
	 =  HappyAbsSyn8
		 (Just $ Fixity happy_var_2 InfixL
	)
happyReduction_12 _ _  = notHappyAtAll 

happyReduce_13 = happySpecReduce_2  8 happyReduction_13
happyReduction_13 (HappyTerminal (TInteger happy_var_2))
	_
	 =  HappyAbsSyn8
		 (Just $ Fixity happy_var_2 InfixR
	)
happyReduction_13 _ _  = notHappyAtAll 

happyReduce_14 = happySpecReduce_1  8 happyReduction_14
happyReduction_14 _
	 =  HappyAbsSyn8
		 (Nothing
	)

happyReduce_15 = happySpecReduce_2  9 happyReduction_15
happyReduction_15 (HappyAbsSyn9  happy_var_2)
	(HappyAbsSyn10  happy_var_1)
	 =  HappyAbsSyn9
		 (happy_var_1 : happy_var_2
	)
happyReduction_15 _ _  = notHappyAtAll 

happyReduce_16 = happySpecReduce_0  9 happyReduction_16
happyReduction_16  =  HappyAbsSyn9
		 ([]
	)

happyReduce_17 = happySpecReduce_1  10 happyReduction_17
happyReduction_17 (HappyAbsSyn10  happy_var_1)
	 =  HappyAbsSyn10
		 (happy_var_1
	)
happyReduction_17 _  = notHappyAtAll 

happyReduce_18 = happySpecReduce_1  10 happyReduction_18
happyReduction_18 (HappyAbsSyn10  happy_var_1)
	 =  HappyAbsSyn10
		 (happy_var_1
	)
happyReduction_18 _  = notHappyAtAll 

happyReduce_19 = happySpecReduce_1  10 happyReduction_19
happyReduction_19 (HappyAbsSyn10  happy_var_1)
	 =  HappyAbsSyn10
		 (happy_var_1
	)
happyReduction_19 _  = notHappyAtAll 

happyReduce_20 = happySpecReduce_1  10 happyReduction_20
happyReduction_20 (HappyAbsSyn10  happy_var_1)
	 =  HappyAbsSyn10
		 (happy_var_1
	)
happyReduction_20 _  = notHappyAtAll 

happyReduce_21 = happyReduce 7 11 happyReduction_21
happyReduction_21 ((HappyAbsSyn5  happy_var_7) `HappyStk`
	(HappyAbsSyn17  happy_var_6) `HappyStk`
	(HappyAbsSyn24  happy_var_5) `HappyStk`
	(HappyAbsSyn16  happy_var_4) `HappyStk`
	(HappyTerminal (TString happy_var_3)) `HappyStk`
	(HappyTerminal (TUpperName happy_var_2)) `HappyStk`
	_ `HappyStk`
	happyRest)
	 = HappyAbsSyn10
		 (PrimOpSpec {
                    cons = happy_var_2,
                    name = happy_var_3,
                    cat = happy_var_4,
                    ty = happy_var_5,
                    desc = happy_var_6,
                    opts = happy_var_7
                }
	) `HappyStk` happyRest

happyReduce_22 = happyReduce 4 12 happyReduction_22
happyReduction_22 ((HappyAbsSyn5  happy_var_4) `HappyStk`
	(HappyAbsSyn17  happy_var_3) `HappyStk`
	(HappyAbsSyn24  happy_var_2) `HappyStk`
	_ `HappyStk`
	happyRest)
	 = HappyAbsSyn10
		 (PrimTypeSpec { ty = happy_var_2, desc = happy_var_3, opts = happy_var_4 }
	) `HappyStk` happyRest

happyReduce_23 = happyReduce 5 13 happyReduction_23
happyReduction_23 ((HappyAbsSyn5  happy_var_5) `HappyStk`
	(HappyAbsSyn17  happy_var_4) `HappyStk`
	(HappyAbsSyn24  happy_var_3) `HappyStk`
	(HappyTerminal (TString happy_var_2)) `HappyStk`
	_ `HappyStk`
	happyRest)
	 = HappyAbsSyn10
		 (PseudoOpSpec { name = happy_var_2, ty = happy_var_3, desc = happy_var_4, opts = happy_var_5 }
	) `HappyStk` happyRest

happyReduce_24 = happySpecReduce_3  14 happyReduction_24
happyReduction_24 (HappyAbsSyn17  happy_var_3)
	(HappyTerminal (TString happy_var_2))
	_
	 =  HappyAbsSyn10
		 (Section { title = happy_var_2, desc = happy_var_3 }
	)
happyReduction_24 _ _ _  = notHappyAtAll 

happyReduce_25 = happySpecReduce_2  15 happyReduction_25
happyReduction_25 (HappyAbsSyn5  happy_var_2)
	_
	 =  HappyAbsSyn5
		 (happy_var_2
	)
happyReduction_25 _ _  = notHappyAtAll 

happyReduce_26 = happySpecReduce_0  15 happyReduction_26
happyReduction_26  =  HappyAbsSyn5
		 ([]
	)

happyReduce_27 = happySpecReduce_1  16 happyReduction_27
happyReduction_27 _
	 =  HappyAbsSyn16
		 (Dyadic
	)

happyReduce_28 = happySpecReduce_1  16 happyReduction_28
happyReduction_28 _
	 =  HappyAbsSyn16
		 (Monadic
	)

happyReduce_29 = happySpecReduce_1  16 happyReduction_29
happyReduction_29 _
	 =  HappyAbsSyn16
		 (Compare
	)

happyReduce_30 = happySpecReduce_1  16 happyReduction_30
happyReduction_30 _
	 =  HappyAbsSyn16
		 (GenPrimOp
	)

happyReduce_31 = happySpecReduce_1  17 happyReduction_31
happyReduction_31 (HappyAbsSyn17  happy_var_1)
	 =  HappyAbsSyn17
		 (happy_var_1
	)
happyReduction_31 _  = notHappyAtAll 

happyReduce_32 = happySpecReduce_0  17 happyReduction_32
happyReduction_32  =  HappyAbsSyn17
		 (""
	)

happyReduce_33 = happySpecReduce_3  18 happyReduction_33
happyReduction_33 _
	(HappyAbsSyn17  happy_var_2)
	_
	 =  HappyAbsSyn17
		 (happy_var_2
	)
happyReduction_33 _ _ _  = notHappyAtAll 

happyReduce_34 = happySpecReduce_2  19 happyReduction_34
happyReduction_34 (HappyAbsSyn17  happy_var_2)
	(HappyAbsSyn17  happy_var_1)
	 =  HappyAbsSyn17
		 (happy_var_1 ++ happy_var_2
	)
happyReduction_34 _ _  = notHappyAtAll 

happyReduce_35 = happySpecReduce_0  19 happyReduction_35
happyReduction_35  =  HappyAbsSyn17
		 (""
	)

happyReduce_36 = happySpecReduce_3  20 happyReduction_36
happyReduction_36 _
	(HappyAbsSyn17  happy_var_2)
	_
	 =  HappyAbsSyn17
		 ("{" ++ happy_var_2 ++ "}"
	)
happyReduction_36 _ _ _  = notHappyAtAll 

happyReduce_37 = happySpecReduce_1  20 happyReduction_37
happyReduction_37 (HappyTerminal (TNoBraces happy_var_1))
	 =  HappyAbsSyn17
		 (happy_var_1
	)
happyReduction_37 _  = notHappyAtAll 

happyReduce_38 = happySpecReduce_3  21 happyReduction_38
happyReduction_38 _
	(HappyAbsSyn21  happy_var_2)
	_
	 =  HappyAbsSyn21
		 (happy_var_2
	)
happyReduction_38 _ _ _  = notHappyAtAll 

happyReduce_39 = happySpecReduce_3  22 happyReduction_39
happyReduction_39 (HappyAbsSyn21  happy_var_3)
	_
	(HappyAbsSyn23  happy_var_1)
	 =  HappyAbsSyn21
		 ([happy_var_1] ++ happy_var_3
	)
happyReduction_39 _ _ _  = notHappyAtAll 

happyReduce_40 = happySpecReduce_1  22 happyReduction_40
happyReduction_40 (HappyAbsSyn23  happy_var_1)
	 =  HappyAbsSyn21
		 ([happy_var_1]
	)
happyReduction_40 _  = notHappyAtAll 

happyReduce_41 = happySpecReduce_0  22 happyReduction_41
happyReduction_41  =  HappyAbsSyn21
		 ([]
	)

happyReduce_42 = happyReduce 7 23 happyReduction_42
happyReduction_42 (_ `HappyStk`
	(HappyTerminal (TInteger happy_var_6)) `HappyStk`
	_ `HappyStk`
	(HappyTerminal (TUpperName happy_var_4)) `HappyStk`
	_ `HappyStk`
	(HappyTerminal (TUpperName happy_var_2)) `HappyStk`
	_ `HappyStk`
	happyRest)
	 = HappyAbsSyn23
		 ((happy_var_2, happy_var_4, happy_var_6)
	) `HappyStk` happyRest

happyReduce_43 = happySpecReduce_3  24 happyReduction_43
happyReduction_43 (HappyAbsSyn24  happy_var_3)
	_
	(HappyAbsSyn24  happy_var_1)
	 =  HappyAbsSyn24
		 (TyF happy_var_1 happy_var_3
	)
happyReduction_43 _ _ _  = notHappyAtAll 

happyReduce_44 = happySpecReduce_3  24 happyReduction_44
happyReduction_44 (HappyAbsSyn24  happy_var_3)
	_
	(HappyAbsSyn24  happy_var_1)
	 =  HappyAbsSyn24
		 (TyC happy_var_1 happy_var_3
	)
happyReduction_44 _ _ _  = notHappyAtAll 

happyReduce_45 = happySpecReduce_1  24 happyReduction_45
happyReduction_45 (HappyAbsSyn24  happy_var_1)
	 =  HappyAbsSyn24
		 (happy_var_1
	)
happyReduction_45 _  = notHappyAtAll 

happyReduce_46 = happySpecReduce_2  25 happyReduction_46
happyReduction_46 (HappyAbsSyn27  happy_var_2)
	(HappyAbsSyn30  happy_var_1)
	 =  HappyAbsSyn24
		 (TyApp happy_var_1 happy_var_2
	)
happyReduction_46 _ _  = notHappyAtAll 

happyReduce_47 = happySpecReduce_1  25 happyReduction_47
happyReduction_47 (HappyAbsSyn24  happy_var_1)
	 =  HappyAbsSyn24
		 (happy_var_1
	)
happyReduction_47 _  = notHappyAtAll 

happyReduce_48 = happySpecReduce_3  25 happyReduction_48
happyReduction_48 _
	(HappyAbsSyn24  happy_var_2)
	_
	 =  HappyAbsSyn24
		 (happy_var_2
	)
happyReduction_48 _ _ _  = notHappyAtAll 

happyReduce_49 = happySpecReduce_1  25 happyReduction_49
happyReduction_49 (HappyTerminal (TLowerName happy_var_1))
	 =  HappyAbsSyn24
		 (TyVar happy_var_1
	)
happyReduction_49 _  = notHappyAtAll 

happyReduce_50 = happySpecReduce_3  26 happyReduction_50
happyReduction_50 _
	(HappyAbsSyn27  happy_var_2)
	_
	 =  HappyAbsSyn24
		 (TyUTup happy_var_2
	)
happyReduction_50 _ _ _  = notHappyAtAll 

happyReduce_51 = happySpecReduce_3  27 happyReduction_51
happyReduction_51 (HappyAbsSyn27  happy_var_3)
	_
	(HappyAbsSyn24  happy_var_1)
	 =  HappyAbsSyn27
		 (happy_var_1 : happy_var_3
	)
happyReduction_51 _ _ _  = notHappyAtAll 

happyReduce_52 = happySpecReduce_1  27 happyReduction_52
happyReduction_52 (HappyAbsSyn24  happy_var_1)
	 =  HappyAbsSyn27
		 ([happy_var_1]
	)
happyReduction_52 _  = notHappyAtAll 

happyReduce_53 = happySpecReduce_2  28 happyReduction_53
happyReduction_53 (HappyAbsSyn27  happy_var_2)
	(HappyAbsSyn24  happy_var_1)
	 =  HappyAbsSyn27
		 (happy_var_1 : happy_var_2
	)
happyReduction_53 _ _  = notHappyAtAll 

happyReduce_54 = happySpecReduce_0  28 happyReduction_54
happyReduction_54  =  HappyAbsSyn27
		 ([]
	)

happyReduce_55 = happySpecReduce_1  29 happyReduction_55
happyReduction_55 (HappyTerminal (TLowerName happy_var_1))
	 =  HappyAbsSyn24
		 (TyVar happy_var_1
	)
happyReduction_55 _  = notHappyAtAll 

happyReduce_56 = happySpecReduce_1  29 happyReduction_56
happyReduction_56 (HappyAbsSyn30  happy_var_1)
	 =  HappyAbsSyn24
		 (TyApp happy_var_1 []
	)
happyReduction_56 _  = notHappyAtAll 

happyReduce_57 = happySpecReduce_1  30 happyReduction_57
happyReduction_57 (HappyTerminal (TUpperName happy_var_1))
	 =  HappyAbsSyn30
		 (TyCon happy_var_1
	)
happyReduction_57 _  = notHappyAtAll 

happyReduce_58 = happySpecReduce_2  30 happyReduction_58
happyReduction_58 _
	_
	 =  HappyAbsSyn30
		 (TyCon "()"
	)

happyReduce_59 = happySpecReduce_1  30 happyReduction_59
happyReduction_59 _
	 =  HappyAbsSyn30
		 (SCALAR
	)

happyReduce_60 = happySpecReduce_1  30 happyReduction_60
happyReduction_60 _
	 =  HappyAbsSyn30
		 (VECTOR
	)

happyReduce_61 = happySpecReduce_1  30 happyReduction_61
happyReduction_61 _
	 =  HappyAbsSyn30
		 (VECTUPLE
	)

happyNewToken action sts stk
	= lex_tok(\tk -> 
	let cont i = action i i tk (HappyState action) sts stk in
	case tk of {
	TEOF -> action 72 72 tk (HappyState action) sts stk;
	TArrow -> cont 31;
	TDArrow -> cont 32;
	TEquals -> cont 33;
	TComma -> cont 34;
	TOpenParen -> cont 35;
	TCloseParen -> cont 36;
	TOpenParenHash -> cont 37;
	THashCloseParen -> cont 38;
	TOpenBrace -> cont 39;
	TCloseBrace -> cont 40;
	TOpenBracket -> cont 41;
	TCloseBracket -> cont 42;
	TOpenAngle -> cont 43;
	TCloseAngle -> cont 44;
	TSection -> cont 45;
	TPrimop -> cont 46;
	TPseudoop -> cont 47;
	TPrimtype -> cont 48;
	TWith -> cont 49;
	TDefaults -> cont 50;
	TTrue -> cont 51;
	TFalse -> cont 52;
	TDyadic -> cont 53;
	TMonadic -> cont 54;
	TCompare -> cont 55;
	TGenPrimOp -> cont 56;
	TFixity -> cont 57;
	TInfixN -> cont 58;
	TInfixL -> cont 59;
	TInfixR -> cont 60;
	TNothing -> cont 61;
	TVector -> cont 62;
	TSCALAR -> cont 63;
	TVECTOR -> cont 64;
	TVECTUPLE -> cont 65;
	TThatsAllFolks -> cont 66;
	TLowerName happy_dollar_dollar -> cont 67;
	TUpperName happy_dollar_dollar -> cont 68;
	TString happy_dollar_dollar -> cont 69;
	TInteger happy_dollar_dollar -> cont 70;
	TNoBraces happy_dollar_dollar -> cont 71;
	_ -> happyError' tk
	})

happyError_ 72 tk = happyError' tk
happyError_ _ tk = happyError' tk

happyThen :: () => ParserM a -> (a -> ParserM b) -> ParserM b
happyThen = (>>=)
happyReturn :: () => a -> ParserM a
happyReturn = (return)
happyThen1 = happyThen
happyReturn1 :: () => a -> ParserM a
happyReturn1 = happyReturn
happyError' :: () => (Token) -> ParserM a
happyError' tk = (\token -> happyError) tk

parsex = happySomeParser where
  happySomeParser = happyThen (happyParse action_0) (\x -> case x of {HappyAbsSyn4 z -> happyReturn z; _other -> notHappyAtAll })

happySeq = happyDontSeq


parse :: String -> Either String Info
parse = run_parser parsex
{-# LINE 1 "templates/GenericTemplate.hs" #-}
{-# LINE 1 "templates/GenericTemplate.hs" #-}
{-# LINE 1 "<command-line>" #-}





# 1 "/usr/include/stdc-predef.h" 1 3 4

# 17 "/usr/include/stdc-predef.h" 3 4



















# 47 "/usr/include/stdc-predef.h" 3 4

# 59 "/usr/include/stdc-predef.h" 3 4








{-# LINE 5 "<command-line>" #-}
{-# LINE 1 "templates/GenericTemplate.hs" #-}
-- Id: GenericTemplate.hs,v 1.26 2005/01/14 14:47:22 simonmar Exp 

{-# LINE 13 "templates/GenericTemplate.hs" #-}

{-# LINE 46 "templates/GenericTemplate.hs" #-}








{-# LINE 67 "templates/GenericTemplate.hs" #-}

{-# LINE 77 "templates/GenericTemplate.hs" #-}

{-# LINE 86 "templates/GenericTemplate.hs" #-}

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

{-# LINE 155 "templates/GenericTemplate.hs" #-}

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
      case happyDrop k ((st):(sts)) of
        sts1@(((st1@(HappyState (action))):(_))) ->
          let drop_stk = happyDropStk k stk in
          happyThen1 (fn stk tk) (\r -> action nt j tk st1 sts1 (r `HappyStk` drop_stk))

happyMonad2Reduce k nt fn (1) tk st sts stk
     = happyFail (1) tk st sts stk
happyMonad2Reduce k nt fn j tk st sts stk =
      case happyDrop k ((st):(sts)) of
        sts1@(((st1@(HappyState (action))):(_))) ->
         let drop_stk = happyDropStk k stk





             new_state = action

          in
          happyThen1 (fn stk tk) (\r -> happyNewToken new_state sts1 (r `HappyStk` drop_stk))

happyDrop (0) l = l
happyDrop n ((_):(t)) = happyDrop (n - ((1) :: Int)) t

happyDropStk (0) l = l
happyDropStk n (x `HappyStk` xs) = happyDropStk (n - ((1)::Int)) xs

-----------------------------------------------------------------------------
-- Moving to a new state after a reduction

{-# LINE 256 "templates/GenericTemplate.hs" #-}
happyGoto action j tk st = action j j tk (HappyState action)


-----------------------------------------------------------------------------
-- Error recovery ((1) is the error token)

-- parse error if we are in recovery and we fail again
happyFail (1) tk old_st _ stk@(x `HappyStk` _) =
     let i = (case x of { HappyErrorToken (i) -> i }) in
--      trace "failing" $ 
        happyError_ i tk

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
happyFail  i tk (HappyState (action)) sts stk =
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

{-# LINE 322 "templates/GenericTemplate.hs" #-}
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
