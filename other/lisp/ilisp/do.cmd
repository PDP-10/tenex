00100	####MAC
00200	conn ILISP/macro/TNXLSP=/p=/p=/p_TNXLSP.mac/
00300	####ZL
00400	ii.sav/(dskin icrexit)/(nouuo nil)/(excise)/(icrx)/ssa$$$zlisp//
00500	####ZLT
00600	zlisp/(chcore)/60 Y 2000      /(dskin <mlisp>zutil)/
00700	(crexit)/ssa$$$z2//
00800	####C
00900	<ILISP>ML$/(MLISP C T)/+Q/
01000	####RNF2
01100	conn ilisp/tnxlsp.boot/27 Y1100 1200 700        /
01200	(inc (input btsch tnxlap) nil)/
01300	(sysclr)/(excise)/(setq lptlength 105)/(gc)/(ddt)/
01400	gctim=/0/popj p,$x/(speak 0)/(exit)/
01500	ssa 0 477 tnxlsp//
01600	####DO
01700	sos/do.cmd//
01800	####DELV
01900	delver/yy*.*/
02000	####MLISP
02100	MLISPC/(chcore)/160 Y3000  2000    /
02200	(DSKIN CREXIT)/(TEN)/(excise)/(CREXIT)/
02300	SSA$$$ML//
02400	####
