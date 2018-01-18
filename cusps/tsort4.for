00100	      SUBROUTINE TSORT4(N,MOVE,ICOMP)                                   FAM08700
00200	C                                                                       FAM08710
00300	C     THE FOLLOWING COMMENTS ARE INTERSPERSED WITH CONTINUE STATEMENTS  FAM08720
00400	C     THIS IS BECAUSE IBM FORTRAN G (INCREDIBLY) LIMITS THE NUMBER OF   FAM08730
00500	C     COMMENT CARDS WHICH MAY OCCUR CONSECUTIVELY. THEY HAVE LABELS     FAM08740
00600	C     BECAUSE OTHERWISE IBM FORTRAN H OBJECTS -- THE CHANGE FROM        FAM08750
00700	C     VERSION 14 TO VERSION 16 OF OS/360 REDUCED THE NUMBER OF          FAM08760
00800	C     ALLOWABLE COMMENT CARDS -- NO DOUBT BY VERSION 30 WE WILL ONLY BE FAM08770
00900	C     ALLOWED ONE COMMENT CARD AT A TIME                                FAM08780
01000	C                                                                       FAM08790
01100	C     A 360 PROGRAMMER'S LOT IS NOT A HAPPY ONE                         FAM08800
01200	C                                                                       FAM08810
01300	C      THIS IS A COMPLETELY GENERAL SORTING PROGRAM WHICH USES AN       FAM08820
01400	C      OPTIMIZED VERSION OF FLOYDS TREE SORT 3 ALGORITHM (CACM DEC.1964)FAM08830
01500	C      IT IS AN IN PLACE SORT OF ORDER N*LOG(BASE 2)N                   FAM08840
01600	C                                                                       FAM08850
01700	C      THE OPTIMIZATIONS DEVIZED BY R.DEWAR AND L.FISHER, U. OF CHICAGO FAM08860
01800	C      INCREASE THE SPEED OF THE ORIGINAL BY A FACTOR OF TWO            FAM08870
01900	C                                                                       FAM08880
02000	C      CODED BY R.DEWAR, U. OF CHICAGO, DEC. 1967.                      FAM08890
02100	C                                                                       FAM08900
02200	C      IT IS WRITTEN IN A VERY SIMPLE FORTRAN WHICH SHOULD COMPILE ON   FAM08910
02300	C      ALMOST ANY SYSTEM                                                FAM08920
02400	C                                                                       FAM08930
02500	990   CONTINUE                                                          FAM08940
02600	C                                                                       FAM08950
02700	C                                                                       FAM08960
02800	C      USE OF THE ROUTINE ..                                            FAM08970
02900	C                                                                       FAM08980
03000	C      CALL TSORT3(N,MOVE,COMP)                                         FAM08990
03100	C                                                                       FAM09000
03200	C      N IS THE NUMBER OF ITEMS TO BE SORTED                            FAM09010
03300	C                                                                       FAM09020
03400	C      MOVE IS THE NAME OF A SUBROUTINE USED BY THE SORTING PROGRAM TO  FAM09030
03500	C      MOVE ITEMS, A CALL OF MOVE(N,M) IS USED TO MOVE THE NTH ITEM TO  FAM09040
03600	C      THE MTH, THE OLD VALUE OF THE NTH ITEM IS UNCHANGED, THE OLD     FAM09050
03700	C      VALUE OF THE MTH ITEM IS DESTROYED                               FAM09060
03800	C                                                                       FAM09070
03900	C      ICOMP IS THE NAME OF A FUNCTION SUBPROGRAM USED TO COMPARE TWO   FAM09080
04000	C      ITEMS. ICOMP(N,M) RETURNS A NEGATIVE VALUE IF THE NTH ITEM SHOULDFAM09090
04100	C      EVENTUALLY LIE AT A LOWER INDEX THAN THE MTH, A POSITIVE VALUE   FAM09100
04200	C      IF THE NTH ITEM SHOULD LIE HIGHER THAN THE MTH, AND A ZERO       FAM09110
04300	C      VALUE IF THERE IS NO PREFERENCE. FOR MANY SIMPLE SORTS THIS MAY  FAM09120
04400	C      BE ACHEIVED WITH A SINGLE SUBTRACTION.                           FAM09130
04500	C                                                                       FAM09140
04600	C                                                                       FAM09150
04700	991   CONTINUE                                                          FAM09160
04800	C                                                                       FAM09170
04900	C                                                                       FAM09180
05000	C      REMEMBER IN THE CALLING PROGRAM TO INDICATE TO THE COMPILER      FAM09190
05100	C      THAT THE 2ND AND 3RD ARGUMENTS ARE EXTERNAL (EG AN F CARD IN     FAM09200
05200	C      FORTRAN 2, EXTERNAL STATEMENTS IN FORTRAN 4)                     FAM09210
05300	C                                                                       FAM09220
05400	C      NOTE. THE N+1TH ITEM IS USED AS A SCRATCH LOCATION BY THE SORTINGFAM09230
05500	C      PROGRAM, ITS OLD VALUE WILL BE DESTROYED, AND SHOULD BE SAVED IF FAM09240
05600	C      NECCESSARY.                                                      FAM09250
05700	C                                                                       FAM09260
05800	C      COMMON STATEMENTS IN THE CALLING PROGRAM AND THE TWO SUB-PROGRAMSFAM09270
05900	C      WILL ALLOW THE NECCESSARY COMMUNICATION                          FAM09280
06000	C                                                                       FAM09290
06100	C      NOTE THAT THE ROUTINE KNOWS NOTHING ABOUT THE NATURE OR          FAM09300
06200	C      COMPLEXITY OF THE DATA ITEMS, THIS IS ALL SPECIFIED BY THE       FAM09310
06300	C      NATURE OF THE TWO SUB-PROGRAMS, THUS ONE MAY SORT MUTIPLE        FAM09320
06400	C      ARRAYS, OR MOVE ONLY INDICES , CARRY ALONG RIDING ARRAYS ETC.    FAM09330
06500	C                                                                       FAM09340
06600	C                                                                       FAM09350
06700	992   CONTINUE                                                          FAM09360
06800	C                                                                       FAM09370
06900	C                                                                       FAM09380
07000	C      EXAMPLE OF USE                                                   FAM09390
07100	C                                                                       FAM09400
07200	C      GIVEN TWO ARRAYS A,B CONTAINING 1000 NUMBERS, SORT THE VALUES    FAM09410
07300	C      IN A (SIGNED FLOATING POINT NUMBERS) IN DECREASING ORDER AND     FAM09420
07400	C      SHUFFLE ARRAY B INTO THE CORRESPONDING ORDER                     FAM09430
07500	C                                                                       FAM09440
07600	C              EXTERNAL MOVEA,ICOMPR                                    FAM09450
07700	C              COMMON A,B                                               FAM09460
07800	C              DIMENSION A(1001),B(1001)                                FAM09470
07900	C              .                                                        FAM09480
08000	C              .                                                        FAM09490
08100	C              CALL TSORT4(1000,MOVEA,ICOMPR)                           FAM09500
08200	C              .                                                        FAM09510
08300	C              .                                                        FAM09520
08400	C              END                                                      FAM09530
08500	C                                                                       FAM09540
08600	993   CONTINUE                                                          FAM09550
08700	C                                                                       FAM09560
08800	C              SUBROUTINE MOVEA(N,M)                                    FAM09570
08900	C              COMMON A,B                                               FAM09580
09000	C              DIMENSION A(1001),B(1001)                                FAM09590
09100	C              A(M)=A(N)                                                FAM09600
09200	C              B(M)=B(N)                                                FAM09610
09300	C              RETURN                                                   FAM09620
09400	C              END                                                      FAM09630
09500	C              FUNCTION ICOMPR(N,M)                                     FAM09640
09600	C              COMMON IA,IB                                             FAM09650
09700	C              DIMENSION IA(1001),IB(1001)                              FAM09660
09800	C       CNOTE THE USE OF INTEGER MODE - THIS WILL USUALLY WORK OK       FAM09670
09900	C              ICOMPR=IA(M)-IA(N)                                       FAM09680
10000	C              RETURN                                                   FAM09690
10100	C              END                                                      FAM09700
10200	C                                                                       FAM09710
10300	C      INITIALIZE (THE DIVISION IS THE ONLY ONE USED)                   FAM09720
10400	C                                                                       FAM09730
10500	       NH=N/2                                                           FAM09740
10600	       N1=N+1                                                           FAM09750
10700	C                                                                       FAM09760
10800	C      TEST FOR SPECIAL CASES OF ONE OR TWO ITEMS                       FAM09770
10900	C                                                                       FAM09780
11000	      IF (N-2) 60,55,100                                                FAM09790
11100	C                                                                       FAM09800
11200	C      TEST FOR THE PARITY OF THE NUMBER ITEMS, THIS SAVES THE OVERFLOW FAM09810
11300	C      TEST IN THE FIRST LOOP                                           FAM09820
11400	C                                                                       FAM09830
11500	100    IF ((N/2)*2-N) 10,1,10                                           FAM09840
11600	C                                                                       FAM09850
11700	C      DEAL WITH THE NODE HAVING NO RIGHT SON SEPERATELY                FAM09860
11800	C                                                                       FAM09870
11900	1      IF (ICOMP(N,NH)) 2,2,3                                           FAM09880
12000	C                                                                       FAM09890
12100	C      THE FOLLOWING IS THE EXCHANGE SEQUENCE                           FAM09900
12200	C                                                                       FAM09910
12300	3      CALL MOVE(N,N1)                                                  FAM09920
12400	       CALL MOVE(NH,N)                                                  FAM09930
12500	       CALL MOVE(N1,NH)                                                 FAM09940
12600	2      NH=NH-1                                                          FAM09950
12700	C                                                                       FAM09960
12800	C      MULTIPLY BY 2 TO GET LEFT SON                                    FAM09970
12900	C                                                                       FAM09980
13000	10    NS=NH+NH                                                          FAM09990
13100	C                                                                       FAM10000
13200	C      COMPARE WITH RIGHT SON                                           FAM10010
13300	C                                                                       FAM10020
13400	       IF (ICOMP(NS,NS+1)) 4,5,5                                        FAM10030
13500	4      NS=NS+1                                                          FAM10040
13600	C                                                                       FAM10050
13700	C      NS IS NOW THE LARGER SON, COMPARE WITH ROOT                      FAM10060
13800	C                                                                       FAM10070
13900	5      IF (ICOMP(NH,NS)) 6,30,30                                        FAM10080
14000	C                                                                       FAM10090
14100	C      SHUFFLEING IS NECCASSARY, NOW (BUT NOT TILL NOW) SAVE THE ROOT   FAM10100
14200	C                                                                       FAM10110
14300	6      NHT=NS                                                           FAM10120
14400	       CALL MOVE(NH,N1)                                                 FAM10130
14500	       CALL MOVE(NS,NH)                                                 FAM10140
14600	C                                                                       FAM10150
14700	C      NOW ENTER THE LOOP WHICH IS THE SAME AS WHAT WE HAVE DONE SO FAR FAM10160
14800	C      EXCEPT THAT WE HAVE TO TEST IF THERE ARE ANY SONS                FAM10170
14900	C                                                                       FAM10180
15000	      GO TO 7                                                           FAM10190
15100	8      IF (ICOMP(NS,NS+1)) 11,12,12                                     FAM10200
15200	11     NS=NS+1                                                          FAM10210
15300	12     IF (ICOMP(N1,NS)) 22,29,29                                       FAM10220
15400	C                                                                       FAM10230
15500	C      MOVE THE SON UP A LEVEL                                          FAM10240
15600	C                                                                       FAM10250
15700	22     CALL MOVE(NS,NHT)                                                FAM10260
15800	       NHT=NS                                                           FAM10270
15900	7      NS=NHT+NHT                                                       FAM10280
16000	       IF (NS-N) 8,12,29                                                FAM10290
16100	C                                                                       FAM10300
16200	C      AT THE END OF THE LOOP, MOVE THE ROOT INTO THE HOLE              FAM10310
16300	C                                                                       FAM10320
16400	29     CALL MOVE(N1,NHT)                                                FAM10330
16500	30     NH=NH-1                                                          FAM10340
16600	C                                                                       FAM10350
16700	C      GO BACK FOR MORE IF THERE ARE SUCH , NOTE THAT THE LAST LOOP     FAM10360
16800	C      (NH=1) IS A WASTE OF TIME, HOWEVER IT IS QUICKER TO DO IT        FAM10370
16900	C      THAN TO TEST FOR IT                                              FAM10380
17000	C                                                                       FAM10390
17100	       IF (NH ) 10,31,10
17200	C                                                                       FAM10410
17300	C      THE TREE IS NOW PARTIALLY ORDERED                                FAM10420
17400	C                                                                       FAM10430
17500	31     NO=N                                                             FAM10440
17600	C                                                                       FAM10450
17700	C      GO TEST FOR SPECIAL CASE OF ONLY TWO ITEMS                       FAM10460
17800	C                                                                       FAM10470
17900	       GO TO 50                                                         FAM10480
18000	C                                                                       FAM10490
18100	C      NO IS THE SIZE OF THE REMAINING UNSORTED ARRAY, MOVE THE FIRST   FAM10500
18200	C      (LARGEST) INTO PLACE, SAVING THE DISPLACED ELEMENT               FAM10510
18300	C                                                                       FAM10520
18400	32     CALL MOVE(NO,N1)                                                 FAM10530
18500	       CALL MOVE(1,NO)                                                  FAM10540
18600	C                                                                       FAM10550
18700	C      AND WALL OFF THE NEXT ELEMENT (IE SQUEEZE THE ARRAY DOWN 1)      FAM10560
18800	C                                                                       FAM10570
18900	       NO=NO-1                                                          FAM10580
19000	C                                                                       FAM10590
19100	C      NOW SEE WHETHER 2 OR 3 IS TO RISE TO THE TOP                     FAM10600
19200	C                                                                       FAM10610
19300	      IF (ICOMP(2,3)) 34,34,37                                          FAM10620
19400	34     CALL MOVE(3,1)                                                   FAM10630
19500	       NOS=3                                                            FAM10640
19600	       NS = 6                                                             FAM10650
19700	       GO TO 40                                                         FAM10660
19800	37     CALL MOVE(2,1)                                                   FAM10670
19900	       NOS=2                                                            FAM10680
20000	       NS=4                                                             FAM10690
20100	C                                                                       FAM10700
20200	C     PULL UP SMALLEST SON UNTIL THE BOTTOM OF THE TREE APPEARS         FAM10710
20300	C                                                                       FAM10720
20400	       GO TO 40                                                         FAM10730
20500	42    IF (ICOMP(NS,NS+1)) 45,43,43                                      FAM10740
20600	45     NS=NS+1                                                          FAM10750
20700	43    CALL MOVE(NS,NOS)                                                 FAM10760
20800	       NOS=NS                                                           FAM10770
20900	      NS=NOS+NOS                                                        FAM10780
21000	40     IF (NS-NO) 42,43,49                                              FAM10790
21100	C                                                                       FAM10800
21200	C     NOW START PULLING DOWN PARENTS TILL CORRECT PLACE IS FOUND        FAM10810
21300	C                                                                       FAM10820
21400	49    NS=NOS/2                                                          FAM10830
21500	71    IF (ICOMP(N1,NS)) 73,73,72                                        FAM10840
21600	72    CALL MOVE(NS,NOS)                                                 FAM10850
21700	      NOS=NS                                                            FAM10860
21800	      NS=NS/2                                                           FAM10870
21900	      GO TO 71                                                          FAM10880
22000	73    CALL MOVE(N1,NOS)                                                 FAM10890
22100	50     IF (NO-3) 54,54,32                                               FAM10900
22200	C                                                                       FAM10910
22300	C     HERE DEAL WITH THE CASE OF THREE ITEMS LEFT TO BE SORTED          FAM10920
22400	C                                                                       FAM10930
22500	54     CALL MOVE(3,N1)                                                  FAM10940
22600	       CALL MOVE(1,3)                                                   FAM10950
22700	       CALL MOVE(N1,1)                                                  FAM10960
22800	C                                                                       FAM10970
22900	C      WE COME HERE WHEN THERE ARE ONLY TWO ITEMS, JUST EXCHANGE IF     FAM10980
23000	C      NECCESSARY                                                       FAM10990
23100	55     IF(ICOMP(1,2)) 60,60,56                                          FAM11000
23200	56     CALL MOVE(1,N1)                                                  FAM11010
23300	       CALL MOVE(2,1)                                                   FAM11020
23400	       CALL MOVE(N1,2 )                                                  FAM11030
23500	60     CONTINUE
23600	       RETURN                                                           FAM11040
23700	C                                                                       FAM11050
23800	       END                                                              FAM11060
