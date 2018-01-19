
(LAP &COPY SUBR) 
(PUSH P 6) 
(MOVEI 6 4) 
(PUSH P (C 0)) 
(MOVE 3 ISPTR) 
(MOVEM 3 SPTR) 
(MOVE 3 IDPTR) 
(MOVEM 3 DPTR) 
(HLRZ@ 4 1) 
(HLRZ@ 5 2) 
(133000 0 SPTR) 
LOOP 
(134000 3 SPTR) 
(CAIN 3 42) 
(JRST 0 FINISH) 
(136000 3 DPTR) 
(367000 6 LOOP) 
(MOVEI 6 4) 
(MOVEM 2 0 P) 
(MOVE 3 ISPTR) 
(MOVEM 3 SPTR) 
(HRRZ@ 1 1) 
(HLRZ@ 4 1) 
(134000 3 SPTR) 
(CAIN 3 42) 
(JRST 0 LASTCH) 
(136000 3 DPTR) 
(MOVE 3 IDPTR) 
(MOVEM 3 DPTR) 
(HRRZ@ 2 2) 
(HLRZ@ 5 2) 
(JRST 0 LOOP) 
LASTCH 
(MOVEI 1 0) 
(136000 1 DPTR) 
FINISH 
(MOVEI 1 0) 
(CAIN 6 4) 
(JRST 0 NULLCDR) 
LOOP1 
(136000 1 DPTR) 
(365000 6 LOOP1) 
EXIT 
(POP P 2) 
(POP P 6) 
(POPJ P) 
NULLCDR 
(336000 0 0 P) 
(334000 1 (C 0 0 (QUOTE T) 0)) 
(HRRM@ 1 0 P) 
(JRST 0 EXIT) 
ISPTR 
(440700 0 0 4) 
IDPTR 
(440700 0 0 5) 
SPTR 
(0) 
DPTR 
(0) 
NIL 

(LAP &STRP SUBR) 
(HLRZ@ 3 1) 
(MOVE 4 PTR) 
(134000 5 4) 
(CAIE 5 42) 
(JRST 0 FALSE) 
(MOVEI 5 5) 
(CAMN 1 2) 
(364000 5 LOOP) 
(HLRZ@ 3 2) 
(MOVE 4 PTR) 
LOOP 
(134000 1 4) 
(JUMPE 1 FALSE) 
(CAIN 1 42) 
(JRST 0 TRUE) 
(367000 5 LOOP) 
FALSE 
(TDZA 1 1) 
TRUE 
(MOVEI 1 (QUOTE T) S) 
(POPJ P) 
PTR 
(440700 0 0 3) 
NIL 


(DEFPROP &X&
 T
SPECIAL)

(DEFPROP &Y&
 T
SPECIAL)

(DEFPROP NEQ
 (LAMBDA (X) (LIST (QUOTE NOT) (CONS (QUOTE EQ) (CDR X))))
MACRO)

(DEFPROP NEQUAL
 (LAMBDA (X) (LIST (QUOTE NOT) (CONS (QUOTE EQUAL) (CDR X))))
MACRO)

(DEFPROP LEQUAL
 (LAMBDA (X) (LIST (QUOTE NOT) (CONS (QUOTE GREATERP) (CDR X))))
MACRO)

(DEFPROP GEQUAL
 (LAMBDA (X) (LIST (QUOTE NOT) (CONS (QUOTE LESSP) (CDR X))))
MACRO)

(DEFPROP PRELIST
 (LAMBDA (L N)
  (PROG (&V &VV &L1 &L2 &UPPER1 &X& I)
        (SETQ &L1 1)
        (SETQ &UPPER1 N)
        (SETQ &L2 L)
        (SETQ &V (SETQ &VV (LIST NIL)))
   LOOP (COND ((OR (*GREAT &L1 &UPPER1) (NULL &L2)) (RETURN (CDR &V))))
        (SETQ &X& &L1)
        (SETQ I (CAR &L2))
        (SETQ &L1 (ADD1 &L1))
        (SETQ &L2 (CDR &L2))
        (NCONC &VV (SETQ &VV (LIST I)))
        (GO LOOP)))
EXPR)

(DEFPROP SUFLIST
 (LAMBDA (L N)
  (COND ((*LESS N 1) L)
        (T (PROG (&V)
            LOOP (COND ((AND L (NOT (*LESS (SETQ N (SUB1 N)) 0))) (SETQ &V (SETQ L (CDR L)))) (T (RETURN &V)))
                 (GO LOOP)))))
EXPR)

(DEFPROP STR
 (LAMBDA (X)
  (PROG2 (COND ((SETQ X (EXPLODEC X)) (RPLACD (LAST X) (QUOTE (/")))) (T (SETQ X (QUOTE (/")))))
         (READLIST (CONS (QUOTE /") X))))
EXPR)

(DEFPROP STRP
 (LAMBDA (X) (AND (ATOM X) (NOT (NUMBERP X)) (&STRP (GET X (QUOTE PNAME)) (LAST (GET X (QUOTE PNAME))))))
EXPR)

(DEFPROP STRLEN
 (LAMBDA (X) (LENGTH (EXPLODEC X)))
EXPR)

(DEFPROP SEQ
 (LAMBDA (X Y) (EQUAL (EXPLODEC X) (EXPLODEC Y)))
EXPR)

(DEFPROP AT
 (LAMBDA (X)
  (COND ((NOT (ATOM X)) (AT (STR X)))
        ((NUMBERP X) (READLIST (CONS (QUOTE //) (EXPLODE X))))
        ((NOT (STRP X)) X)
        (T (PROG (S D G)
                 (SETQ G (GENSYM))
                 (SETQ S (GET X (QUOTE PNAME)))
                 (PUTPROP G
                          (SETQ D (MAPCAR (FUNCTION (LAMBDA (X) (CAR (GET (GENSYM) (QUOTE PNAME))))) S))
                          (QUOTE PNAME))
                 (RETURN (COND ((&COPY S D) (QUOTE &NONAME)) (T (INTERN G))))))))
EXPR)

(DEFPROP CAT
 (LAMBDA (X Y) (READLIST (CONS (QUOTE /") (APPEND (EXPLODEC X) (APPEND (EXPLODEC Y) (LIST (QUOTE /")))))))
EXPR)

(DEFPROP SUBSTR
 (LAMBDA (S STRT LEN)
  (READLIST
   (CONS (QUOTE /")
         (APPEND (COND ((NUMBERP LEN) (PRELIST (SUFLIST (EXPLODEC S) (SUB1 STRT)) LEN))
                       (T (SUFLIST (EXPLODEC S) (SUB1 STRT))))
                 (LIST (QUOTE /"))))))
EXPR)

(DEFPROP PRINTSTR
 (LAMBDA (X) (TERPRI (PRINC X)))
EXPR)

(DEFPROP PRINTTY
 (LAMBDA (X) (PROG (FILE) (SETQ FILE (OUTC NIL NIL)) (PRINC X) (PRINC (QUOTE " ")) (OUTC FILE NIL) (RETURN X)))
EXPR)

(DEFPROP NEQ
 (LAMBDA (X Y) (NOT (EQ X Y)))
EXPR)

(DEFPROP NEQUAL
 (LAMBDA (X Y) (NOT (EQUAL X Y)))
EXPR)

(DEFPROP LEQUAL
 (LAMBDA (X Y) (NOT (*GREAT X Y)))
EXPR)

(DEFPROP GEQUAL
 (LAMBDA (X Y) (NOT (*LESS X Y)))
EXPR)

(DEFPROP &VECTOR
 (LAMBDA (PREFIX FN X Y)
  (COND (PREFIX
         (COND ((AND X (ATOM X))
                (COND ((GET FN (QUOTE MACRO)) (EVAL (LIST FN (LIST (QUOTE QUOTE) X)))) (T (FN X))))
               (T (MAPCAR FN X))))
        (T (PROG (V L ATOMX ATOMY CARX CARY M)
                 (SETQ ATOMX (AND X (ATOM X)))
                 (SETQ ATOMY (AND Y (ATOM Y)))
                 (SETQ M (GET FN (QUOTE MACRO)))
                 (COND
                  ((AND ATOMX ATOMY)
                   (RETURN
                    (COND (M (EVAL (LIST FN (LIST (QUOTE QUOTE) X) (LIST (QUOTE QUOTE) Y)))) (T (FN X Y))))))
                 (SETQ V (SETQ L (LIST NIL)))
            LOOP (COND ((OR (NULL X) (NULL Y)) (RETURN (CDR V))))
                 (COND (ATOMX (SETQ CARX X)) (T (SETQ CARX (CAR X)) (SETQ X (CDR X))))
                 (COND (ATOMY (SETQ CARY Y)) (T (SETQ CARY (CAR Y)) (SETQ Y (CDR Y))))
                 (SETQ L
                       (CDR
                        (RPLACD L
                                (LIST
                                 (COND (M (EVAL (LIST FN (LIST (QUOTE QUOTE) CARX) (LIST (QUOTE QUOTE) CARY))))
                                       (T (FN CARX CARY)))))))
                 (GO LOOP)))))
EXPR)

(DEFPROP &REPLACE
 (LAMBDA (L X V) (COND (X (&REP1 L X V (CAR X) 1)) (T V)))
EXPR)

(DEFPROP &REP1
 (LAMBDA (L X V Y N)
  (COND ((ATOM L)
         (COND ((EQUAL Y N) (CONS (&REPLACE NIL (CDR X) V) NIL)) (T (CONS NIL (&REP1 NIL X V Y (ADD1 N))))))
        ((EQUAL Y N) (CONS (&REPLACE (CAR L) (CDR X) V) (CDR L)))
        (T (CONS (CAR L) (&REP1 (CDR L) X V Y (ADD1 N))))))
EXPR)

(DEFPROP &DECOMPOSE
 (LAMBDA (TEM L) (PROG2 (&DEC1 TEM L NIL) L))
EXPR)

(DEFPROP &DEC1
 (LAMBDA (TEM L U)
  (COND ((NULL TEM) (NULL L))
        ((ATOM TEM) (OR (EQ TEM (QUOTE )) (SET TEM L) T))
        ((ATOM L) (OR (AND (NULL L) (EQUAL TEM (QUOTE ()))) (&SETNIL TEM)))
        ((EQ (CAR TEM) (QUOTE )) (OR (&DEC1 (CDR TEM) L T) (&DEC1 TEM (CDR L) U)))
        (U (AND (&DEC1 (CAR TEM) (CAR L) T) (&DEC1 (CDR TEM) (CDR L) T)))
        (T (SETQ U (&DEC1 (CAR TEM) (CAR L) NIL)) (AND (&DEC1 (CDR TEM) (CDR L) NIL) U))))
EXPR)

(DEFPROP &SETNIL
 (LAMBDA (TEM)
  (COND ((OR (NULL TEM) (EQ TEM (QUOTE ))) NIL)
        ((ATOM TEM) (SET TEM NIL))
        (T (&SETNIL (CAR TEM)) (&SETNIL (CDR TEM)))))
EXPR)
