

(DEFPROP &FOR1
 (LAMBDA (L FN EX BE LISTS)
  (CONS (QUOTE PROG)
        (CONS (&PROGVARS L LISTS FN EX)
              (APPEND (&INITS 1 L LISTS (&RPLACA FN EX))
                      (CONS (QUOTE LOOP)
                            (CONS (&TEST L (&TEST1 L LISTS 1) FN EX)
                                  (APPEND (&SETS L LISTS)
                                          (APPEND (&NEXTS L LISTS 1)
                                                  (APPEND (&VAL FN EX)
                                                          (APPEND
                                                           (COND
                                                            (BE
                                                             (LIST
                                                              (LIST (QUOTE COND) (LIST BE (&RET FN EX))))))
                                                           (CONS (QUOTE (GO LOOP))
                                                                 (COND
                                                                  ((NOT
                                                                    (EQUAL (LENGTH (&NEWVARS L)) (LENGTH L)))
                                                                   (CONS (QUOTE EXIT)
                                                                         (APPEND (&RESETS L LISTS 1 (CDR L))
                                                                                 (LIST
                                                                                  (&RET FN EX)))))))))))))))))
EXPR)

(DEFPROP &PROGVARS
 (LAMBDA (L LISTS FN EX)
  (APPEND (COND ((&RPLACA FN EX) (QUOTE (&V &VV))) (T (QUOTE (&V))))
          (APPEND LISTS
                  (APPEND (&RNGES L 1)
                          (APPEND (&NEWVARS L)
                                  (COND
                                   ((AND EX (NOT (OR (EQ FN (QUOTE PROG2)) (EQ FN (QUOTE APPEND)))))
                                    (QUOTE (&NOTFIRST &EX)))))))))
EXPR)

(DEFPROP &RNGES
 (LAMBDA (L N)
  (COND ((NULL L) NIL)
        (T (APPEND (COND
                    ((EQ (&HOW L) (QUOTE _))
                     (APPEND (COND ((NUMBERP (&UPPER L)) NIL) (T (LIST (AT (CAT (QUOTE "&UPPER") N)))))
                             (COND ((NUMBERP (&INCR L)) NIL)
                                   (T (LIST (AT (CAT (QUOTE "&INC") N))
                                            (AT (CAT (QUOTE "&POS") N))
                                            (AT (CAT (QUOTE "&NEG") N))
                                            (AT (CAT (QUOTE "&ZERO") N))))))))
                   (&RNGES (CDR L) (ADD1 N))))))
EXPR)

(DEFPROP &NEWVARS
 (LAMBDA (L)
  (COND ((NULL L) NIL) ((EQ (&NEW L) (QUOTE NEW)) (CONS (&VAR L) (&NEWVARS (CDR L)))) (T (&NEWVARS (CDR L)))))
EXPR)

(DEFPROP &INITS
 (LAMBDA (N L LISTS R)
  (COND ((NULL L) (COND (R (QUOTE ((SETQ &V (SETQ &VV (LIST NIL)))))) (T NIL)))
        (T (APPEND (COND ((EQ (&HOW L) (QUOTE _)) (&INITS1 L (CAR LISTS) N))
                         (T (LIST (LIST (QUOTE SETQ) (CAR LISTS) (&LIST L)))))
                   (&INITS (ADD1 N) (CDR L) (CDR LISTS) R)))))
EXPR)

(DEFPROP &INITS1
 (LAMBDA (L LST N)
  (CONS (LIST (QUOTE SETQ) LST (&LOWER L))
        (APPEND (COND ((NUMBERP (&UPPER L)) NIL)
                      (T (LIST (LIST (QUOTE SETQ) (AT (CAT (QUOTE "&UPPER") N)) (&UPPER L)))))
                (COND ((NUMBERP (&INCR L)) NIL)
                      (T (LIST (LIST (QUOTE SETQ) (AT (CAT (QUOTE "&INC") N)) (&INCR L))
                               (LIST (QUOTE COND)
                                     (LIST (LIST (QUOTE GREATERP) (AT (CAT (QUOTE "&INC") N)) 0)
                                           (LIST (QUOTE SETQ) (AT (CAT (QUOTE "&POS") N)) T))
                                     (LIST (LIST (QUOTE LESSP) (AT (CAT (QUOTE "&INC") N)) 0)
                                           (LIST (QUOTE SETQ) (AT (CAT (QUOTE "&NEG") N)) T))
                                     (LIST T (LIST (QUOTE SETQ) (AT (CAT (QUOTE "&ZERO") N)) T)))))))))
EXPR)

(DEFPROP &TEST
 (LAMBDA (L TESTS FN EX)
  (LIST (QUOTE COND)
        (LIST (COND ((CDR TESTS) (CONS (QUOTE OR) TESTS)) (T (CAR TESTS)))
              (COND ((EQUAL (LENGTH (&NEWVARS L)) (LENGTH L)) (&RET FN EX)) (T (QUOTE (GO EXIT)))))))
EXPR)

(DEFPROP &TEST1
 (LAMBDA (L LISTS N)
  (COND ((NULL L) NIL) (T (APPEND (&TEST2 L (CAR LISTS) N) (&TEST1 (CDR L) (CDR LISTS) (ADD1 N))))))
EXPR)

(DEFPROP &TEST2
 (LAMBDA (L LST N)
  (COND ((EQ (&HOW L) (QUOTE _)) (&TEST3 (&INCR L) (&NUM (&UPPER L) (QUOTE "&UPPER") N) LST N))
        (T (LIST (LIST (QUOTE NULL) LST)))))
EXPR)

(DEFPROP &TEST3
 (LAMBDA (INC UP LST N)
  (COND ((NUMBERP INC) (LIST (LIST (COND ((*GREAT INC 0) (QUOTE GREATERP)) (T (QUOTE LESSP))) LST UP)))
        (T (LIST (LIST (QUOTE AND) (AT (CAT (QUOTE "&POS") N)) (LIST (QUOTE GREATERP) LST UP))
                 (LIST (QUOTE AND) (AT (CAT (QUOTE "&NEG") N)) (LIST (QUOTE LESSP) LST UP))
                 (AT (CAT (QUOTE "&ZERO") N))))))
EXPR)

(DEFPROP &SETS
 (LAMBDA (L LISTS)
  (COND ((NULL L) NIL)
        (T (CONS (LIST (QUOTE SETQ)
                       (&VAR L)
                       (COND ((EQ (&HOW L) (QUOTE IN)) (LIST (QUOTE CAR) (CAR LISTS))) (T (CAR LISTS))))
                 (&SETS (CDR L) (CDR LISTS))))))
EXPR)

(DEFPROP &NEXTS
 (LAMBDA (L LISTS N)
  (COND ((NULL L) NIL)
        (T (CONS (LIST (QUOTE SETQ)
                       (CAR LISTS)
                       (COND ((EQ (&HOW L) (QUOTE _)) (&NEXTS1 (&INCR L) (CAR LISTS) N))
                             (T (LIST (QUOTE CDR) (CAR LISTS)))))
                 (&NEXTS (CDR L) (CDR LISTS) (ADD1 N))))))
EXPR)

(DEFPROP &NEXTS1
 (LAMBDA (INC LST N)
  (COND ((EQUAL INC 1) (LIST (QUOTE ADD1) LST))
        ((EQUAL INC -1) (LIST (QUOTE SUB1) LST))
        (T (LIST (QUOTE PLUS) LST (&NUM INC (QUOTE "&INC") N)))))
EXPR)

(DEFPROP &VAL
 (LAMBDA (FN EX)
  (COND ((NULL EX) NIL)
        ((EQ FN (QUOTE PROG2)) (LIST (LIST (QUOTE SETQ) (QUOTE &V) EX)))
        ((&RPLACA FN EX) (LIST (LIST (QUOTE NCONC) (QUOTE &VV) (LIST (QUOTE SETQ) (QUOTE &VV) EX))))
        ((EQ FN (QUOTE APPEND)) (LIST (LIST (QUOTE SETQ) (QUOTE &V) (LIST (QUOTE APPEND) (QUOTE &V) EX))))
        (T (LIST (LIST (QUOTE SETQ) (QUOTE &EX) EX)
                 (LIST (QUOTE SETQ)
                       (QUOTE &V)
                       (LIST (QUOTE COND)
                             (LIST (QUOTE &NOTFIRST) (LIST FN (QUOTE &V) (QUOTE &EX)))
                             (QUOTE ((SETQ &NOTFIRST T) &EX))))))))
EXPR)

(DEFPROP &RESETS
 (LAMBDA (L LISTS N MANY)
  (COND ((NULL L) NIL)
        (T (APPEND (COND ((EQ (&NEW L) (QUOTE OLD)) (&RESETS1 (&TEST2 L (CAR LISTS) N) L MANY)))
                   (&RESETS (CDR L) (CDR LISTS) (ADD1 N) MANY)))))
EXPR)

(DEFPROP &RESETS1
 (LAMBDA (TT L MANY)
  (COND (MANY
         (LIST
          (LIST (QUOTE AND)
                (COND ((CDR TT) (CONS (QUOTE OR) TT)) (T (CAR TT)))
                (LIST (QUOTE SETQ) (&VAR L) NIL))))
        (T (LIST (LIST (QUOTE SETQ) (&VAR L) NIL)))))
EXPR)

(DEFPROP &RET
 (LAMBDA (FN EX) (COND ((&RPLACA FN EX) (QUOTE (RETURN (CDR &V)))) (T (QUOTE (RETURN &V)))))
EXPR)

(DEFPROP &LISTLST
 (LAMBDA (L N) (COND ((NULL L) NIL) (T (CONS (AT (CAT (QUOTE "&L") N)) (&LISTLST (CDR L) (ADD1 N))))))
EXPR)

(DEFPROP &NUM
 (LAMBDA (V X N) (COND ((NUMBERP V) V) (T (AT (CAT X N)))))
EXPR)

(DEFPROP &RPLACA
 (LAMBDA (FN EX) (AND (EQ FN (QUOTE APPEND)) (NOT (ATOM EX)) (EQ (CAR EX) (QUOTE LIST))))
EXPR)

(DEFPROP &NEW
 (LAMBDA (L) (CAAR L))
EXPR)

(DEFPROP &VAR
 (LAMBDA (L) (CADAR L))
EXPR)

(DEFPROP &HOW
 (LAMBDA (L) (CADDAR L))
EXPR)

(DEFPROP &LIST
 (LAMBDA (L) (CAR (CDDDAR L)))
EXPR)

(DEFPROP &LOWER
 (LAMBDA (L) (CADAR (CDDDAR L)))
EXPR)

(DEFPROP &UPPER
 (LAMBDA (L) (CADDAR (CDDDAR L)))
EXPR)

(DEFPROP &INCR
 (LAMBDA (L) (CAR (CDDDAR (CDDDAR L))))
EXPR)

(DEFPROP &LOOP1
 (LAMBDA (NAME FN EX BE)
  (COND ((&RPLACA FN EX)
         (APPEND (QUOTE (PROG (&V &VV) (SETQ &V (SETQ &VV (LIST NIL))) LOOP))
                 (COND ((EQ NAME (QUOTE &DO))
                        (LIST (LIST (QUOTE NCONC) (QUOTE &VV) (LIST (QUOTE SETQ) (QUOTE &VV) EX))
                              (LIST (QUOTE COND) (LIST BE (QUOTE (RETURN (CDR &V)))) (QUOTE (T (GO LOOP))))))
                       (T (LIST (LIST (QUOTE COND)
                                      (LIST BE
                                            (LIST (QUOTE NCONC)
                                                  (QUOTE &VV)
                                                  (LIST (QUOTE SETQ) (QUOTE &VV) EX)))
                                      (QUOTE (T (RETURN (CDR &V)))))
                                (QUOTE (GO LOOP)))))))
        ((COND ((EQ FN (QUOTE APPEND)) (SETQ EX (LIST (QUOTE APPEND) (QUOTE &V) EX))) (T EX))
         (APPEND (QUOTE (PROG (&V) LOOP))
                 (COND ((EQ NAME (QUOTE &DO))
                        (LIST (LIST (QUOTE SETQ) (QUOTE &V) EX)
                              (LIST (QUOTE COND) (LIST BE (QUOTE (RETURN &V))) (QUOTE (T (GO LOOP))))))
                       (T (LIST (LIST (QUOTE COND)
                                      (LIST BE (LIST (QUOTE SETQ) (QUOTE &V) EX))
                                      (QUOTE (T (RETURN &V))))
                                (QUOTE (GO LOOP)))))))
        (T (APPEND (QUOTE (PROG NIL LOOP))
                   (COND ((EQ NAME (QUOTE &DO))
                          (LIST (LIST (QUOTE COND) (LIST (LIST (QUOTE NOT) BE) (QUOTE (GO LOOP))))))
                         (T (LIST (LIST (QUOTE COND) (LIST BE (QUOTE (GO LOOP)))))))))))
EXPR)

(DEFPROP &CARS
 (LAMBDA (X L N)
  (COND ((NULL L) X)
        ((NUMBERP (CAR L))
         (COND ((NOT (*GREAT N 3))
                (&CARS (LIST (AT
                              (CAT (QUOTE "C")
                                   (CAT (SUBSTR (QUOTE "ADDD")
                                                (COND ((NOT (*GREAT (*PLUS (CAR L) N) 4)) 1) (T 2))
                                                (COND
                                                 ((NOT (*GREAT (*PLUS (CAR L) N) 4)) (CAR L))
                                                 (T (*DIF 4 N))))
                                        (SUBSTR (CAR X) 2 (QUOTE ALL)))))
                             (CADR X))
                       (COND ((NOT (*GREAT (*PLUS (CAR L) N) 4)) (CDR L))
                             (T (CONS (*DIF (*PLUS (CAR L) N) 4) (CDR L))))
                       (*PLUS (CAR L) N)))
               ((NOT (*GREAT (CAR L) 4))
                (&CARS (LIST (AT (CAT (QUOTE "C") (CAT (SUBSTR (QUOTE "ADDD") 1 (CAR L)) (QUOTE "R")))) X)
                       (CDR L)
                       (CAR L)))
               (T (&CARS (LIST (QUOTE CDDDDR) X) (CONS (*DIF (CAR L) 4) (CDR L)) 4))))
        (T (&CARS (LIST (QUOTE CAR)
                        (LIST (QUOTE SUFLIST)
                              X
                              (COND
                               ((AND (NOT (ATOM (CAR L))) (EQ (CAAR L) (QUOTE ADD1))) (CADAR L))
                               (T (LIST (QUOTE SUB1) (CAR L))))))
                  (CDR L)
                  1))))
EXPR)
