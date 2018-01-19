

(DEFPROP &X&
 T
SPECIAL)

(DEFPROP &Y&
 T
SPECIAL)

(DEFPROP &FOR
 (LAMBDA (X) (&FOR1 (CADADR X) (CAR (CDADDR X)) (CADR (CADDDR X)) (CADAR (CDDDDR X)) (&LISTLST (CADADR X) 1)))
MACRO)

(DEFPROP &DO
 (LAMBDA (X) (&LOOP1 (CAR X) (CADADR X) (CAR (CDADDR X)) (CADR (CADDDR X))))
MACRO)

(DEFPROP &WHILE
 (LAMBDA (X) (&LOOP1 (CAR X) (CADADR X) (CADR (CADDDR X)) (CAR (CDADDR X))))
MACRO)

(DEFPROP &INDEX
 (LAMBDA (X) (&CARS (CADR X) (CDADDR X) 4))
MACRO)

(DEFPROP &FOR
 (LAMBDA (L FN EX B)
  (PROG (&Y& NOTFIRST LST)
        (SETQ LST
              (MAPCAR (FUNCTION
                       (LAMBDA (&X&)
                        (CONS (LIST (CADR &X&)
                                    (EQ (CADDR &X&) (QUOTE ON))
                                    (EQ (CADDR &X&) (QUOTE _))
                                    (EQ (CAR &X&) (QUOTE NEW))
                                    (COND ((GET (CADR &X&) (QUOTE VALUE)) (CDR (GET (CADR &X&) (QUOTE VALUE))))
                                          (T (CDR (GET (QUOTE &UNBOUND&) (QUOTE VALUE))))))
                              (EVAL (CADDDR &X&)))))
                      L))
   LOOP (COND ((&FORSTOP LST) (&FORRESET LST T) (RETURN &Y&)))
        (MAPCAR (FUNCTION (LAMBDA (&X&) (SET (CAAR &X&) (COND ((CADAR &X&) (CDR &X&)) (T (CADR &X&)))))) LST)
        (SETQ &Y& (EVAL (COND (NOTFIRST (LIST FN (QUOTE &Y&) EX)) (T (SETQ NOTFIRST T) EX))))
        (COND ((EVAL B) (&FORRESET LST NIL) (RETURN &Y&)))
        (SETQ LST
              (MAPCAR (FUNCTION
                       (LAMBDA (&X&) (CONS (CAR &X&) (COND ((CADDAR &X&) (EVAL (CDDR &X&))) (T (CDDR &X&))))))
                      LST))
        (GO LOOP)))
EXPR)

(DEFPROP &FORSTOP
 (LAMBDA (L) (AND L (OR (NULL (CDAR L)) (&FORSTOP (CDR L)))))
EXPR)

(DEFPROP &FORRESET
 (LAMBDA (L &Y&)
  (MAPCAR (FUNCTION
           (LAMBDA (&X&)
            (COND ((CADDDR (CAR &X&)) (SET (CAAR &X&) (CADDDR (CDAR &X&))))
                  (T (AND &Y& (NULL (CDR &X&)) (SET (CAAR &X&) NIL))))))
          L))
EXPR)

(DEFPROP &RANGE
 (LAMBDA (LOW UP INC) (COND ((EQUAL INC 0) NIL) (T (&RANGE1 LOW UP INC (*GREAT INC 0) (*LESS INC 0)))))
EXPR)

(DEFPROP &RANGE1
 (LAMBDA (LOW UP INC POS NEG)
  (COND ((OR (AND POS (*GREAT LOW UP)) (AND NEG (*LESS LOW UP))) NIL)
        (T (LIST LOW (QUOTE &RANGE1) (*PLUS LOW INC) UP INC POS NEG))))
EXPR)

(DEFPROP &DO
 (LAMBDA (FN EX B) (PROG (V) L (SETQ V (FN V (EVAL EX))) (COND ((EVAL B) (RETURN V)) (T (GO L)))))
EXPR)

(DEFPROP &WHILE
 (LAMBDA (FN B EX) (PROG (V) L (COND ((EVAL B) (SETQ V (FN V (EVAL EX)))) (T (RETURN V))) (GO L)))
EXPR)

(DEFPROP &INDEX
 (LAMBDA (L X) (COND (X (CAR (SUFLIST (CAR (SUFLIST L (SUB1 (CAR X)))) (SUB1 X)))) (T L)))
EXPR)
