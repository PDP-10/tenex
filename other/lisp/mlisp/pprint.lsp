

(DEFPROP &X&
 T
SPECIAL)

(DEFPROP &Y&
 T
SPECIAL)

(DEFPROP PPRINTQ
 T
*FEXPR)

(DEFPROP PPRINTQ
 (LAMBDA (L) (PPRINT L NIL))
FEXPR)

(DEFPROP PPRINT
 (LAMBDA (L EXPAND)
  (PROG (INDS)
        (SETQ INDS (QUOTE (VALUE SPECIAL MACRO EXPR FEXPR)))
        (PROG (&V &L1 A)
              (SETQ &L1 L)
         LOOP (COND ((NULL &L1) (RETURN &V)))
              (SETQ A (CAR &L1))
              (SETQ &L1 (CDR &L1))
              (SETQ &V
                    (COND ((ATOM A)
                           (PROG (&V &L1 IND)
                                 (SETQ &L1 INDS)
                            LOOP (COND ((NULL &L1) (RETURN &V)))
                                 (SETQ IND (CAR &L1))
                                 (SETQ &L1 (CDR &L1))
                                 (SETQ &V
                                       (COND ((GET A IND)
                                              (DPRINT (LIST (QUOTE DEFPROP) A (GET A IND) IND) EXPAND))
                                             (T NIL)))
                                 (GO LOOP)))
                          (T (SETQ INDS A))))
              (GO LOOP))))
EXPR)

(DEFPROP DPRINT
 (LAMBDA (SEXPR EXPAND)
  (COND ((AND (NOT (ATOM SEXPR)) (EQ (CAR SEXPR) (QUOTE DEFPROP)))
         (PROG NIL
               (PRINC (TERPRI (QUOTE "(DEFPROP ")))
               (PRIN1 (CADR SEXPR))
               (TERPRI (SPRINT (TERPRI (CADDR SEXPR)) 2 EXPAND))
               (PRIN1 (CADDDR SEXPR))
               (TERPRI (PRINC (QUOTE ")")))))
        (T (TERPRI (SPRINT (TERPRI SEXPR) 1 EXPAND)))))
EXPR)

(DEFPROP SPRINT
 (LAMBDA (SEXP COLUMN EXPAND) (&SPRINT1 (COND (EXPAND (EXPANDMACROS SEXP)) (T SEXP)) COLUMN 0))
EXPR)

(DEFPROP &SPRINT1
 (LAMBDA (SEXP COLUMN RIGHTEND)
  (PROG NIL
        (COND ((*GREAT (&COL) COLUMN) (TERPRI NIL)))
        (PROG (&V &L1 &UPPER1 &X&)
              (SETQ &L1 (ADD1 (&COL)))
              (SETQ &UPPER1 COLUMN)
         LOOP (COND ((*GREAT &L1 &UPPER1) (RETURN &V)))
              (SETQ &X& &L1)
              (SETQ &L1 (ADD1 &L1))
              (SETQ &V (PRINC (QUOTE " ")))
              (GO LOOP))
        (COND ((OR (ATOM SEXP) (*LESS (*PLUS (FLATSIZE SEXP) RIGHTEND) (CHRCT))) (RETURN (PRIN1 SEXP))))
        (PRINC (QUOTE "("))
        (COND
         ((AND (ATOM (CAR SEXP))
               (OR (NOT (*LESS (LENGTH SEXP) 3)) (EQUAL (FLATSIZE (CAR SEXP)) 1))
               (*LESS (&SIZE SEXP) (CHRCT)))
          (PROG (PR)
                (COND ((SETQ PR (EQ (PRIN1 (CAR SEXP)) (QUOTE PROG)))
                       (SPRINT (CAR (SETQ SEXP (CDR SEXP))) (SETQ COLUMN (ADD1 (&COL))) NIL))
                      ((EQ (CAR SEXP) (QUOTE LAMBDA))
                       (SETQ COLUMN (*DIF (&COL) 6))
                       (SPRINT (CAR (SETQ SEXP (CDR SEXP))) (ADD1 (&COL)) NIL))
                      (T (SETQ COLUMN (ADD1 (&COL)))))
                (PROG NIL
                 LOOP (COND
                       ((NOT
                         (COND
                          ((&DOT (CDR SEXP)
                                 (COND ((AND PR (ATOM (CADR SEXP))) (*DIF COLUMN 5)) (T COLUMN))
                                 RIGHTEND)
                           (PRIN1 (CDDR SEXP)))
                          ((CDR (SETQ SEXP (CDR SEXP))) (AND (*LESS COLUMN (&COL)) (TERPRI NIL)))
                          (T T)))
                        (GO LOOP))))))
         (T (SETQ COLUMN (&COL))
            (PROG NIL
             LOOP (COND
                   ((NOT
                     (COND ((&DOT SEXP COLUMN RIGHTEND) (PRIN1 (CDR SEXP)))
                           ((SETQ SEXP (CDR SEXP)) (TERPRI NIL))
                           (T T)))
                    (GO LOOP))))))
        (PRINC (QUOTE ")"))))
EXPR)

(DEFPROP &DOT
 (LAMBDA (SEXP COLUMN RIGHTEND)
  (PROG2 (&SPRINT1 (CAR SEXP)
                   COLUMN
                   (COND ((NULL (SETQ SEXP (CDR SEXP))) (ADD1 RIGHTEND))
                         ((ATOM SEXP) (*PLUS (*PLUS RIGHTEND 4) (FLATSIZE SEXP)))
                         (T 0)))
         (COND ((AND SEXP (ATOM SEXP)) (PRINC (QUOTE " . "))) (T NIL))))
EXPR)

(DEFPROP &SIZE
 (LAMBDA (SEXP)
  (COND ((OR (ATOM SEXP) (ATOM (CDR SEXP))) (*PLUS (FLATSIZE SEXP) 17))
        (T (*PLUS (*PLUS (FLATSIZE (CAR SEXP)) 2) (&SIZE (CADR SEXP))))))
EXPR)

(DEFPROP &COL
 (LAMBDA NIL (ADD1 (*DIF (LINELENGTH NIL) (CHRCT))))
EXPR)

(DEFPROP EXPANDMACROS
 (LAMBDA (SEXP)
  (COND ((OR (ATOM SEXP) (EQ (CAR SEXP) (QUOTE QUOTE))) SEXP)
        ((&ISMACRO (CAR SEXP)) (EXPANDMACROS ((LAMBDA (M S) (M S)) (GET (CAR SEXP) (QUOTE MACRO)) SEXP)))
        (T (CONS (EXPANDMACROS (CAR SEXP)) (&EXPANDREST (CDR SEXP))))))
EXPR)

(DEFPROP &EXPANDREST
 (LAMBDA (L) (COND ((ATOM L) L) (T (CONS (EXPANDMACROS (CAR L)) (&EXPANDREST (CDR L))))))
EXPR)

(DEFPROP &ISMACRO
 (LAMBDA (A) (AND (ATOM A) (NOT (NUMBERP A)) (GET A (QUOTE MACRO))))
EXPR)
