

(DEFPROP &FILENAME
 T
SPECIAL)

(DEFPROP &CURFN
 T
SPECIAL)

(DEFPROP &ECNT
 T
SPECIAL)

(DEFPROP &RCNT
 T
SPECIAL)

(DEFPROP &SPECS
 T
SPECIAL)

(DEFPROP &FNS
 T
SPECIAL)

(DEFPROP &X&
 T
SPECIAL)

(DEFPROP &Y&
 T
SPECIAL)

(DEFPROP SCNVAL
 T
SPECIAL)

(DEFPROP &SCANVAL
 T
SPECIAL)

(DEFPROP &SCANTYPE
 T
SPECIAL)

(DEFPROP &IDTYPE
 T
SPECIAL)

(DEFPROP &STRTYPE
 T
SPECIAL)

(DEFPROP &NUMTYPE
 T
SPECIAL)

(DEFPROP &DELIMTYPE
 T
SPECIAL)

(DEFPROP BASE
 T
SPECIAL)

(DEFPROP IBASE
 T
SPECIAL)

(DEFPROP BLANK
 T
SPECIAL)

(DEFPROP CR
 T
SPECIAL)

(DEFPROP VT
 T
SPECIAL)

(DEFPROP LOC
 T
SPECIAL)

(DEFPROP CONLIST
 T
SPECIAL)

(DEFPROP GEN
 T
SPECIAL)

(DEFPROP REMOB
 T
SPECIAL)

(DEFPROP KLIST
 T
SPECIAL)

(DEFPROP BPORG
 T
SPECIAL)

(DEFPROP BPEND
 T
SPECIAL)

(DEFPROP MLISP
 T
*FEXPR)

(DEFPROP &LAP
 T
*FEXPR)

(DEFPROP MLISP
 (LAMBDA (L)
  (PROG (FILE TIM EX &FILENAME &CURFN &ECNT &RCNT &SPECS &FNS &X& &Y& &SCANVAL &SCANTYPE)
        (COND ((NOT (&ISDEVICE (CAR L))) (SETQ L (CONS (QUOTE DSK:) L))))
        (COND ((ATOM (SETQ &FILENAME (CADR L))) (SETQ FILE &FILENAME)) (T (SETQ FILE (CAR &FILENAME))))
        (INC (EVAL (LIST (QUOTE INPUT) (CAR L) &FILENAME)) NIL)
        (COND
         ((SETQ L (CDDR L))
          (COND ((CAR L)
                 (PRINTSTR (QUOTE "UCI MLISP DOES NOT COMPILE AUTOMATICALLY."))
                 (PRINTSTR (QUOTE "FIRST TRANSLATE TO LISP, THEN COMPILE THE LISP."))
                 (GO EXIT))
                (T (SETQ &Y& (EVAL (LIST (QUOTE OUTPUT) (QUOTE DSK:) (CONS FILE (QUOTE LSP)))))))))
        (CSYM &M000)
        (PRINTSTR (TERPRI (TERPRI (QUOTE "*****"))))
        (SETQ TIM (TIME))
        (COND
         ((AND (SETQ EX (MTRANS)) (NOT (EQUAL EX (QUOTE (PROG NIL)))))
          (SETQ &FNS (CONS (PRINTTY (QUOTE RESTART)) &FNS))
          (PUTPROP (QUOTE RESTART) (LIST (QUOTE LAMBDA) NIL EX) (QUOTE EXPR))))
        (SETQ TIM (*QUO (*DIF (TIME) TIM) 1750))
        (PRINTSTR (TERPRI (QUOTE "*****")))
        (CSYM G0000)
        (COND ((NOT (EQ &SCANVAL (QUOTE /.))) (&ERROR (QUOTE "END OF PROGRAM NOT A PERIOD"))))
        (PRINTSTR (CAT TIM (QUOTE " SECONDS TRANSLATION TIME")))
        (PRINTSTR (CAT &ECNT (QUOTE " ERRORS DETECTED")))
        (PRINTSTR (CAT &RCNT (QUOTE " FUNCTIONS REDEFINED")))
        (TERPRI (INC NIL T))
        (COND ((NULL L) (COND ((GET (QUOTE RESTART) (QUOTE EXPR)) (RESTART))) (GO EXIT)))
        (SETQ &Y& NIL)
        (MAPCAR (FUNCTION
                 (LAMBDA (&X&)
                  (COND ((NOT (MEMBER &X& &Y&)) (PUTPROP &X& T (QUOTE SPECIAL)) (SETQ &Y& (CONS &X& &Y&))))))
                &SPECS)
        (PRINTSTR (QUOTE "SPECIAL DECLARATIONS:"))
        (TERPRI (TERPRI (PRINT (SETQ &SPECS &Y&))))
        (MAPCAR (FUNCTION
                 (LAMBDA (&X&)
                  (COND ((GET &X& (QUOTE SPECIAL)) (&WARNING (QUOTE "FUNCTION ALSO DECLARED SPECIAL") &X&)))))
                (SETQ &FNS (REVERSE &FNS)))
        (PRINTSTR (CAT (QUOTE "PRINTING ONTO ") (CAT FILE (QUOTE ".LSP"))))
        (TERPRI (OUTC T (TERPRI NIL)))
        (SETQ BASE 10)
        (SETQ L (OR (NULL (CDR L)) (CADR L)))
        (PPRINT (CONS (QUOTE (SPECIAL)) &SPECS) NIL)
        (PPRINT (CONS (QUOTE (*FEXPR *LEXPR)) &FNS) NIL)
        (PPRINT (CONS (QUOTE (MACRO)) &FNS) L)
        (PPRINT (CONS (QUOTE (EXPR FEXPR)) &FNS) L)
        (SETQ BASE 12)
   EXIT (OUTC NIL T)
        (INC NIL T)
        (RETURN (TERPRI (QUOTE ***-END-OF-RUN-***)))))
FEXPR)

(DEFPROP MTRANS
 (LAMBDA NIL
  (PROG (EX)
        (SETQ &SPECS (SETQ &FNS NIL))
        (SETQ &ECNT (SETQ &RCNT 0))
        (SETQ &CURFN (QUOTE TOP-LEVEL))
        (SETQ &X& T)
        (SCANSET)
        (&SCAN)
        (SETQ EX (&EXPR))
        (SCANRESET)
        (RETURN EX)))
EXPR)

(DEFPROP MEVAL
 (LAMBDA NIL
  (PROG (MODE &X&)
        (PRINC (TERPRI (QUOTE "WELCOME TO MLISP. TYPE `HELP;' FOR HELP.")))
        (SCANSET)
        (SETQ MODE (QUOTE M))
        (PROG (&V)
         LOOP (COND (T (SETQ &V
                             (PROG NIL
                                   (PRINC (TERPRI (TERPRI MODE)))
                                   (SETQ &X& (COND ((EQ MODE (QUOTE M)) (MTRANS)) (T (READ))))
                                   (COND ((EQ &X& (QUOTE LISP)) (SCANRESET) (SETQ MODE (QUOTE L)))
                                         ((EQ &X& (QUOTE MLISP)) (SCANSET) (SETQ MODE (QUOTE M)))
                                         ((EQ &X& (QUOTE HELP))
                                          (PROG NIL
                                                (EVAL (QUOTE (INC (INPUT HELP SYS: (HELP . MLI)) NIL)))
                                                (PRINTSTR (READ))
                                                (INC NIL T)))
                                         ((EQ MODE (QUOTE M))
                                          (SCANRESET)
                                          (ERRSET (PRINT (EVAL &X&)) T)
                                          (SCANSET))
                                         (T (ERRSET (PRINT (EVAL &X&)) T))))))
                    (T (RETURN &V)))
              (GO LOOP))))
EXPR)

(DEFPROP &EXPR
 (LAMBDA NIL (&HIER 0 (&SIMPEX)))
EXPR)

(DEFPROP &HIER
 (LAMBDA (RBP EX)
  (COND
   ((OR (EQ &SCANTYPE &NUMTYPE) (EQ &SCANTYPE &STRTYPE)) (&ERROR (QUOTE "ILLEGAL INFIX OPERATOR")))
   ((*GREAT RBP (&BINDINGPOWER &SCANVAL (QUOTE &LEFT))) EX)
   (T (&HIER1 RBP EX (&BINDINGPOWER &SCANVAL (QUOTE &RIGHT))))))
EXPR)

(DEFPROP &HIER1
 (LAMBDA (RBP EX RBP1)
  (&HIER RBP (&TINFIX (&ADVANCE &SCANVAL) (&NEXTDELIM (QUOTE )) EX (&HIER RBP1 (&SIMPEX)))))
EXPR)

(DEFPROP &SIMPEX
 (LAMBDA NIL
  ((LAMBDA (EX)
    (COND ((&NEXTDELIM (QUOTE /[))
           (LIST (QUOTE &INDEX) EX (CONS (QUOTE LIST) (&ARGS (QUOTE /]) (QUOTE "ILLEGAL INDEX EXPRESSION")))))
          (T EX)))
   (COND ((&ID) (&TFNCALL (&ADVANCE &SCANVAL)))
         ((EQ &SCANTYPE &NUMTYPE) (&ADVANCE &SCANVAL))
         ((GET &SCANVAL (QUOTE &RESWORD))
          (COND ((&NEXT (QUOTE BEGIN)) (CONS (QUOTE PROG) (CONS (&TDECL NIL) (&EXPRLIST))))
                ((&NEXT (QUOTE IF)) (CONS (QUOTE COND) (&TCOND (&EXPR))))
                ((&NEXT (QUOTE FOR)) (&TFOR))
                ((&NEXT (QUOTE WHILE)) (&TWHILE (&QEXPR)))
                ((&NEXT (QUOTE DO)) (&TDO (QUOTE (QUOTE PROG2)) (&QEXPR) (QUOTE DO)))
                ((&NEXT (QUOTE COLLECT)) (&TDO (QUOTE (QUOTE APPEND)) (&QEXPR) (QUOTE COLLECT)))
                ((&NEXT (QUOTE LAMBDA)) (&TLAMBDA T))
                ((&NEXT (QUOTE DEFINE)) (&TDEFINE))
                ((&NEXT (QUOTE COMMENT)) (&SEMISKIP) (&SCAN) (&SIMPEX))
                ((GET &SCANVAL (QUOTE &FNTYPE)) (&TFN (&ADVANCE &SCANVAL) (&ADVANCE &SCANVAL)))
                ((EQ &SCANVAL (QUOTE OCTAL)) (&OCTALNUM))
                ((EQ &SCANVAL (QUOTE INLINE)) (&INLINECODE))
                (T (&ERROR (QUOTE "ILLEGAL RESERVED WORD BEGINNING AN EXPRESSION")))))
         ((GET &SCANVAL (QUOTE &PREFIX)) (&TPREFIX (&ADVANCE &SCANVAL) (&NEXTDELIM (QUOTE ))))
         ((EQ &SCANVAL (QUOTE ')) (&ADVANCE (LIST (QUOTE QUOTE) (SREAD))))
         ((&NEXTDELIM (QUOTE /()) (&TPAREN (&EXPR)))
         ((&NEXTDELIM (QUOTE <))
          (CONS (QUOTE LIST) (&ARGS (QUOTE >) (QUOTE "ILLEGAL EXPRESSION IN LIST BRACKETS"))))
         ((EQ &SCANTYPE &STRTYPE) (&ADVANCE (LIST (QUOTE QUOTE) &SCANVAL)))
         (T (&ERROR (QUOTE "ILLEGAL SYMBOL BEGINNING A SIMPLE EXPRESSION"))))))
EXPR)

(DEFPROP &TPREFIX
 (LAMBDA (FN VOP) (&TP1 FN VOP (&HIER (&BINDINGPOWER FN (QUOTE &RIGHT)) (&SIMPEX))))
EXPR)

(DEFPROP &TP1
 (LAMBDA (FN VOP EX)
  (COND ((EQ FN (QUOTE PLUS)) EX)
        ((AND (EQ FN (QUOTE DIFFERENCE)) (SETQ FN (QUOTE MINUS)) (NUMBERP EX) (NOT VOP)) (MINUS EX))
        (VOP (LIST (QUOTE &VECTOR) T (LIST (QUOTE QUOTE) FN) EX NIL))
        (T (LIST FN EX))))
EXPR)

(DEFPROP &TINFIX
 (LAMBDA (FN VOP X Y)
  (COND ((EQ FN (QUOTE _))
         (COND (VOP (LIST (QUOTE &DECOMPOSE) X Y))
               ((ATOM X) (LIST (QUOTE SETQ) X Y))
               ((EQ (CAR X) (QUOTE &INDEX)) (&TREPLACE (CADR X) (CADDR X) Y (GENSYM)))
               ((ATOM (CAR X)) (LIST (QUOTE STORE) X Y))
               (T (&ERROR (CAT (QUOTE "ILLEGAL ASSIGNMENT TO  ") X)))))
        (VOP (LIST (QUOTE &VECTOR) NIL (LIST (QUOTE QUOTE) FN) X Y))
        ((AND (EQ Y 1) (EQ FN (QUOTE PLUS))) (LIST (QUOTE ADD1) X))
        ((AND (EQ Y 1) (EQ FN (QUOTE DIFFERENCE))) (LIST (QUOTE SUB1) X))
        ((AND (GET FN (QUOTE &ASSOC)) (NOT (ATOM X)) (EQ FN (CAR X))) (APPEND X (LIST Y)))
        (T (LIST FN X Y))))
EXPR)

(DEFPROP &TDECL
 (LAMBDA (L)
  (COND ((&NEXT (QUOTE NEW)) (&TDECL (APPEND L (&VARS (QUOTE ;) NIL NIL))))
        ((&NEXT (QUOTE SPECIAL)) (&TDECL (PROG2 (&VARS (QUOTE ;) T NIL) L)))
        (T L)))
EXPR)

(DEFPROP &EXPRLIST
 (LAMBDA NIL
  (PROG (EX L X)
   LOOP (COND ((SETQ EX (&EXPR)) (SETQ L (CONS EX L))))
        (SETQ X (&NEXTDELIM (QUOTE ;)))
        (COND ((&NEXT (QUOTE END)) (RETURN (REVERSE L)))
              (X (GO LOOP))
              (T (&ERROR (QUOTE "MISSING SEMICOLON AFTER EXPRESSION"))))))
EXPR)

(DEFPROP &TCOND
 (LAMBDA (EX)
  (COND ((&NEXT (QUOTE THEN)) (&TC1 (CONS EX (&TALSO (&EXPR)))))
        (T (&ERROR (QUOTE "ILLEGAL EXPRESSION AFTER IF")))))
EXPR)

(DEFPROP &TC1
 (LAMBDA (L)
  (COND ((&NEXT (QUOTE ELSE))
         (COND ((&NEXT (QUOTE IF)) (CONS L (&TCOND (&EXPR)))) (T (LIST L (CONS T (&TALSO (&EXPR)))))))
        (T (LIST L))))
EXPR)

(DEFPROP &TALSO
 (LAMBDA (EX) (COND ((&NEXT (QUOTE ALSO)) (CONS EX (&TALSO (&EXPR)))) (T (LIST EX))))
EXPR)

(DEFPROP &TFOR
 (LAMBDA NIL
  (LIST (QUOTE &FOR)
        (LIST (QUOTE QUOTE) (&FORCLAUSE))
        (LIST (QUOTE QUOTE)
              (COND ((&NEXT (QUOTE DO)) (QUOTE PROG2))
                    ((&NEXT (QUOTE COLLECT)) (QUOTE APPEND))
                    ((&NEXTDELIM (QUOTE ;)) (&ADVANCE &SCANVAL))
                    (T (&ERROR (QUOTE "EXPECTED DO, COLLECT OR ; IN FOR-LOOP")))))
        (&QEXPR)
        (COND ((&NEXT (QUOTE UNTIL)) (&QEXPR)) (T (QUOTE (QUOTE NIL))))))
EXPR)

(DEFPROP &FORCLAUSE
 (LAMBDA NIL
  (CONS (CONS (COND ((&NEXT (QUOTE NEW)) (QUOTE NEW)) (T (QUOTE OLD)))
              (CONS (COND ((&ID) (&ADVANCE &SCANVAL))
                          (T (&ERROR (QUOTE "NON-IDENTIFIER OR PREFIX AFTER FOR"))))
                    (COND ((&NEXT (QUOTE IN)) (LIST (QUOTE IN) (&EXPR)))
                          ((&NEXT (QUOTE ON)) (LIST (QUOTE ON) (&EXPR)))
                          ((&NEXTDELIM (QUOTE _))
                           (LIST (QUOTE _)
                                 (LIST (QUOTE &RANGE)
                                       (&EXPR)
                                       (COND ((&NEXT (QUOTE TO)) (&EXPR))
                                             (T (&ERROR (QUOTE "ILLEGAL LOWER LIMIT IN FOR-LOOP"))))
                                       (COND ((&NEXT (QUOTE BY)) (&EXPR)) (T 1)))))
                          (T (&ERROR (QUOTE "MISSING IN, ON, OR _ AFTER CONTROL VARIABLE IN FOR-LOOP"))))))
        (COND ((&NEXT (QUOTE FOR)) (&FORCLAUSE)) (T NIL))))
EXPR)

(DEFPROP &TDO
 (LAMBDA (FN EX X)
  (COND ((&NEXT (QUOTE UNTIL)) (LIST (QUOTE &DO) FN EX (&QEXPR)))
        (T (&ERROR (CAT (QUOTE "EXPECTED UNTIL IN ") (CAT X (QUOTE "-UNTIL EXPRESSION")))))))
EXPR)

(DEFPROP &TWHILE
 (LAMBDA (EX)
  (COND ((&NEXT (QUOTE DO)) (LIST (QUOTE &WHILE) (QUOTE (QUOTE PROG2)) EX (&QEXPR)))
        ((&NEXT (QUOTE COLLECT)) (LIST (QUOTE &WHILE) (QUOTE (QUOTE APPEND)) EX (&QEXPR)))
        (T (&ERROR (QUOTE "EXPECTED DO OR COLLECT IN WHILE EXPRESSION")))))
EXPR)

(DEFPROP &TDEFINE
 (LAMBDA NIL
  (PROG (&V)
   LOOP (SETQ &V
              (PROG (VAL TYP)
                    (COND
                     ((AND (NOT (EQ (SETQ TYP &SCANTYPE) &IDTYPE)) (NOT (EQ &SCANTYPE &DELIMTYPE)))
                      (&ERROR (QUOTE "ILLEGAL SYMBOL BEING DEFINED"))))
                    (SETQ VAL (&ADVANCE &SCANVAL))
                    (COND ((&NEXT (QUOTE PREFIX)) (&MAKPREFIX VAL)))
                    (COND
                     ((OR (AND (EQ &SCANTYPE &IDTYPE) (NOT (EQ &SCANVAL (QUOTE DIFFERENCE))))
                          (AND (EQ &SCANTYPE &DELIMTYPE) (NOT (MEMBER &SCANVAL (QUOTE (/, ;))))))
                      (PUTPROP &SCANVAL TYP (QUOTE &TRANSTYPE))
                      (PUTPROP (&ADVANCE &SCANVAL) VAL (QUOTE &TRANS))))
                    (COND
                     ((&NUMB VAL (QUOTE &LEFT))
                      (OR (&NUMB VAL (QUOTE &RIGHT)) (&ERROR (QUOTE "MISSING RIGHT BINDING POWER")))))))
        (COND ((NOT (&NEXTDELIM (QUOTE /,))) (RETURN &V)) (T (GO LOOP)))))
EXPR)

(DEFPROP &NUMB
 (LAMBDA (VAL IND)
  (COND ((EQ &SCANTYPE &NUMTYPE) (&ADVANCE (PUTPROP VAL &SCANVAL IND)))
        ((&NEXT (QUOTE DIFFERENCE))
         (COND ((EQ &SCANTYPE &NUMTYPE) (&ADVANCE (PUTPROP VAL (MINUS &SCANVAL) IND)))
               (T (&ERROR (QUOTE "ILLEGAL BINDING POWER")))))))
EXPR)

(DEFPROP &TFN
 (LAMBDA (IND &CURFN)
  (PROG (L)
        (&FNCHECK &CURFN)
        (PUTPROP &CURFN (SETQ L (&TLAMBDA NIL)) IND)
        (COND ((EQ IND (QUOTE EXPR)) (AND (EQ (LENGTH (CADR L)) 1) (&MAKPREFIX &CURFN)))
              ((EQ IND (QUOTE FEXPR)) (PUTPROP &CURFN T (QUOTE *FEXPR)))
              ((EQ IND (QUOTE LEXPR))
               (COND ((EQ (LENGTH (CADR L)) 1)
                      (PUTPROP &CURFN (LIST (QUOTE LAMBDA) (CAADR L) (CADDR L)) (QUOTE EXPR))
                      (PUTPROP &CURFN T (QUOTE *LEXPR)))
                     (T (&ERROR (CAT (QUOTE "LEXPRS MUST HAVE EXACTLY ONE ARGUMENT, NOT ") (CADR L))))))
              (T NIL))
        (SETQ &FNS (CONS &CURFN &FNS))))
EXPR)

(DEFPROP &FNCHECK
 (LAMBDA (X)
  (COND ((GETL X (QUOTE (EXPR FEXPR SUBR FSUBR MACRO)))
         (SETQ &RCNT (ADD1 &RCNT))
         (&WARNING (QUOTE "FUNCTION REDEFINED") X))
        (T (PRINTTY X))))
EXPR)

(DEFPROP &MAKPREFIX
 (LAMBDA (FN)
  (PROG NIL
        (OR (GET FN (QUOTE &RIGHT)) (PUTPROP FN 1750 (QUOTE &RIGHT)))
        (OR (GET FN (QUOTE &LEFT)) (PUTPROP FN -1 (QUOTE &LEFT)))
        (PUTPROP FN T (QUOTE &PREFIX))))
EXPR)

(DEFPROP &TLAMBDA
 (LAMBDA (ALLOW)
  (COND ((&NEXTDELIM (QUOTE /()) (&TL1 (&VARS (QUOTE /)) (&NEXT (QUOTE SPECIAL)) T) ALLOW))
        (T (&ERROR (QUOTE "'(' NEEDED FOR LAMBDA VARIABLES")))))
EXPR)

(DEFPROP &TL1
 (LAMBDA (L ALLOW)
  (COND ((&NEXTDELIM (QUOTE ;)) (&TL2 (LIST (QUOTE LAMBDA) L (&EXPR)) ALLOW))
        (T (&ERROR (QUOTE "';' NEEDED AFTER LAMBDA VARIABLES")))))
EXPR)

(DEFPROP &TL2
 (LAMBDA (EX ALLOW)
  (COND ((AND ALLOW (&NEXTDELIM (QUOTE ;)))
         (COND ((&NEXTDELIM (QUOTE /()) (CONS EX (&ARGS (QUOTE /)) (QUOTE "ILLEGAL LAMBDA ARGUMENT"))))
               (T (&ERROR (QUOTE "'(' NEEDED FOR LAMBDA ARGUMENTS")))))
        (T EX)))
EXPR)

(DEFPROP &VARS
 (LAMBDA (TERMIN ISSPEC ALLOW)
  (COND ((&NEXTDELIM TERMIN) NIL) (T (CONS (&TID ISSPEC) (&VAR1 TERMIN ISSPEC ALLOW)))))
EXPR)

(DEFPROP &VAR1
 (LAMBDA (TERMIN ISSPEC ALLOW)
  (COND ((&NEXTDELIM (QUOTE /,))
         (CONS (&TID (OR (AND ALLOW (&NEXT (QUOTE SPECIAL))) (AND (NOT ALLOW) ISSPEC)))
               (&VAR1 TERMIN ISSPEC ALLOW)))
        ((&NEXTDELIM TERMIN) NIL)
        (T (&ERROR (QUOTE "ILLEGAL PROG OR LAMBDA VARIABLE")))))
EXPR)

(DEFPROP &ARGS
 (LAMBDA (TERMIN MSG) (COND ((&NEXTDELIM TERMIN) NIL) (T (CONS (&EXPR) (&ARG1 TERMIN MSG)))))
EXPR)

(DEFPROP &ARG1
 (LAMBDA (TERMIN MSG)
  (COND ((&NEXTDELIM (QUOTE /,)) (CONS (&EXPR) (&ARG1 TERMIN MSG)))
        ((&NEXTDELIM TERMIN) NIL)
        (T (&ERROR MSG))))
EXPR)

(DEFPROP &TID
 (LAMBDA (ISSPEC)
  (COND ((&ID) (AND ISSPEC (SETQ &SPECS (CONS &SCANVAL &SPECS))) (&ADVANCE &SCANVAL))
        (T (&ERROR (QUOTE "NON-IDENTIFIER OR PREFIX USED IN FORMAL VARIABLE LIST")))))
EXPR)

(DEFPROP &TFNCALL
 (LAMBDA (X) (COND ((&NEXTDELIM (QUOTE /()) (CONS X (&ARGS (QUOTE /)) (QUOTE "ILLEGAL ARGUMENT")))) (T X)))
EXPR)

(DEFPROP &TREPLACE
 (LAMBDA (X L Y G)
  (LIST (QUOTE PROG2) (LIST (QUOTE SETQ) X (LIST (QUOTE &REPLACE) X L (LIST (QUOTE SETQ) G Y))) G))
EXPR)

(DEFPROP &TPAREN
 (LAMBDA (EX) (COND ((&NEXTDELIM (QUOTE /))) EX) (T (&ERROR (QUOTE "ILLEGAL PARENTHESIZED EXPRESSION")))))
EXPR)

(DEFPROP &OCTALNUM
 (LAMBDA NIL
  (PROG (IBASE)
        (SETQ IBASE 10)
        (&SCAN)
        (COND ((EQ &SCANTYPE &NUMTYPE) (RETURN (&ADVANCE &SCANVAL)))
              (T (&ERROR (QUOTE "RESERVED WORD OCTAL NOT FOLLOWED BY A NUMBER"))))))
EXPR)

(DEFPROP &INLINECODE
 (LAMBDA NIL
  (PROG (BASE IBASE)
        (SETQ BASE (SETQ IBASE 10))
        (COND
         ((OR (ATOM (SETQ &SCANVAL (SREAD))) (NOT (EQ (CAR &SCANVAL) (QUOTE LAP))))
          (&ERROR (QUOTE "INLINE CODE DOES NOT BEGIN WITH: (LAP <NAME> <IND>)"))))
        (&FNCHECK (CADR &SCANVAL))
        (COND (&Y&
               (PROG NIL
                     (OUTC T NIL)
                     (PRINT &SCANVAL)
                L    (COND ((PRINT (READ)) (GO L)) (T (OUTC (TERPRI NIL) NIL)))))
              (T (EVAL (CONS (QUOTE &LAP) (CDR &SCANVAL)))))
        (&SCAN)))
EXPR)

(DEFPROP &ERROR
 (LAMBDA (MSG)
  (PROG (PAGE LINE IFILE OFILE X)
        (SETQ &ECNT (ADD1 &ECNT))
        (SETQ PAGE (CAR (PGLINE)))
        (SETQ LINE (CDR (PGLINE)))
        (SETQ OFILE (OUTC NIL NIL))
        (TERPRI NIL)
        (PRINTSTR (CAT (QUOTE "*** ERROR IN ") &CURFN))
        (PRINTSTR (CAT (QUOTE "*** ") MSG))
        (PRINTSTR (CAT (QUOTE "*** CURRENT SYMBOL IS ") &SCANVAL))
        (COND ((NULL (SETQ IFILE (INC NIL NIL))) (GO MORE)))
        (PRINTSTR (CAT (QUOTE "*** LINE NUMBER ") (CAT LINE (CAT (QUOTE //) PAGE))))
        (PRINTSTR (QUOTE "*** TYPE E TO EDIT YOUR FILE, C TO CONTINUE"))
   LOOP (COND ((EQ (SETQ X (SREAD)) (QUOTE E))
               (AND (EQ (READCH) CR) (READCH))
               (PRINTSTR VT)
               (SWAP &FILENAME PAGE LINE))
              ((EQ X (QUOTE C)) (GO MORE))
              (T (PRINTSTR (CAT (QUOTE "TYPE E OR C, NOT  ") X)) (GO LOOP)))
   MORE (PRINTSTR (QUOTE "*** SKIPPING TO NEXT SEMICOLON"))
        (INC IFILE NIL)
        (OUTC OFILE NIL)
        (&SEMISKIP)))
EXPR)

(DEFPROP &WARNING
 (LAMBDA (MSG X)
  (PROG (OFILE)
        (SETQ OFILE (OUTC NIL NIL))
        (PRINC (TERPRI (QUOTE "*** WARNING ***, ")))
        (PRINC MSG)
        (PRINC (QUOTE ": "))
        (PRINTSTR X)
        (OUTC OFILE NIL)
        (RETURN X)))
EXPR)

(DEFPROP &SEMISKIP
 (LAMBDA NIL
  (PROG (&V)
   LOOP (COND ((NOT (AND (EQ &SCANVAL (QUOTE ;)) (EQ &SCANTYPE &DELIMTYPE))) (SETQ &V (&SCAN)))
              (T (RETURN &V)))
        (GO LOOP)))
EXPR)

(DEFPROP &SCAN
 (LAMBDA NIL
  (COND ((EQ (SETQ &SCANTYPE (SCAN)) &IDTYPE) (&SCAN1 (INTERN SCNVAL)))
        ((EQ &SCANTYPE &DELIMTYPE) (&SCAN1 (INTERN (ASCII SCNVAL))))
        (T (SETQ &SCANVAL SCNVAL))))
EXPR)

(DEFPROP &SCAN1
 (LAMBDA (X)
  (COND ((AND (GET X (QUOTE &TRANS)) &X&)
         (SETQ &SCANTYPE (GET X (QUOTE &TRANSTYPE)))
         (SETQ &SCANVAL (GET X (QUOTE &TRANS))))
        (T (SETQ &SCANVAL X))))
EXPR)

(DEFPROP &NEXT
 (LAMBDA (X) (COND ((EQ &SCANVAL X) (&ADVANCE T))))
EXPR)

(DEFPROP &NEXTDELIM
 (LAMBDA (X) (COND ((AND (EQ &SCANVAL X) (EQ &SCANTYPE &DELIMTYPE)) (&ADVANCE T))))
EXPR)

(DEFPROP &ADVANCE
 (LAMBDA (X) (PROG2 (&SCAN) X))
EXPR)

(DEFPROP &ID
 (LAMBDA NIL
  (AND (EQ &SCANTYPE &IDTYPE) (NOT (OR (GET &SCANVAL (QUOTE &RESWORD)) (GET &SCANVAL (QUOTE &PREFIX))))))
EXPR)

(DEFPROP &BINDINGPOWER
 (LAMBDA (X IND) (COND ((SETQ X (GET X IND)) X) (T (GET (QUOTE &DEFAULT) IND))))
EXPR)

(DEFPROP &QEXPR
 (LAMBDA NIL (LIST (QUOTE QUOTE) (&EXPR)))
EXPR)

(DEFPROP &ISDEVICE
 (LAMBDA (X)
  (OR (AND (ATOM X) (EQ (CAR (LAST (EXPLODEC X))) (QUOTE :))) (AND (NOT (ATOM X)) (NOT (ATOM (CDR X))))))
EXPR)

(DEFPROP &LAP
 (LAMBDA (X)
  (PROG (LOC CONLIST GEN REMOB)
        (SETQ GEN (GENSYM))
        (SETQ CONLIST (LIST NIL))
        (SETQ LOC BPORG)
   LOOP (COND ((NULL (SETQ &SCANVAL (SREAD))) (GO EXIT)) ((ATOM &SCANVAL) (GO A)) (T (GO I)))
   A    (DEFSYM &SCANVAL LOC)
        (GO LOOP)
   I    (DEPOSIT LOC (GWD &SCANVAL))
        (&BPCHECK)
        (GO LOOP)
   EXIT (DEFSYM GEN LOC)
        (MAPCAR (FUNCTION
                 (LAMBDA (Y)
                  (PROG NIL (SETQ KLIST (CONS (CONS Y LOC) KLIST)) (DEPOSIT LOC (GWD Y)) (&BPCHECK))))
                (CDR CONLIST))
        (PUTPROP (CAR X) (NUMVAL BPORG) (CADR X))
        (SETQ BPORG LOC)
        (MAPCAR (FUNCTION
                 (LAMBDA (Y)
                  (COND
                   ((AND (REMPROP Y (QUOTE SYM)) (GET Y (QUOTE UNDEF)))
                    (&ERROR (CAT (QUOTE "UNDEFINED LABEL USED IN INLINE CODE: ") Y))))))
                REMOB)))
FEXPR)

(DEFPROP &BPCHECK
 (LAMBDA NIL
  (COND ((NOT (*LESS (SETQ LOC (ADD1 LOC)) BPEND)) (&ERROR (QUOTE "BINARY PROGRAM SPACE EXCEEDED")))))
EXPR)
