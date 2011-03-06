; (declare (genprefix setup))

;;;################################################################
;;;
;;;           SETUP - initialization file for SHRDLU
;;;
;;;################################################################

(setq parsings 0) ;atom used in the timing package

(SETQ ELSE
      T
      SAVESENT
      NIL
      ALTMODE
      (ASCII 27.)
      DOT
      (ASCII 46.)
      *1
      '[1]
      *2
      '[2]
      *3
      '[3]
      *4
      '[4]
      *5
      '[5]
      *6
      '[6]
      LASTSENTNO
      0.
      SENTNO
      1.
      UNMKD
      '(COMPONENT BOTH)
      LASTIME
      NIL) 

(SETQ DPSTOP
      NIL
      NODE-STOP
      NIL
      SMN-STOP
      NIL
      ERT-TIME
      0.
      ALTMODE
      (LIST (ASCII 27.))
      BREAKCHARS
      (LIST #\Space #\Newline)
      LINEL
      65.
      =LINE
      '========================================================) 

;(OR (GET 'CLAUSE 'SUBR)
;    (LABELTRACE CLAUSE NG VG ADJG PREPG CONJOIN)) 

;;*PAGE

;;;**********************************************************************
;;;            SWITCHES AND SWITCH-SETTING PACKAGES
;;;**********************************************************************

(SETQ FEATURESWITCHES '(MOBYREAD DISCOURSE NOMEM IASSUME TIMID)) 

(SETQ PAUSESWITCHES
      '(ANS-AFTEREVALUATION-PAUSE ANS-AFTERFORMULATION-PAUSE
				  EVALANS-PAUSE
				  SH-SENT-PAUSE
				  SH-BEFOREANSWER-PAUSE
				  SH-FINISHED-PAUSE
				  PNS-BK
				  PLNRSEE-PAUSE)) 

(SETQ CONTROLSWITCHES '(NOSTOP ANSWER?
			       SMN
			       TOPLEVEL-ERRSET?
			       ERT-ERRSET?
			       MAKEINTERN)) 

(SETQ DISPLAYSWITCHES '(PARSETRACE PARSEBREAK
				   PARSENODE-SEE
				   LABELTRACE
				   MAKE-VERBOSE
				   LABELBREAK
				   BUILDSEE
				   BUILD-SEE
				   PLANNERSEE
				   SH-PRINT-TIME)) 

;;;*************************

(SETQ MAKE-VERBOSE
      NIL
      PARSETRACE
      NIL
      PARSEBREAK
      NIL
      PARSENODE-SEE
      NIL
      LABELTRACE
      NIL
      LABELBREAK
      NIL
      BUILDSEE
      NIL
      BUILD-SEE
      NIL
      PLANNERSEE
      NIL
      SH-PRINT-TIME
      NIL) 

(SETQ MOBYREAD
      NIL
      DISCOURSE
      T
      WANT-DISPLAY
      NIL
      NOMEM
      NIL
      IASSUME
      T
      TIMID
      200.) 

(SETQ MAKEINTERN NIL) 

(SETQ SH-BEFOREANSWER-PAUSE
      NIL
      ANS-AFTEREVALUATION-PAUSE
      NIL
      ANS-AFTERFORMULATION-PAUSE
      NIL
      EVALANS-PAUSE
      NIL
      NOSTOP
      NIL
      NEVERSTOP
      NIL
      ANSWER?
      T
      SMN
      NIL
      DOIT
      NIL
      TOPLEVEL-ERRSET?
      NIL
      ERT-ERRSET?
      T
      SH-PARSE-PAUSE
      NIL
      SH-PARSESMNTC-PAUSE
      NIL
      SH-AFTERANSWER-PAUSE
      NIL
      PNS-BK
      NIL
      PLNRSEE-PAUSE
      NIL
      BYPASS-PLANNER
      NIL) 

;;;***********************************

(DEFUN QUIETMODE NIL 
       (MAPC #'(LAMBDA (X) (SET X NIL)) DISPLAYSWITCHES)) 

(DEFUN NOPAUSES NIL 
       (MAPC #'(LAMBDA (X) (SET X NIL)) PAUSESWITCHES)) 

(DEFUN NORMALFEATUREMODE NIL 
       (SETQ MOBYREAD NIL DISCOURSE T NOMEM NIL IASUME T TIMID 200.)) 

(DEFUN USERMODE NIL 
       (QUIETMODE)
       (NORMALFEATUREMODE)
       (NOPAUSES)
       (SETQ NOSTOP
	     T
	     NEVERSTOP
	     T
	     ANNOYANCE
	     T
	     ANSWER?
	     T
	     SH-STANDARD-PRINTOUT
	     NIL
	     SMN
	     NIL
	     TOPLEVEL-ERRSET?
	     T
	     ERT-ERRSET
	     T)
       (SETQ *RSET NIL))

(DEFUN DEBUGMODE NIL 
       (QUIETMODE)
       (NORMALFEATUREMODE)
       (NOPAUSES)
       (SETQ NOSTOP
	     NIL
	     NEVERSTOP
	     NIL
	     ANSWER?
	     T
	     SMN
	     NIL
	     PLANNERSEE
	     'ALL
	     PARSETRACE
	     'ALL
	     TOPLEVEL-ERRSET?
	     NIL
	     ERT-ERRSET
	     T)
       (SETQ *RSET T)
) 

(SETQ ZOG-USER NIL ZOGUSER NIL) 

;;;*******************************

 

;;*PAGE

;;;*****************************************************************
;;;           INITIALIZATION ROUTINES
;;;*****************************************************************

(DEFUN INITIALSTUFF (version note) 
       (TERPRI)
       (TERPRI)
       (SAY "SHRDLU version ")
       (PRINC VERSION)
       (TERPRI)
       (SAY "Loaded ")
       (PRINC (NTH-VALUE 4 (GET-DECODED-TIME)))
       (PRINC '/)
       (PRINC (NTH-VALUE 3 (GET-DECODED-TIME)))
       (PRINC '/)
       (PRINC (NTH-VALUE 5 (GET-DECODED-TIME)))
       (PRINC '\ )
       (SAY "in ")
       (PRINC (LISP-IMPLEMENTATION-TYPE))
       (PRINC '\ )
       (PRINC (LISP-IMPLEMENTATION-VERSION))
       (TERPRI)
       (TERPRI)
       (AND NOTE (PROGN (TERPRI)(APPLY-SAY NOTE)
            (TERPRI)(TERPRI)))
       (OR NEVERSTOP
         (PROG NIL
           (SAY "You are now in a read-eval-print loop.")
           (TERPRI)
           (SAY "Type \"go\" to enter ready state.")))
       (CATCH 'ABORT-PARSER (ERT))
       (SHRDLU)) 

(DEBUGMODE) 

(setq sh-standard-printout t smnbreak nil smntrace nil annoyance nil)

(setq errlist nil)
