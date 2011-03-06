
; (DECLARE (GENPREFIX SYSCOM)) 

;;;*********************************************************************
;;;
;;;                 SYSCOM    - TOPLEVEL AND GENERAL UTILITY FUNCTIONS
;;;
;;;**********************************************************************

(DEFUN SHRDLU NIL 
       (PROG (ERT-TIME END AMB TIMAMB BOTH BACKREF BACKREF2 ANSNAME
	      LASTREL WHO PT PTW SENT PUNCT IGNORE H N NB FE SM RE
	      MES MESP C CUT CURTIME STATE GLOBAL-MESSAGE LEVEL
	      P-TIME SMN-TIME PLNR-TIME ANS-TIME ANS-PLNR-TIME
	      SH-GCTIME) 
	     (DECLARE (SPECIAL LEVEL N CUT C END PLNR-TIME PT PTW NB
	                       BOTH GLOBAL-MESSAGE MES BACKREF WHO
			       LASTREL ANSNAME BACKREF2 FE
			       SH-GCTIME))
	     (CLEANOUT TSS EVX NODE ANS OSS RSS X)		       ;FLUSH OLD GENSYMS
	CATCH-LOOP
	     (CATCH 'ABORT-PARSER
	      (PROG NIL 
	       LOOP (SETQ SENTNO (+ 1 SENTNO) 
			  PARSINGS 0. 
			  LEVEL 0. 
			  LASTSENTNO (+ 1 LASTSENTNO) 
			  LASTSENT C 
			  GLOBAL-MESSAGE NIL 
			  MES '(NOPE)
			  BACKREF NIL 				       ;???????????????????
			  RUNTIME (GET-INTERNAL-RUN-TIME) 
			  SH-GCTIME 0.
			  PLNR-TIME 0. 
			  ANS-PLNR-TIME 0. 
			  SMN-TIME 0. 
			  ERT-TIME 0.
			  WHO NIL)
	       UP   (SETQ N (SETQ SENT (ETAOIN)))
		    (OR ANNOYANCE (PRINT *1))
		    (AND IGNORE (GO UP))
		    ;;;
		    (COND
		     ((AND
		       (SETQ PT (SETQ C (PARSEVAL PARSEARGS)))
		       C)
		      (OR ANNOYANCE (PRINT *2))
		      (SETQ FE (FE C))
		      (SETQ NB SENT)
		      (SETQ H (H C))
		      (SETQ INTERPRETATION (SM C))
		      (AND SH-BEFOREANSWER-PAUSE
			   (ERT BEFORE ANSWERING))
		      (COND
		       (SMN (AND SH-PARSE-PAUSE
				 (ERT PARSING COMPLETED))
			    (GO LOOP))
		       ((NOT ANSWER?)
			(AND SH-PARSESMNTC-PAUSE
			     (ERT ANALYSIS COMPLETED)))
		       ((TIME-ANSWER '(ANSWER C)))
		       ((APPLY-SAY
			       (OR GLOBAL-MESSAGE
				   '(I DON\'T UNDERSTAND\.))))))
		     ((OR ANNOYANCE (PRINT *3))
		      (APPLY-SAY
			     (OR GLOBAL-MESSAGE
				 '(I DON\'T UNDERSTAND\.)))))
		    (SHRDLU-TIMER)
		    (AND MOBYTEST-IN-PROGRESS (AFTER-EACH-SENTENCE))
		    (AND SH-STANDARD-PRINTOUT (SHSTPO))
		    (AND SH-AFTERANSWER-PAUSE (ERT))
		    (GO LOOP)))
	     (GO CATCH-LOOP))) 

(DEFUN TIMER (T0 T1) (/ (- T1 T0) 1000000.0)) 

(DEFUN PARSEVAL (A) 
       (PROG (P-TTIME P-GC SM-TIME MP-TIME RETURN-NODE) 
             (DECLARE (SPECIAL SM-TIME MP-TIME))
	     (SETQ P-GC 0.
		   SM-TIME 0. 
		   MP-TIME 0. 
		   P-TTIME (GET-INTERNAL-RUN-TIME))
	     (SETQ RETURN-NODE (EVAL (CONS 'PARSE A)))
	     (SETQ P-TIME (- (TIMER P-TTIME (GET-INTERNAL-RUN-TIME))
				      SM-TIME
				      PLNR-TIME))
	     (OR (= P-GC 0.)
		 (SETQ P-TIME
		       (- P-TIME
				   (TIMER P-GC 0))))
	     (SETQ SMN-TIME SM-TIME PLNR-TIME MP-TIME)
	     (RETURN RETURN-NODE))) 

(SETQ PARSEARGS '(CLAUSE MAJOR TOPLEVEL)) 

;;*page

;;;********************************************
;;;
;;;   test package !!  -experimental version
;;;
;;;********************************************
;;; how to use:
;;;
;;;   from within an  break at "READY", open, via uread, the file that
;;;   contains the sentences to be tested (see sample files on LANG;) and
;;;   open a file to write onto and do a (IOC r) whenever thing are set
;;;   up (remember that all prints will copy to the file after the ioc
;;;   is executed so a sneaky way to comment the output file is to say
;;;   "(say ...)" or some such.) 
;;;     Next set the (global) variable "mobytest-in-progress" to non-nil.
;;;   This will evade every break that the system does via ERTEX - that
;;;   should be all of them but at the moment (8/6/74) that can't be
;;;   guarenteed.
;;;     Functions below trap at the obvious places and could be tailored
;;;   to desired stuff.
;;;     At this point, the preliminaries are over; proceed the  break
;;;   and type a "m" and the next READY. - it should take off. 
;;;

(DEFUN AFTER-EACH-SENTENCE NIL 
       (COND (C (WALLP C) (DP (CAR (SM C)))))
       (TYO 12.)) 						       ;form feed

(DEFUN END-OF-FILE-CONDITION NIL 
       (AND ^R (UFILE SHTRCE >))
       (AND GO-AWAY (VALRET 'U))) 

(SETQ GO-AWAY NIL MOBYTEST-IN-PROGRESS NIL) 

;;*page

;;;********************************************************************************
;;;                        Fancy timing package
;;;********************************************************************************

(DEFUN SHRDLU-TIMER NIL 
       (PROG (BASE) 
	     (OR SH-PRINT-TIME (RETURN T))
	     (SETQ BASE 10.)
	     (TERPRI)
	     (PRINC 'TOTAL\ TIME\ USED\:\ )
	     (PRINC (TIMER RUNTIME (GET-INTERNAL-RUN-TIME)))
	     (PRINTC '\ \ AMOUNT\ SPENT\ IN\ GARBAGE\ COLLECTION)
	     (PRINC (TIMER SH-GCTIME 0))
	     (OR (EQ SH-PRINT-TIME 'FANCY) (RETURN T))
	     (TERPRI)
	     (PRINC 'BREAKDOWN\:)
	     (PRINTC '\ \ \ PARSING)
	     (PRINC P-TIME)
	     (PRINTC '\ \ \ SEMANTICS)
	     (PRINC SMN-TIME)
	     (PRINTC '\ \ \ MICROPLANNER)
	     (PRINTC '\ \ \ \ \ \ FOR\ SEMANTICS)
	     (PRINC PLNR-TIME)
	     (PRINTC '\ \ \ \ \ \ FOR\ ANSWERING)
	     (PRINC ANS-PLNR-TIME)
	     (PRINTC '\ \ \ ANSWERING)
	     (PRINC ANS-TIME)
	     (TERPRI))) 

(DEFUN TIME-ANSWER (REAL-CALL) 
       (PROG (MP-TIME SM-TIME PLNR-TIME ANS-TTIME GC RESULT) 
	     (SETQ MP-TIME 0. 
		   SM-TIME 0. 
		   GC 0.
		   ANS-TTIME (GET-INTERNAL-RUN-TIME) 
		   PLNR-TIME 0.)
	     (SETQ RESULT (EVAL REAL-CALL))
	     (SETQ ANS-TIME
		   (- (TIMER ANS-TTIME (GET-INTERNAL-RUN-TIME)) PLNR-TIME))
	     (OR (= GC 0.)
		 (SETQ ANS-TIME
		       (- ANS-TIME
				   (TIMER GC 0.))))
	     (SETQ ANS-PLNR-TIME 0 ;MPLNR-TIME 
		   SMN-TIME (+ SMN-TIME SM-TIME))
	     (RETURN RESULT))) 

(DEFUN PARSE-STATISTICS NIL 
       (COND ((= PARSINGS 0.)					       ;initialization
	      (SETF (GET 'PARSINGS 'WINS) 0.)))
       (AND RE
	    (SETF (GET 'PARSINGS 'WINS)
		     (+ 1 (GET 'PARSINGS 'WINS))))
       (SETQ PARSINGS (+ 1 PARSINGS))) 

;;; these next two are left over from previous incarnations
;;;(DEFUN TIMER NIL 
;;;       (AND SH-PRINT-TIME
;;;	    (PRINT 'TIME-USED)
;;;	    (PRINC (- (TIME-SINCE RUNTIME) ERT-TIME)))) 

(DEFUN TIME-SINCE (X) (/ (- (GET-INTERNAL-RUN-TIME) X) 1000000.0)) 

;;*page

;;;****************************************************************
;;;        Functions that extract input from the user
;;;****************************************************************

(DEFUN-FEXPR INTEROGATE (MESSAGE) 
       (PROG (CH) 
	MES  (MAPC #'PRINT3 MESSAGE)
	     (TERPRI)
	     (COND ((MEMQ (SETQ CH (READ-CHAR)) '(Y \y))
		    (RETURN T))
		   ;;;  ((EQ CH '?)
		   ;;;   (EVAL (GET 'FLUSH 'EXPLANATION))
		   ;;;   (GO MES))
		   (T (RETURN NIL))))) 

(DEFUN-FEXPR DEFLIST
	 (LIST) (MAPC #'(LAMBDA (A) 
				(SETF (GET (CAR A) (CAR LIST))
					 (CADR A)))
			      (CDR LIST))
			(CAR LIST))

;;*PAGE

;;;****************************************************************
;;;           specialized and not so, output routines
;;;**************************************************************** 

(DEFUN % NIL 							       ;THIS FUNCTION PRINTS THE CURRENT SENTENCE
       (TERPRI)
       (MAPC 'PRINT3 SENT)
       (PRINC PUNCT)) 

(DEFUN DA (X) 
       (AND
	(GET X 'THASSERTION)
	(DISP
	 (APPLY 'APPEND
		(MAPCAR 'CDDR
			(APPLY 'APPEND
			       (MAPCAR 'CDR
				       (CDR (GET X
						 'THASSERTION))))))))) 

(DEFUN DISP
	 (&REST 0A) 
	 	(TERPRI)
	 	(AND (CONSP 0A)
		   (PRINC (CAR 0A))
		   (PRINC '\ >>\ )
		   (PRINC (CADR 0A))
		   (TERPRI))
		(PRINT (CDDR 0A)))

(DEFUN DTABLE (L) 
       (PRINT =LINE)
       (MAPC '(LAMBDA (X) 
		      (PRINTC (TAB 5.) X (TAB 22.) '= (EVAL X))
		      (COND ((GET X 'TURNED)
			     (TAB 30.)
			     (PRINC (LIST (GET X 'TURNED))))))
	     L)
       (PRINTC =LINE)) 

(DEFUN DP (X) 
       (PROG (PLIST) 
	     (TERPRI)
	     (TERPRI)
	     (PRINC '[)
	     (PRINC X)
	     (PRINC '])
	     (SETQ PLIST (SYMBOL-PLIST X))
	A    (COND ((MEMQ (CAR PLIST) '(PNAME VALUE)) (GO B)))
	     (TERPRI)
	     (TAB 4.)
	     (PRINC (CAR PLIST))
	     (PRINC (CADR PLIST))
;	     (SPRINT (CADR PLIST) (*DIF LINEL 18.) 18.)
	B    (COND ((SETQ PLIST (CDDR PLIST)) (GO A)))
	     (TERPRI)
	     (AND DPSTOP (ERT))
	     (RETURN '*))) 

(DEFUN-FEXPR DSAY (L) (APPLY-SAY L)) 

;;*page

;;;****************************************************************
;;;        functions for hand-tailored garbage collection
;;;****************************************************************

(DEFUN FORGET NIL 
       (SETQ LASTSENT NIL 
	     LASTREL NIL 
	     BACKREF NIL 
	     BACKREF2 NIL 
	     LASTTIME NIL 
	     LASTPLACE NIL)
       (SETQ LASTSENTNO 0.)
       (MAPC '(LAMBDA (PN) (MAPC '(LAMBDA (PROP) (REMPROP PN PROP))
				 '(BIND LASTBIND)))
	     '(IT THEY ONE))
       (AND EVENTLIST (PROGN (THFLUSH HISTORY) (STARTHISTORY)))) 

;;; THIS FUNCTION HAS ALSO INCLUDED A CALL TO "PLNRCLEAN"
;;; TO SCRUB AWAY THE EVENTLIST - BUT THE DETAILS OF ITS
;;; MICROPLANNER MANIPULATIONS ARE STILL BEING CHECKED FOR
;;; VERACTITY IN THE PRESENT DAY ENVIRONMENT (6/24/74)
;;; THE CODE WAS:
;;;  (DEFUN PLNRCLEAN (X)
;;;     (MAPC '(LAMBDA (Y)
;;;               (MAPC '(LAMBDA (Z)
;;;                         (THREMOVE (CAR Z)) )
;;;                     (CDDR Y)))
;;;           (GET X 'THASSERTION)) )
;;;
;;; AND THE CALL WAS:
;;;    (MAPC 'PLNRCLEAN EVENTLIST)
;;;

(DEFUN-FEXPR CLEANOUT (LIST) 					       ;REMOB'S ALL GENSYMS OF THE MEMBERS OF LIST
       (MAPC #'(LAMBDA (A) 
		       (CLEANX A 0. (GET A 'MAKESYM))
		       (SETF (GET A 'MAKESYM) 0.))
	     LIST)) 

(DEFUN-FEXPR CLEANUP (SYMBOL-LIST) 
       ;;CLEANUP IS USED TO GET RID OF GENSYMS NO LONGER NEEDED ALL
       ;;GENSYMS FROM THE NUMBER "OLD" TO THE NUMBER "NEW" ARE
       ;;REMOB'ED THE "OLD" AND "NEW" PROPERTIES ARE UPDATED
       (MAPC '(LAMBDA (SYMBOL) 
		      (CLEANX SYMBOL
			      (GET SYMBOL 'OLD)
			      (SETF (GET SYMBOL 'OLD)
				       (GET SYMBOL 'NEW)))
		      (SETF (GET SYMBOL 'NEW)
			       (GET SYMBOL 'MAKESYM)))
	     SYMBOL-LIST)) 

(DEFUN CLEANX (A B C) 
       ;; CLEANX REMOB'S GENSYMS OF THE SYMBOL "A" FROM B+1 UP TO AND
       ;;INCLUDING C
       (PROG (SAVE I) 
	     (SETQ B (OR B 0.))
	     (SETQ SAVE (GET A 'MAKESYM))
	     (AND C
		  (> C B)
		  (SETF (GET A 'MAKESYM) B)
		  (DO ((I B (+ 1 I))) ((= I C)) (UNINTERN (MAKESYM A))))
	     (RETURN (SETF (GET A 'MAKESYM) SAVE)))) 

;;*PAGE

;;;****************************************************************
;;;        a most complete and sophisticated break package
;;;****************************************************************

(DEFUN-FEXPR THERT (MESSAGE) (ERTEX MESSAGE NIL T)) 

(DEFUN-FEXPR ERT (MESSAGE) (ERTEX MESSAGE NIL T)) 		       ;ALWAYS STOPS, NEVER CAUSES ABORTION. USED FOR
								       ;GUARENTEED STOPS AS IN DEBUGGING OR ETAOIN

(DEFUN-FEXPR ERTERR (MESSAGE) (ERTEX MESSAGE T NIL)) 		       ;USED FOR KNOWN WIERD STATES SUCH AS CHOP. USES
								       ;"NOSTOP" SWITCH, CAUSES ABORTION

(DEFUN-FEXPR BUG (MESSAGE) 
       (ERTEX (CONS 'BUG!!!!!!!!!! MESSAGE) T NIL)) 		       ; MARKES UNANTICIPATED WIERD STATES WHICH
								       ;INDICATE MISTAKES IN THE CODE.

(DEFUN-FEXPR GLOBAL-ERR (MESSAGE) 
       (ERTEX (SETQ GLOBAL-MESSAGE MESSAGE) T NIL)) 		       ; MARKES KNOWN INADEQUACIES OF THE SYSTEM.
								       ;SWITCHABLE STOP, CAUSES ABORTION

(DEFUN ERTEX (MESSAGE CAUSE-ABORTION IGNORE-NOSTOP-SWITCH?) 
       (PROG (ERT-TIME GLOP EXP ST-BUFFER BUILDING-ST-FORM
	      FIRSTWORD) 
	     (AND NEVERSTOP (RETURN NIL))
	     (AND MOBYTEST-IN-PROGRESS (IOC W))
	     (AND NOSTOP
		  (NOT IGNORE-NOSTOP-SWITCH?)
		  (AND CAUSE-ABORTION
		       (THROW 'ABORT-PARSER CAUSE-ABORTION))
		  (RETURN T))
	     (SETQ ERT-TIME (GET-INTERNAL-RUN-TIME))
	     (TERPRI)
	     (MAPC #'PRINT3 MESSAGE)
	     (AND MOBYTEST-IN-PROGRESS
		  (THROW 'MOBYTEST ABORTPARSER))
	PRINT(SETQ FIRSTWORD T ST-BUFFER NIL BUILDING-ST-FORM NIL)     ;"ST" REFERS TO SHOW, TELL.
	     (COND (ZOG-USER (PRINT 'LISTENING--->))
		   (T (PRINT '>>>)))
	LISTEN
	     (COND
	      ;;SHELP UP SPURIOUS CHARACTERS
	      ((MEMBER (PEEK-CHAR) (LIST '#\Space))  ;SP, LF
	       (READ-CHAR)
	       (GO LISTEN))
	      ;;CHECK FOR DELIMITER
	      ((EQ (PEEK-CHAR) '#\Newline)			       ;CARRIAGE RETURN
	       (COND (BUILDING-ST-FORM (SETQ EXP		       ;DELIMITER CASE
					     (REVERSE ST-BUFFER))
				       (GO EVAL-EXP))
		     (T (READ-CHAR)				       ;SPURIOUS CHARACTER CASE
			(GO LISTEN)))))
	     ;;;
	     (OR (SETQ GLOP (READ)) (GO PRINT))
	     ;;;
	     (COND ((ATOM GLOP)
		    (SETQ GLOP (OR (GET GLOP 'ABBREV) GLOP))
		    (COND ((MEMQ GLOP '(T P NIL))		       ;LEAVE-LOOP CHARS
			   (SETQ ERT-TIME
				 (+ (TIME-SINCE ERT-TIME)
				       ERT-TIME))		       ;ERT-TIME IS BOUND BY SHRDLU
			   (RETURN GLOP))
			  ((EQ GLOP 'GO)			       ;CAUSE RETURN TO READY-STATE
			   (THROW 'ABORT-PARSER 'GO))
			  (BUILDING-ST-FORM (SETQ ST-BUFFER
						  (CONS GLOP
							ST-BUFFER))
					    (GO LISTEN))
			  ((AND FIRSTWORD
				(MEMQ GLOP '(SHOW TELL)))
			   (SETQ BUILDING-ST-FORM T 
				 ST-BUFFER (CONS GLOP ST-BUFFER) 
				 FIRSTWORD NIL)
			   (GO LISTEN))
			  (ZOGUSER (PRINC GLOP)
				   (SAY ISN\'T A COMMAND)
				   (TERPRI)
				   (GO PRINT))
			  (T (SETQ EXP GLOP) (GO EVAL-EXP))))
		   (T (COND ((EQ (CAR GLOP) 'RETURN)
			     (RETURN (EVAL (CADR GLOP))))
			    (T (SETQ EXP GLOP) (GO EVAL-EXP)))))
	     ;;;
	EVAL-EXP
	     (IGNORE-ERRORS (LIST (PRINT (EVAL EXP))))
	     (GO PRINT))) 

;;*PAGE


(DEFUN-FEXPR COMBINATION? (WORDS) 
       ;;THIS FUNCTION CHECKS TO SEE IF THE WORDS PASSED AS ARGS FORM
       ;;A COMBINATION SUCH AS "STACK-UP" OR "ON-TOP-OF" COMBINATIONS
       ;;ARE IN THE DICTIONARY AS A SINGLE ATOM COMPOSED OF THE WORDS
       ;;IN THE COMBINATION SEPARATED BY DASHES ALL COMBINATIONS HAVE
       ;;THE FEATURE "COMBINATION" AND HAVE A ROOT WHICH IS A LIST OF
       ;;THE WORDS IN THE COMBINATION
       (PROG (COMBINE) 
             (DECLARE (SPECIAL COMBINE))
	     (MAPC 
	      #'(LAMBDA (X) 
		(SETQ COMBINE (NCONC COMBINE
				     (CONS '-
					   (EXPLODE (EVAL X))))))
	      WORDS)
	     (SETQ COMBINE (LIST (INTERN (STRING (MAKNAM (CDR COMBINE))))))
	     (AND (ISQ COMBINE COMBINATION) (RETURN COMBINE))
	     (RETURN NIL))) 

(SETQ CONSO '(B C D F G H J K L M N P Q R S T V W X Z)) 

(DEFUN FINDB
	 (X Y) (COND ((NULL X) NIL)
			     ((EQ Y (CDR X)) X)
			     (T (FINDB (CDR X) Y))))

(DEFUN FROM
	 (A B) (COND ((OR (NOT A) (EQ A B)) NIL)
			     (T (CONS (WORD A) (FROM (CDR A) B)))))

(DEFUN MAKESYM (A) 
       ;; FUNCTION MAKESYM MAKES UP A GENSYM OF ITS ARG
       (SETF (GET A 'MAKESYM)
		(+ 1 (OR (GET A 'MAKESYM) 0.)))
       (SETQ A (MAKNAM (APPEND (OR (GET A 'EXPLO)
				   (SETF (GET A 'EXPLO)
					    (EXPLODE A)))
			       (EXPLODE (GET A 'MAKESYM)))))
       (COND (MAKEINTERN (INTERN A)) (A))) 

(DEFUN LIS2FY (X) 
       (COND ((ATOM X) (LIST (LIST X)))
	     ((ATOM (CAR X)) (LIST X))
	     (X))) 

(DEFUN MEET (A MEET) 
       ;; MEET RETURNS THE INTERSECTION OF 2 LISTS TREATED AS SETS
       (PROG (SET) 
	GO   (COND ((NULL A) (RETURN (REVERSE SET)))
		   ((MEMQ (CAR A) MEET)
		    (SETQ SET (CONS (CAR A) SET))))
	     (SETQ A (CDR A))
	     (GO GO))) 

(DEFUN SHRDLU-MOD (A B) (SHRDLU-UNION (SETDIF A (CADR B)) (CAR B)))

(DEFUN SHRDLU-NTH (NUM LIST) 
       (COND ((ATOM LIST) (ERT SHRDLU-NTH - ILLEGAL LIST))
	     ((< NUM 1.) (ERT SHRDLU-NTH - ILLEGAL NUMBER)))
       (PROG NIL 
	UP   (COND ((EQUAL NUM 1.) (RETURN (CAR LIST)))
		   ((SETQ LIST (CDR LIST))
		    (SETQ NUM (- NUM 1))
		    (GO UP))
		   (T (ERT SHRDLU-NTH - LIST TOO SHORT))))) 

(DEFUN PR1 (A) 
		 (COND ((ATOM (H A)) (LIST (WORD (NB A)) (FE A)))
		       ((PR2 (SM A))
			(LIST (FROM (NB A) (N A))
			      (FE A)
			      (SM A)
			      (COND ((ATOM (H A)) '\ )
				    ((MAPLIST #'PR1
					      (REVERSE (H A)))))))))

(DEFUN PR2 (A) 
  (OR
   (ATOM A)
   (MAPC 
    #'(LAMBDA (B) 
	      (AND (GET B 'SM)
		   (OR (MEMQ B ALIST)
		       (SETQ ALIST
			     (CONS (LIST B
					 (GET B 'SM)
					 (GET B
					      'REFER))
				   ALIST)))))
    A)))

(DEFUN PRINT2 (X) 
       (COND ((> CHRCT (FLATSIZE X)) (PRINC '\ ))
	     (T (TERPRI)))
       (PRINC X)) 

(DEFUN PRINT3 (X) 
       (PROG2 (OR (> CHRCT (FLATSIZE X)) (TERPRI))
	      (PRINC X)
	      (PRINC '\ ))) 

(DEFUN PRINTEXT (TEXT) 
       (COND (TEXT (TERPRI)
		   (EVAL (CONS 'SAY (LISTIFY TEXT)))))) 

(DEFUN-FEXPR PRINTC
	 (L) (PROG (TEST) 
			   (TERPRI)
		      =>   (COND ((NULL L) (RETURN NIL)))
			   (SETQ TEST (EVAL (CAR L)))
			   (COND ((EQ TEST '<TAB>))
				 (T (PRINC TEST) (PRINC '\ )))
			   (SETQ L (CDR L))
			   (GO =>)))

(DEFUN QUOTIFY (X) (LIST 'QUOTE X)) 

(DEFUN-FEXPR SAY (A) (MAPC #'PRINT3 A))

(DEFUN SETDIF (A SETDIF) 
       (PROG (SET) 
	GO   (COND ((NULL A) (RETURN (REVERSE SET)))
		   ((MEMQ (CAR A) SETDIF))
		   ((SETQ SET (CONS (CAR A) SET))))
	     (SETQ A (CDR A))
	     (GO GO))) 

(DEFUN STA
	 (A B) (PROG NIL 
			GO   (COND ((NULL B) (RETURN T))
				   ((NULL A))
				   ((EQ (CAR A) (CAR B))
				    (SETQ A (CDR A))
				    (SETQ B (CDR B))
				    (GO GO)))))

(DEFUN SHRDLU-UNION (A B) 
       (PROG (SET) 
	     (SETQ SET (REVERSE A))
	GO   (COND ((NULL B) (RETURN (REVERSE SET)))
		   ((MEMQ (CAR B) SET))
		   ((SETQ SET (CONS (CAR B) SET))))
	     (SETQ B (CDR B))
	     (GO GO))) 

(DEFUN WALLP
	 (A) (PROG (ALIST LINEL) 
			   (SETQ LINEL WPLINEL)
			   (AND (STATUS TTY) (TYO 12.))
			   (TERPRI)
			   (SPRINT (LIST (PR1 A) (REVERSE ALIST))
				   LINEL
				   0.)))

(SETQ WPLINEL 72.) 

(DEFUN-FEXPR DEFS (L) 
       (PROG (A) 
	     (AND (NULL (CDR L)) (RETURN L))
	     (SETQ A (CAR L))
	     (SETQ L (CDR L))
	LOOP 
	   (COND ((EQ (CAR L) 'EXPR)
	             (EVAL (APPEND (LIST 'DEFUN A (CADADR L)) (CDDADR L))))
		 ((EQ (CAR L) 'FEXPR)
		     (EVAL (APPEND (LIST 'DEFUN-FEXPR A (CADADR L)) (CDDADR L))))
	         (T (SETF (GET A (CAR L)) (CADR L))))
	   (COND ((SETQ L (CDDR L)) (GO LOOP)))
	   (RETURN A))) 

(DEFUN TAB
	 (N) (PROG NIL (RETURN '<TAB>)))

(DEFUN SHRDLU-SPACE (N) 
       (PROG (NN) 
	A    (COND ((> N 0.)
		    (PRINC '\ )
		    (SETQ N (- N 1))
		    (GO A))))) 

(DEFUN BE-SILENT NIL
	(SETQ LABELTRACE NIL)
	(SETQ PARSETRACE NIL)
	(SETQ PLANNERSEE NIL)
	(SETQ SH-STANDARD-PRINTOUT NIL)
	(SETQ ANNOYANCE T))
