; (declare (genprefix blockl))

;;;################################################################
;;;
;;;          BLOCKL - lisp code for the BLOCKS world
;;;
;;;################################################################



(DEFUN ABSVAL (X) (COND ((MINUSP X) (MINUS X)) (X)))

(DEFUN ATAB (X) (OR (ASSQ X ATABLE) (ERT ATABLE)))

(DEFUN CLEAR
       (LOC SIZE OBJ)
       (PROG (W X1 X2)
	     (SETQ OBJ (LISTIFY OBJ))
	     (AND (MEMQ NIL
			(MAPCAR #'(LAMBDA (X Y)
					       (AND (> X -1)
						    (> 1201 (+ X Y))
						    T))
				LOC
				SIZE))
		  (RETURN NIL))
	     (SETQ W ATABLE)
	GO   (COND ((NULL W) (RETURN LOC))
		   ((MEMQ (CAAR W) OBJ))
		   ((AND (< (CAR LOC) (+ (CAR (SETQ X1 (CADAR W)))
						(CAR (SETQ X2 (CADDAR W)))))
			 (< (CAR X1) (+ (CAR LOC) (CAR SIZE)))
			 (< (CADR LOC) (+ (CADR X1) (CADR X2)))
			 (< (CADR X1) (+ (CADR LOC) (CADR SIZE)))
			 (< (CADDR LOC) (+ (CADDR X1) (CADDR X2)))
			 (< (CADDR X1) (+ (CADDR LOC) (CADDR SIZE))))
		    (RETURN NIL)))
	     (SETQ W (CDR W))
	     (GO GO)))

(DEFUN DIFF (X Y) (MAPCAR #'- X Y))

(DEFUN DIV2 (X) (FLOOR (/ X 2)))

(DEFUN ENDTIME (LIST TIME) (PROG (Y)
				 (OR (SETQ Y (END? TIME)) (RETURN LIST))
			    UP	 (COND ((NULL LIST) (RETURN NIL))
				       ((NOT (> (CAAR LIST) Y))
					(RETURN LIST))
				       ((SETQ LIST (CDR LIST)) (GO UP)))))

(DEFUN EV NIL (OR NOMEM (THV EV)))
(DEFUN FINDSPACE
 (TYPE SURF SIZE OBJ)
 (PROG (XYMAX XYMIN N V X1 X2 FINDLEVEL)
       (SETQ OBJ (LISTIFY OBJ))
       (AND (MEMQ SURF OBJ) (RETURN NIL))
       (COND ((EQ SURF ':TABLE) (SETQ XYMIN '(0 0))
				       (SETQ XYMAX '(1200 1200))
				       (SETQ FINDLEVEL 0)
				       (GO ON))
	     ((SETQ X (ATAB SURF))))
       (COND
	((EQ TYPE 'CENTER)
	 (COND ((CLEAR (SETQ V
			     (LIST (MAX 0 (+ (CAADR X)
						(DIV2 (- (CAADDR X)
							 (CAR SIZE)))))
				   (MAX 0
					(+ (CADADR X)
					      (DIV2 (- (CADR (CADDR X))
						       (CADR SIZE)))))
				   (+ (CADDR (CADR X)) (CADDR (CADDR X)))))
		       SIZE
		       OBJ)
		(RETURN V))
	       ((RETURN NIL))))
	((EQ (CAR X) ':BOX)
	 (SETQ XYMIN (LIST (CAADR X) (CADADR X)))
	 (SETQ XYMAX (LIST (+ (CAADDR X) (CAADR X))
			   (+ (CADR (CADDR X)) (CADADR X))))
	 (SETQ FINDLEVEL 1))
	((SETQ X1 (DIV2 (CAR SIZE)))
	 (SETQ Y1 (DIV2 (CADR SIZE)))
	 (SETQ XYMAX
	       (LIST (MIN 1200 (- (+ (CAADDR X) (CAADR X) X1) 1))
		     (MIN 1200 (- (+ (CADR (CADDR X)) (CADADR X) Y1) 1))))
	 (SETQ XYMIN (LIST (MAX 0 (- (CAADR X) X1))
			   (MAX 0 (- (CADADR X) Y1))))
	 (SETQ FINDLEVEL (+ (CADDR (CADR X)) (CADDR (CADDR X))))))
  ON   (SETQ N 10)
       (SETQ X1 (- (CAR XYMAX) (CAR XYMIN)))
       (SETQ Y1 (- (CADR XYMAX) (CADR XYMIN)))
  GO   (COND ((ZEROP (SETQ N (- N 1))) (RETURN NIL))
	     ((OR (NOT (SETQ V
			     (GROW (LIST (+ (CAR XYMIN) (RANDOM X1))
					 (+ (CADR XYMIN) (RANDOM Y1))
					 FINDLEVEL)
				   XYMIN
				   XYMAX
				   OBJ)))
		  (< (- (CAADR V) (CAAR V)) (CAR SIZE))
		  (< (- (CADADR V) (CADAR V)) (CADR SIZE)))
	      (GO GO))
	     ((RETURN (COND ((EQ TYPE 'RANDOM)
			     (LIST (DIV2 (- (+ (CAAR V) (CAADR V))
						     (CAR SIZE)))
				   (DIV2 (- (+ (CADAR V) (CADADR V))
						     (CADR SIZE)))
				   FINDLEVEL))
			    ((EQ TYPE 'PACK)
			     (LIST (CAAR V) (CADAR V) FINDLEVEL))
			    ((ERT FINDSPACE /-- TYPE))))))))

(DEFUN-FEXPR GOAL
       (X)
       (SETQ PLAN NIL)
       (THVAL (LIST 'THGOAL (CAR X) '(THTBF THTRUE))
	      '((EV COMMAND)))
       (EVLIS (REVERSE PLAN)))

(DEFUN GROW
 (LOC MIN MAX OBJ)
 (PROG (GROW XL XH XO YL YH YO)
       (DECLARE (SPECIAL XL XH XO YL YH YO))
       (SETQ OBJ (LISTIFY OBJ))
       (COND
	((OR
	  (MINUSP (CAAR (SETQ XL (LIST (LIST (- (CAR LOC) (CAR MIN))
					     NIL)))))
	  (MINUSP (CAAR (SETQ XH (LIST (LIST (- (CAR MAX) (CAR LOC))
					     NIL)))))
	  (MINUSP (CAAR (SETQ YL (LIST (LIST (- (CADR LOC) (CADR MIN))
					     NIL)))))
	  (MINUSP (CAAR (SETQ YH (LIST (LIST (- (CADR MAX) (CADR LOC))
					     NIL)))))
	  (NULL
	   (CATCH 'GROW-DONE
	    (MAPC
	      #'(LAMBDA (X)
	       (PROG (XX YY)
		     (COND ((OR (MEMQ (CAR X) OBJ)
				(NOT (< (CAADR X) (CAR MAX)))
				(NOT (< (CADADR X) (CADR MAX)))
				(NOT (> (SETQ XX (+ (CAADR X)
							      (CAADDR X)))
					       (CAR MIN)))
				(NOT (> (SETQ YY (+ (CADADR X)
							      (CADR (CADDR X))))
					       (CADR MIN)))
				(NOT (> (+ (CADDR (CADR X))
						     (CADDR (CADDR X)))
					       (CADDR LOC))))
			    (RETURN NIL))
			   ((> (CAADR X) (CAR LOC))
			    (SETQ XH
				  (ORDER (LIST (- (CAADR X) (CAR LOC))
					       (CAR X))
					 XH)))
			   ((< XX (CAR LOC))
			    (SETQ XL (ORDER (LIST (- (CAR LOC) XX)
						  (CAR X))
					    XL)))
			   ((SETQ XO (CONS (CAR X) XO))))
		     (COND ((> (CADADR X) (CADR LOC))
			    (SETQ YH (ORDER (LIST (- (CADADR X)
							      (CADR LOC))
						  (CAR X))
					    YH)))
			   ((< YY (CADR LOC))
			    (SETQ YL (ORDER (LIST (- (CADR LOC) YY)
						  (CAR X))
					    YL)))
			   ((MEMQ (CAR X) XO) (THROW 'GROW-DONE NIL))
			   ((SETQ YO (CONS (CAR X) YO))))))
	     ATABLE))))
	 (RETURN NIL)))
  GO   (COND ((= (SETQ GROW (MIN (CAAR XL) (CAAR XH) (CAAR YL) (CAAR YH)))
		  2000)
	      (RETURN (LIST (LIST (- (CAR LOC) (CADAR XL))
				  (- (CADR LOC) (CADAR YL)))
			    (LIST (+ (CAR LOC) (CADAR XH))
				  (+ (CADR LOC) (CADAR YH))))))
	     ((MAPC #'(LAMBDA (Y Z W)
			      (PROG (X)
				    (SETQ X (EVAL W))
				    (COND ((> (CAAR X) GROW))
					  ((OR (NULL (CADAR X))
					       (MEMQ (CADAR X)
						     (EVAL Y)))
					   (RPLACA X (LIST 2000
							   (CAAR X))))
					  ((SET Z (CONS (CADAR X)
							(EVAL Z)))
					   (SET W (CDR X))))))
		    '(YO YO XO XO)
		    '(XO XO YO YO)
		    '(XL XH YL YH))
	      (GO GO)))))

(DEFUN LISTIFY (X) (COND ((ATOM X) (LIST X)) (X)))

;(declare (*expr fn))

(DEFUN LOCGREATER (X Y FN) ((LAMBDA (XX YY)
				    (NOT (< (FUNCALL FN (CADR XX))
						(+ (FUNCALL FN (CADR YY))
						      (FUNCALL FN (CADDR YY))))))
			    (LOCG2 '(THV YY) X)
			    (LOCG2 '(THV ZZ) Y)))



(DEFUN LOCG2 (X Y) (COND ((EQ (THV LOC) '\#LOC) (ATAB Y))
			 ((CONS NIL (CONS (EVAL X) (CDDR (ATAB Y)))))))

(DEFUN-FEXPR MEMOREND (A) (OR NOMEM (AND (SETF (GET (THV EV) 'END) THTIME)
					 (APPLY-THASSERT
						(LIST (THVARSUBST (CAR A) NIL )))
					 (SETF (GET (THV EV) 'TYPE) (CAAR A)))))

(DEFUN MEMORY NIL (OR NOMEM (THAND (THVSETQ (THNV EV) (MAKESYM 'E))
				   (THSETQ EVENTLIST (CONS (THV EV) EVENTLIST))
				   (SETF (GET (THV EV) 'START) THTIME)
				   (SETF (GET (THV EV) 'WHY) (THV WHY)))))

(DEFUN OCCUPIER
       (X Y Z)
       (PROG (W X1 X2)
	     (COND ((MINUSP Z) (RETURN ':TABLE)))
	     (SETQ W ATABLE)
	GO   (COND ((NULL W) (RETURN NIL))
		   ((AND (< (- (CAR (SETQ X1 (CADAR W))) 1)
				X
				(+ (CAR X1) (CAR (SETQ X2 (CADDAR W)))))
			 (< (- (CADR X1) 1) Y (+ (CADR X1) (CADR X2)))
			 (< (- (CADDR X1) 1) Z (+ (CADDR X1)
							  (CADDR X2))))
		    (RETURN (CAAR W))))
	     (SETQ W (CDR W))
	     (GO GO)))

(DEFUN ORDER (X Y) (COND ((NULL Y) (LIST X))
			 ((> (CAR X) (CAAR Y))
			  (CONS (CAR Y) (ORDER X (CDR Y))))
			 ((CONS X Y))))

(DEFUN PACKO
       (OBJ TYPE)
       (DECLARE (SPECIAL TYPE))
       (PROG (XX)
	     (MAPC #'(LAMBDA (X)
			     (AND (THVAL '(THGOAL (\#IS (THV X)
							     (THEV TYPE)))
					 (LIST (LIST 'X X)))
				  (SETQ XX (PACKORD X (SIZE X) XX))))
		   OBJ)
	     (RETURN (MAPCAR #'CADR XX))))

(DEFUN PACKON
       (SURF LIST)
       (PROG (X)
	     (SETQ SURF (ATAB SURF))
	GO   (COND ((NULL LIST) (RETURN NIL))
		   ((OR (> (CAR (SETQ X (SIZE (CAR LIST))))
				  (CAADDR SURF))
			(> (CADR X) (CADR (CADDR SURF)))
			(> (+ (CADDR X)
					(CADDR (CADR SURF))
					(CADDR (CADDR SURF)))
				  501))
		    (SETQ LIST (CDR LIST))
		    (GO GO))
		   ((RETURN (CAR LIST))))))

(DEFUN PACKORD
       (X SIZE LIST)
       (COND ((NULL LIST) (LIST (LIST SIZE X)))
	     ((OR (> (CAAAR LIST) (CAR SIZE))
		  (AND (EQ (CAR SIZE) (CAAAR LIST))
		       (> (CADAAR LIST) (CADR SIZE))))
	      (CONS (CAR LIST) (PACKORD X SIZE (CDR LIST))))
	     ((CONS (LIST SIZE X) LIST))))
(DEFUN SIZE (X) (COND ((EQ X ':BOX) '(400 400 300))
		      ((EQ X ':TABLE) '(1200 1200 1200))
		      ((EQ X ':HAND) '(0 0 0))
		      ((ATOM X) (CADDR (ATAB X)))
		      (X)))

(DEFUN STARTHISTORY
 NIL
 (SETQ THTIME 0)
 (SETQ GRASPLIST NIL)
 (SETF (GET 'EE 'WHY) 'COMMAND)
 (SETF (GET 'EE 'START) 0)
 (SETF (GET 'EE 'END) 0)
 (SETF (GET 'EE 'TYPE) '\#START)
 (SETQ EVENTLIST '(EE))
 (THADD '(\#START EE :DIALOG) NIL)
 (CLEANOUT E)
 (MAPC
  #'(LAMBDA (X)
    (AND (GET (CAR X) 'THASSERTION)
	 (SETF (GET (CAR X) 'HISTORY)
		  (LIST (LIST 0
			      (CADR X)
			      (CADAR (THVAL '(THGOAL (\#SUPPORT (THV X) (THV Y)))
					    (LIST (LIST 'X
							'THUNASSIGNED)
						  (LIST 'Y (CAR X))))))))))
  ATABLE)
)


(DEFUN STARTIME (LIST TIME) (< (CAAR LIST) (OR (START? TIME) -1)))

(DEFUN SUPPORT
       (LOC SIZE X)
       (COND ((EQ (CADDR LOC) 0) ':TABLE)
	     ((SETQ LOC (OCCUPIER (+ (CAR LOC) (DIV2 (CAR SIZE)))
				  (+ (CADR LOC) (DIV2 (CADR SIZE)))
				  (- (CADDR LOC) 1)))
	      (COND ((EQ LOC X) NIL) (LOC)))))

(DEFUN TCENT (X1 X2) (LIST (+ (CAR X1) (DIV2 (CAR X2)))
			   (+ (CADR X1) (DIV2 (CADR X2)))
			   (+ (CADDR X1) (CADDR X2))))

(DEFUN TFIND (X Y) (PROG (Z)
			 (OR (SETQ Z (GET X 'HISTORY)) (RETURN NIL))
		    UP	 (COND ((NOT (> (CAAR Z)
					       (OR (END? Y) 77777)))
				(RETURN Z))
			       ((SETQ Z (CDR Z)) (GO UP)))))

(DEFUN TIMECHK
       (EV TIME)
       (COND ((IMPERF? TIME)
	      (NOT (OR (< (GET EV 'END) (OR (START? TIME) -1))
		       (< (OR (END? TIME) 777777)
			      (GET EV 'START)))))
	     ((NOT (OR (< (GET EV 'START) (OR (START? TIME) -1))
		       (< (OR (END? TIME) 777777)
			      (GET EV 'END)))))))

