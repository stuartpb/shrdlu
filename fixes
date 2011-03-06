; The fix for MACLISP's DEFPROP, written by Kent Pitman
; first form, for new lambda's defined
 (DEFMACRO DEFPROPFN ((SYM PROP) BVL &BODY FORMS)
   `(SETF (GET ',SYM ',PROP) #'(LAMBDA ,BVL ,@FORMS)))

; Example:  To define the leg property of bob:
; (DEFPROPFN (BOB LEG) () (PRINT 'bobsleg))
; In maclisp this would've been:
; (DEFPROP BOB (LAMBDA () (PRINT 'bobsleg)) LEG)

; The second fix for DEFPROP, based on the first,
; for the form where a property is defined as some other 
; previously-defined function which takes no args.
 (DEFMACRO DEFPROPNIL (SYM NILFUN PROP)
   `(SETF (GET ',SYM ',PROP) #'(LAMBDA NIL (,NILFUN))))
; from: (DEFPROP THPROG THPROGT THFAIL)
; to:   (DEFPROPNIL THPROG THPROGT THFAIL)


; Here's the improved (working) way to do the defun-fexpr
 (defmacro defun-fexpr (fexpr-name (fexpr-arg) &body fexpr-body)
   (let ((subr-name (intern (format nil "APPLY-~A" fexpr-name))))
      `(progn (defmacro ,fexpr-name (&rest ,fexpr-arg)
                `(,',subr-name ',,fexpr-arg))
      	      (defun ,subr-name (,fexpr-arg) ,@fexpr-body)
              ',fexpr-name)))

; With the following rewrites:
; (defun bob fexpr (x) ...) as (defun-fexpr bob (x) ...)
; (apply 'bob stuff)        as (apply-bob stuff);



;**mlf**
; this is a fix for ASCII from Pitman's notes...
; ASCII is only used in show, setup, and morpho...

(DEFUN ASCII (N)
	(COND ((NUMBERP N) (ASCII (CODE-CHAR N)))
		(T (INTERN (STRING N)))))

(DEFUN READLIST (X)
  (READ-FROM-STRING (APPLY #'CONCATENATE (CONS 'STRING (MAPCAR #'STRING X)))))
; direct inverse of explode
; adjusted for ANSI Common Lisp by jrs
 
(DEFUN EXPLODE (OBJ)
  (MAP 'LIST #'(LAMBDA (X) (INTERN (STRING X))) (FORMAT NIL "~S" OBJ)))
; direct inverse of readlist

(DEFUN FLATSIZE (X)
  (LENGTH (EXPLODE X)))
;this will return the number of characters in the argument if it 
;was printed out with special characters slashified


(DEFUN MEMQ (OBJECT LIST) (MEMBER OBJECT LIST :TEST #'EQUAL))

(DEFUN ASSQ (OBJECT LIST) (ASSOC OBJECT LIST :TEST #'EQUAL))

(DEFUN MAKNAM (CH-LIST)
   (MAKE-SYMBOL (FORMAT NIL "~{~A~}" CH-LIST)))
