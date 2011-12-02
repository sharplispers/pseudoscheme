; -*- Mode: Lisp; Syntax: Common-Lisp; Package: REVISED^4-SCHEME; -*-
; File rts.lisp / See file COPYING

;;;; Revised^4 Scheme run-time system

(in-package "REVISED^4-SCHEME") ;should already exist.

(defmacro defune (name bvl &body body)
  (let ((new-name
	 (intern (let ((string (symbol-name name)))
		   ;; Cf. perhaps-rename in p-utils.scm
		   (if (multiple-value-bind (sym status)
			   (find-symbol string (find-package "PS-LISP"))
			 (declare (ignore sym))
			 (eq status :external))
		       (concatenate 'string "." string)
		       string))
		 *package*)))
    `(progn #+LispM 'compile
	    (defun ,new-name ,bvl ,@body)
	    (ps:set-value-from-function ',new-name)
	    ',name)))

(when (symbolp (symbol-function 'null))	;Symbolics loses
  (setf (symbol-function 'null)
	(symbol-function (symbol-function 'null))))

; Definitions for CAR and CDR for when they are *not* open-coded.
; There really ought to be definitions for CDADDR and friends, but the
; programmer is too lazy to produce them.

(defune car (pair)
  (if (not (consp pair))
      (error "Argument to CAR isn't a pair -- ~S" pair)
      (car pair)))

(defune cdr (pair)
  (if (not (consp pair))
      (error "Argument to CDR isn't a pair -- ~S" pair)
      (cdr pair)))

; Non-open-coded standard Scheme procedures, in alphabetical order (almost)

; DYNAMIC-WIND, sort of.

(defune dynamic-wind (in body out)
  (funcall in)
  (unwind-protect (funcall body)
    (funcall out)))

; MAKE-PROMISE (auxiliary for DELAY macro)

(defstruct (promise (:print-function print-promise)
		    (:predicate promisep)
		    (:constructor make-promise (thunk-or-value)))
  (forced-yet-p nil)
  thunk-or-value)

(defun print-promise (obj stream escapep)
  (declare (ignore escapep))
  (if (promise-forced-yet-p obj)
      (format stream "#{Forced ~S}" (promise-thunk-or-value obj))
      (format stream "#{Delayed}")))

; FORCE

(defune force (obj)
  (cond ((promisep obj)
         (let ((tv (promise-thunk-or-value obj)))
           (cond ((promise-forced-yet-p obj) tv)
                 (t (let ((val (funcall tv)))
                      (setf (promise-thunk-or-value obj) val)
                      (setf (promise-forced-yet-p obj) t)
                      val)))))
        (t obj)))

; LIST?

(defune list? (l)			;New in R4RS
  (do ((l l (cddr l))
       (lag l (cdr lag)))
      ((not (consp l)) (ps:true? (null l)))
    (when (not (consp (cdr l)))
      (return (ps:true? (null (cdr l)))))
    (when (eq (cdr l) lag)
      (return ps:false))))

; LOAD -- forward reference to not-yet-existing EVAL module

#+DEC (proclaim '(function ps:scheme-load ps:scheme-eval))

(defune load (filespec &rest optional-args)
  (apply #'ps:scheme-load filespec optional-args))

(defune eval (form env)
  (ps:scheme-eval form env))

; MAKE-STRING

(defune make-string (size &optional (fill #\?))
  (cond (fill (make-string size :initial-element fill))
        (t (make-string size))))

; MAKE-VECTOR

(defune make-vector (size &optional (fill ps:unspecific))
  (make-sequence 'vector size :initial-element fill))

; NOT

(defune not (obj)
  (ps:true? (eq obj ps:false)))

; NUMBER->STRING

(defune number->string (num &optional (radix 10))
  (let ((*print-base* (if (equal radix '(scheme::heur))
			  10
			  radix)))
    (write-to-string num)))

; READ

(defune read (&optional (port *standard-input*))
  (funcall ps:*scheme-read* port))

; READ-CHAR

(defune read-char (&optional (port *standard-input*))
  (read-char port nil ps:eof-object))

(defune peek-char (&optional (port *standard-input*))
  (peek-char nil port nil ps:eof-object))

; STRING

(defune string (&rest chars)
  (coerce chars 'string))

; STRING->NUMBER

(defune string->number (string &optional (radix 10))
  (if (find-if #'(lambda (c) (digit-char-p c radix)) string)
      (with-input-from-string (s string)
	(let ((n (let ((*read-base* radix))
		   (read s nil ps:eof-object))))
	  (if (or (not (numberp n))
		  (not (eq (read s nil ps:eof-object)
			   ps:eof-object)))
	      ps:false
	      n)))
      ps:false))

; STRING-APPEND

(defune string-append (&rest strings)
  (apply #'concatenate 'simple-string strings))

; SYMBOL->STRING
;  The hair here is all to make printers written in Scheme produce
;  informative output, which wouldn't be the case if symbol->string were
;  the same as symbol-name.

(defune symbol->string (symbol)
  (let ((name (symbol-name symbol))
	(package (symbol-package symbol)))
    (cond ((eq package ps:scheme-package) name)
	  ((not (ps:scheme-symbol-p symbol))
	   (error "symbol->string: invalid argument - ~S"
		  symbol))
	  (t (multiple-value-bind (sym-again status)
		 (find-symbol name package)
	       (declare (ignore sym-again))
	       (let ((fakename
		      (concatenate 'string
				   (if (keywordp symbol)
				       ""
				       (package-name package))
				   (if (eq status :external)
				       ":"
				       "::")
				   name)))
		 (warn "returning ~s for (symbol->string '~s)"
		       fakename
		       symbol)
		 fakename))))))

; VECTOR?

(proclaim '(inline vector?))
(defune vector? (obj)
  (ps:true? (and (simple-vector-p obj)
		   ;; Structures are vectors in Symbolics, Exploder, and CLISP.
		   #+(or tops-20 Lispm)
		   (not (typep obj 'lisp::structure))
		   ;; Strings are simple vectors in CLISP (this is a bug)
		   #+tops-20
		   (not (stringp obj)))))

; WRITE
; Do a real printer some time.
; It seems sensible to respect *print-pretty*, in any case.

(defune write (obj &optional (port *standard-output*))
  (funcall ps:*scheme-write* obj port))

(defune display (obj &optional (port *standard-output*))
  (funcall ps:*scheme-display* obj port))

; CASE-AUX
;  Usually this should be open-coded, but sometimes it may not be.

(defune case-aux (key key-lists else-thunk &rest clause-thunks)
  (do ((ks key-lists (cdr ks))
       (ts clause-thunks (cdr ts)))
      ((null ks) (funcall else-thunk))
    (if (member key (car ks))
	(return (funcall (car ts))))))

; RATIONALIZE - implementation from IEEE Scheme standard

(defune rationalize (x e)
  (let ((e (abs e)))
    (simplest-rational (- x e) (+ x e))))

(defun simplest-rational (x y)
  (labels ((simplest-rational-internal
	    (x y)
	    (multiple-value-bind (fx x-fx)
		(floor x)
	      (multiple-value-bind (fy y-fy)
		  (floor y)
		(if (not (< fx x))
		    fx
		    (if (= fx fy)
			(+ fx
			   (/ 1
			      (simplest-rational-internal
			       (/ 1 y-fy)
			       (/ 1 x-fx))))
			(+ 1 fx)))))))
    (if (not (< x y))
	(if (rationalp x)
	    x
	    (error "(rationalize <irrational> 0) - ~S" x))
	(if (plusp x)
	    (simplest-rational-internal x y)
	    (if (minusp y)
		(- 0
		   (simplest-rational-internal (- 0 y)
					       (- 0 x)))
		0)))))

(defune interaction-environment ()
  (declare (special ps:*current-rep-environment*))
  ps:*current-rep-environment*)

(defune scheme-report-environment (n)
  (declare (special ps:scheme-report-environment))
  (case n
    ((4 5) ps:scheme-report-environment)
    (otherwise (error "invalid scheme report" n))))

(defune syntax-error (message &rest irritants)
  (apply #'ps:scheme-warn message irritants))


; Printer hooks

#+DEC
(progn
(system::define-list-print-function scheme::quote (list stream)
  (declare (list list))
  (if (two-element-list-p list)
      (format stream "'~W" (second list))
      (format stream "~1!~@{~W~^ ~:_~}~." list)))

(system::define-list-print-function scheme::quasiquote (list stream)
  (declare (list list))
  (if (two-element-list-p list)
      (format stream "`~W" (second list))
      (format stream "~1!~@{~W~^ ~:_~}~." list)))

(system::define-list-print-function scheme::unquote (list stream)
  (declare (list list))
  (if (two-element-list-p list)
      ;;+++ Should insert a space for , @FOO
      (format stream ",~W" (second list))
      (format stream "~1!~@{~W~^ ~:_~}~." list)))

(system::define-list-print-function scheme::unquote-splicing (list stream)
  (declare (list list))
  (if (two-element-list-p list)
      (format stream ",@~W" (second list))
      (format stream "~1!~@{~W~^ ~:_~}~." list)))

(defun two-element-list-p (obj)
  (and (consp obj) (consp (cdr obj)) (null (cddr obj))))
);ngorp

#+Symbolics
(progn 'compile
; This stuff seems to not work!
(zl:defprop scheme::quasiquote grind-quasiquote si:grind-macro)
(defun grind-quasiquote (e loc) loc
  (si:gtyo #.(zl:character (char-code #\`)))
  (si:grind-form (cadr e) (zl:locf (cadr e))))
(zl:defprop scheme::unquote grind-unquote si:grind-macro)
(defun grind-unquote (e loc) loc
  (si:gtyo #.(zl:character (char-code #\,)))
  (si:grind-form (cadr e) (zl:locf (cadr e))))
(zl:defprop scheme::unquote-splicing grind-unquote-splicing si:grind-macro)
(defun grind-unquote-splicing (e loc) loc
  (si:gtyo #.(zl:character (char-code #\,)))
  (si:gtyo #.(zl:character (char-code #\@)))
  (si:grind-form (cadr e) (zl:locf (cadr e))))
);ngorp
