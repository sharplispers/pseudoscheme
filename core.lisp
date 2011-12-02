; -*- Mode: Lisp; Syntax: Common-Lisp; Package: PS; -*-
; File core.lisp / See file COPYING

;;;; Pseudoscheme run-time system

(in-package "PS")

; The Scheme booleans
;   - must be self-evaluating
;   - must have invertible read/print syntax
;   - must be uniquely created
;   - can't be symbols without slowing down Scheme's SYMBOL? predicate
;   - similarly for numbers, pairs, etc.
; What values are self-evaluating Common Lisp objects with a read/print
; syntax that aren't used for anything in Scheme?  ...
; There aren't any.
; So, we use symbols, and slow down the SYMBOL? predicate.

(defparameter false 'false)  ;You can set this to 'nil if you want
(defparameter true  't)

(proclaim '(inline truep true? scheme-symbol-p))

; Convert Scheme boolean to Lisp boolean.
;  E.g. (lisp:if (truep foo) ...)

(defun truep (scheme-test)
  (not (eq scheme-test false)))

; Convert Lisp boolean to Scheme boolean.
;  E.g. (cons (true? (lisp:numberp x)) ...)
; This assumes that the argument is never the empty list.

(defun true? (cl-test) (or cl-test false))

(defun scheme-symbol-p (x)
  (declare (optimize (safety 0)))	;compilers are stupid
  (and (symbolp x) (not (eq (car (symbol-plist x)) 'not-a-symbol))))

(setf (get true  'not-a-symbol) t)
(setf (get false 'not-a-symbol) t)
(setf (get nil   'not-a-symbol) t)	;used for Scheme's empty list

;

(defparameter scheme-package (find-package "SCHEME"))

#+Symbolics
(pushnew scheme-package si:*reasonable-packages*)


; ----- Photons

; "A `photon' is an object that PRIN1's as if it had been PRINC'ed."
; 					  -- KMP

(defstruct (photon (:constructor make-photon (string-or-function))
		   (:copier nil)
		   (:print-function print-photon))
  string-or-function)

(defun print-photon (photon stream escape?)
  (declare (ignore escape?))
  (let ((z (photon-string-or-function photon)))
    (if (stringp z)
	(princ z stream)
	(funcall z stream))))

; Miscellaneous objects

(defvar unspecific (make-photon "#{Unspecific}"))
(defvar unassigned  (make-photon "#{Unassigned}"))
(defvar eof-object  (make-photon "#{End-of-file}"))

; PROCEDURE?

(defparameter closures-might-be-conses-p
  #+Lucid nil  ;suppress message about compiler optimizations
  #-Lucid
  (or (consp (eval '#'(lambda (x) x)))	;VAX LISP 2.1
      (consp (let ((g (gensym)))
	       (eval `(progn (defun ,g () 0) #',g)))) ;Symbolics
      (consp (compile nil '(lambda (x) x))) ;just for kicks
      (consp (funcall (compile nil '(lambda (x) ;VAX LISP 2.2
				      #'(lambda () (prog1 x (incf x)))))
		      0))))

(defun procedurep (obj)
  (and (functionp obj)
       (not (symbolp obj))
       (or (not (consp obj))
	   closures-might-be-conses-p)))

; Mumble

(proclaim '(inline booleanp char-whitespace-p output-port-p))

(defun booleanp (obj)
  (or (eq obj true)
      (eq obj false)))

(defun char-whitespace-p (char)
  (or (char= char #\space)
      (not (graphic-char-p char))))

(defun input-port-p (obj)
  (and (streamp obj)
       (input-stream-p obj)
       t))

(defun output-port-p (obj)
  (and (streamp obj)
       (output-stream-p obj)
       t))

;This function is new in CLtL II / ANSI.
#-ansi-cl
(defun realp (obj)
  (and (numberp obj)
       (not (complexp obj))))

; Auxiliary for SET!

(defun set!-aux (name value CL-sym)
  (case (get CL-sym 'defined)
    ((:assignable))
    ((:not-assignable)
     (cerror "Assign it anyhow"
	     "Variable ~S isn't supposed to be SET!"
	     (or name CL-sym)))
    ((nil)
     (warn "SET! of undefined variable ~S" (or name CL-sym))))
  (setf (symbol-value CL-sym) value)
  (if (procedurep value)
      (setf (symbol-function CL-sym) value)
      (fmakunbound CL-sym))
  unspecific)

; Auxiliary for lambda-expression-containing top-level forms on Symbolics

(defmacro at-top-level (&rest forms)
  #+LISPM
  (let ((g (gentemp "[TOP]")));;!?!?
    `(progn (defun ,g () ,@forms)
	    (prog1 (,g)
	      (fmakunbound ',g))))
  #-LISPM
  `(progn ,@forms))

; Auxiliary for copying &rest variables on Symbolics

(defmacro maybe-fix-&rest-parameter (rest-var)
  #+LISPM
  `(setq ,rest-var (copy-list ,rest-var))
  #-LISPM
  (progn rest-var ;ignored
	 `nil))

(defvar *scheme-read*)
(defvar *scheme-write*)
(defvar *scheme-display*)

(defvar *define-syntax!*
  #'(lambda (name+exp) (declare (ignore name+exp)) 'define-syntax))

(defmacro %define-syntax! (name+exp)
  `(eval-when (load)
     (funcall *define-syntax!* ,name+exp)))


; These also appear in loadit.lisp
(defun filename-preferred-case (name)
  #+unix (string-downcase name)
  #-unix (string-upcase name)
  )
(defvar *translated-file-type* (filename-preferred-case "pso"))

; Prelude on all translated files

(defmacro begin-translated-file ()
  `(progn (eval-when (eval compile load)
	    (setq *readtable* cl-readtable))
	  (check-target-package)))

(defparameter cl-readtable (copy-readtable nil))

(defvar *target-package* nil)

(defun check-target-package ()
  (when (and *target-package*
	     (not (eq *target-package* *package*)))
    (warn "Translate-time package ~A differs from attempted load-time package ~A"
	  (package-name *package*)
	  (package-name *target-package*))))

; Auxiliaries for top-level DEFINE

(defun set-value-from-function (CL-sym &optional name) ;Follows a DEFUN
  (setf (symbol-value CL-sym) (symbol-function CL-sym))
  (after-define CL-sym name))

(defun really-set-function (CL-sym value)
  (cond ((procedurep value)
	 #+Lucid
	 (lcl:define-function CL-sym value)
	 #-Lucid
	 (setf (symbol-function CL-sym) value))
	(t
	 (fmakunbound CL-sym))))

(defun set-function-from-value (CL-sym &optional name) ;Follows a SETQ
  (let ((value (symbol-value CL-sym)))
    (really-set-function CL-sym value)
    #+Symbolics
    (scl:record-source-file-name CL-sym (if (procedurep value) 'defun 'defvar))
    (after-define CL-sym name)))

; Follows (SETQ *FOO* ...)

(defun set-forwarding-function (CL-sym &optional name)
  (setf (symbol-function CL-sym)
	#'(lambda (&rest args)
	    (apply (symbol-value CL-sym) args)))
  (after-define CL-sym name))

(defun after-define (CL-sym name)
  (setf (get CL-sym 'defined) t)
  (when name
    (make-photon #'(lambda (port)
		     (let ((*package* scheme-package))
		       (format port "~S defined." name))))))

; EQUAL?

; Differs from Common Lisp EQUAL in that it descends into vectors.
; This is here instead of in rts.lisp because it's an auxiliary for
; open-coding MEMBER and ASSOC, and the rule is that all auxiliaries
; are in the PS package (not REVISED^4-SCHEME).

(defun scheme-equal-p (obj1 obj2)
  (cond ((eql obj1 obj2) t)
        ((consp obj1)			;pair?
         (and (consp obj2)
	      (scheme-equal-p (car obj1) (car obj2))
	      (scheme-equal-p (cdr obj1) (cdr obj2))))
	((simple-string-p obj1)		;string?
	 (and (simple-string-p obj2)
	      (string= (the simple-string obj1)
		       (the simple-string obj2))))
	((simple-vector-p obj1)
	 (and (simple-vector-p obj2)
	      (let ((z (length (the simple-vector obj1))))
		(declare (fixnum z))
		(and (= z (length (the simple-vector obj2)))
		     (do ((i 0 (+ i 1)))
			 ((= i z) t)
		       (declare (fixnum i))
		       (when (not (scheme-equal-p
				   (aref (the simple-vector obj1) i)
				   (aref (the simple-vector obj2) i)))
			 (return nil)))))))
        (t nil)))

; Handy things.

; ERROR, WARN, SYNTAX-ERROR (nonstandard)

(defun scheme-error (message &rest irritants)
  (signal-scheme-condition #'error message irritants))

(defun scheme-warn (message &rest irritants)
  (signal-scheme-condition #'warn message irritants))

(defun signal-scheme-condition (fun message irritants)
  (if (or (not (stringp message))
	  (find #\~ message))
      (apply fun message irritants)
      (apply fun
	     (apply #'concatenate
		    'string
		    (if (stringp message) "~a" "~s")
		    (mapcar #'(lambda (irritant)
				(declare (ignore irritant))
				"~%  ~s")
			    irritants))
	     message
	     irritants)))

#+LispM
(setf (get 'scheme-error :error-reporter) t)  ;Thanks to KMP

; PP (nonstandard)

(defun pp (obj &optional (port *standard-input*))
  (let ((*print-pretty* t)
	(*print-length* nil)
	(*print-level* nil))
    (format port "~&")
    (print obj port)
    (values)))
