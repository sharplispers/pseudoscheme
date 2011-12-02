; -*- Mode: Lisp; Syntax: Common-Lisp; Package: PS; -*-
; File eval.lisp / See file COPYING

;;;; Pseudoscheme run-time system

(in-package "PS")


;; User environment

(defvar scheme-user-environment
  (scheme-translator:make-scheme-user-environment 'scheme::scheme))

(defvar *current-rep-environment* scheme-user-environment)

(defvar *target-environment* *current-rep-environment*)

(defvar scheme-report-environment
  (locally (declare (special scheme-translator:revised^4-scheme-structure))
    (scheme-translator:make-program-env
       'scheme::scheme-report-environment-4
       (list scheme-translator:revised^4-scheme-structure))))

; EVAL itself

(defun scheme-eval (form env)
  (eval (scheme-translator:translate form env)))
      

; Hack to get define-syntaxes communicated from .bin files to system
; in which .bin file is loaded.  Calls to *define-syntax!* necessarily
; occur inside of (eval-when (load) ...)'s, in which case, if the .bin
; file is being loaded by scheme-load, *target-environment* will be
; the correct environment.

(setq *define-syntax!*
      #'(lambda (name+exp)
	  (scheme-translator::process-define-syntax (cons 0 name+exp)
						    *target-environment*)))


; COMPILE -- compile a single procedure
;  (compile symbol)  is like (set! symbol (compile lambda-expression))

(defun scheme-compile (name-or-source)
  (let ((env *current-rep-environment*))
    (cond ((symbolp name-or-source)
	   (let ((CL-sym (scheme-translator:intern-renaming-perhaps
			   (symbol-name name-or-source)
			   (scheme-translator:program-env-package env))))
	     (compile CL-sym)
	     (set-value-from-function CL-sym)))
	  (t
	   (compile nil (scheme-translator:translate-lambda
			     name-or-source env))))))

; "Roadblock" readtable.  Behaves exactly like a regular Common Lisp
; read table, except when the SCHEME package (or a package associated
; with the current Scheme environment) is current, in which case it reads
; a form using the Scheme readtable and package, then wraps (BEGIN
; ...) around it so that the translator will kick in and translate the
; form.

(defparameter roadblock-readtable (copy-readtable nil))

#+Symbolics
(pushnew roadblock-readtable si:*valid-readtables*)

(defun roadblock-read-macro (stream ch)
  (unread-char ch stream)
  (if (or (eq *package* scheme-package)
	  (eq *package* (scheme-translator:program-env-package
			     *target-environment*))
	  (eq *package* (scheme-translator:program-env-package
			     *current-rep-environment*)))
      (let ((form (funcall *scheme-read* stream)))
	(if (eq form eof-object)
	    (values)
	    `(scheme-form ,form)))
      (let ((*readtable* *non-scheme-readtable*))
	(read stream nil 0 t))))

(let ((*readtable* roadblock-readtable))
  (mapc #'(lambda (s)
	    (map nil
		 #'(lambda (c)
		     (set-macro-character c #'roadblock-read-macro nil))
		 s))
	;; Intentionally absent: right parenthesis, semicolon, whitespace
	'(
	  ;; Non-constituents
	  "\"#'(,`"
	  ;; Constituents (more or less)
	  ;;
	  ;; Actually we don't want to hack these, since otherwise the
	  ;; printer (which we can't hook, in general) will be
	  ;; printing all symbols as |FOO|.  This will only matter for
	  ;; symbol evaluation at an unhooked REP or debugging loop,
	  ;; where evaluation is supposed to be in some environment
	  ;; other than that initial one.
	  ;;
	  ;; On the other hand, if in some implementation we CAN
	  ;; reliably hook the printer, or else sufficiently restrict
	  ;; the use of the roadblock readtable (e.g. by passing it
	  ;; explicitly to LOAD and COMPILE-FILE), then we SHOULD
	  ;; block the constituent characters.  Thus I have left them
	  ;; here in this comment.
	  ;;
	  ;; "!$%&*+-./0123456789:<=>?"
	  ;; "@ABCDEFGHIJKLMNOPQRSTUVWXYZ[\\]^_"
	  ;; "abcdefghijklmnopqrstuvwxyz{|}~"
	  )))

(defmacro scheme-form (&whole whole form)
  (let* ((new-form (translate-scheme-form form))
	 (new-form (if (consp new-form) new-form `(progn ,new-form))))
    ;; The following tries to compensate for some versions of LOAD and
    ;; COMPILE-FILE that imagine that macroexpansion is cheap.
    (setf (car whole) (car new-form))
    (setf (cdr whole) (cdr new-form))
    new-form))

; Use ROADBLOCK-EVAL to evaluate a form that was known to have been
; read by the roadblock readtable.

(defun roadblock-eval (form environment)
  (cond ((and (consp form) (eq (car form) 'scheme-form))
	 (scheme-eval (cadr form) environment))
	((symbolp form)
	 (scheme-eval form environment))
	(t
	 (eval form))))

(defvar *scheme-file-type* (filename-preferred-case "scm"))

(defmacro without-requiring-in-package (&body body)
  #-Lucid
  `(progn ,@body)
  #+Lucid
  `(lcl:handler-bind ((lcl:simple-warning
		       #'(lambda (c)
			   (when (search "does not begin with IN-PACKAGE"
					(lcl:simple-condition-format-string c))
			     (lcl:invoke-restart 'lcl:muffle-warning)))))
		     ,@body))

; LOAD

(defun scheme-load (filespec &optional env
			     &rest keys)
  (when (keywordp env) (push env keys) (setq env nil))
  (using-environment env
    #'(lambda (env)
	(without-requiring-in-package
	 (apply #'clever-load:clever-load filespec
		:source-type (or (getf keys :source-type)
				 *scheme-file-type*)
		:message (format nil "into ~s environment"
				 (scheme-translator:program-env-id env))
		#+LispM :package
		#+LispM (scheme-translator:program-env-package env)
		keys)))))

; COMPILE-FILE

(defun scheme-compile-file (filespec &optional env
				     &rest keys)
  (when (keywordp env) (push env keys) (setq env nil))
  (using-environment env
    #'(lambda (env)
	(let ((path
	       (merge-pathnames filespec
				(make-pathname :type *scheme-file-type*))))
	  (format t "~&Compiling ~A using ~S environment~%" (namestring path)
		  (scheme-translator:program-env-id env))
	  (without-requiring-in-package
	   (apply #'compile-file
		  path
		  #+LispM :package #+LispM (scheme-translator:program-env-package env)
		  keys))))))

; using-environment: auxiliary for LOAD and COMPILE-FILE.
;  - *readtable* is bound to roadblock-readtable so that the translator
;    will kick in.  Top-level forms (foo) read as (scheme-form (foo))).
;  - *target-environment* is bound in order to communicate the appropriate
;    environment to the translator.
;  - *target-package* is bound in case we're loading a file of translated
;    code (extension .pso) produced by translate-file.  In this case,
;    we check to make sure that *target-environment* agrees with the
;    IN-PACKAGE form in the file.  (?)

(defun using-environment (env fun)
  (let ((env (or env *target-environment*)))
    (let ((*package* scheme-package)
	  (*readtable* roadblock-readtable)
	  (*target-environment* env)
	  (*target-package* (scheme-translator:program-env-package env))
	  (*scheme-read* *scheme-read*))  ;Allow (setq *scheme-read* ...)
      (funcall fun env))))
	

; TRANSLATE-FILE

(defun translate-file (filespec &optional (env *target-environment*))
  (let ((path (merge-pathnames (if (symbolp filespec)
				   (symbol-name filespec)
				   filespec))))
    (scheme-translator:really-translate-file
	     (if (member (pathname-type path) '(nil :unspecific))
		 (make-pathname :type *scheme-file-type*
				:defaults path)
		 path)
	     (make-pathname :type *translated-file-type*
			    :defaults path)
	     env)))

; Auxiliary routine called when reading from Gnu Emacs using LEDIT package

(defun ledit-eval (filename form)
  (declare (ignore filename))		;for now
  (if (eq *package* scheme-package)
      (scheme-eval form *current-rep-environment*)
      (eval form)))

(locally (declare (special user::*ledit-eval*))
  (setq user::*ledit-eval* #'ledit-eval))

;

(defun set-rep-environment! (env)
  (setq *current-rep-environment* env)
  (setq *target-environment* env)
  (setq *target-package* (scheme-translator:program-env-package env))
  (values))

; Set up "trampolines" to allow evaluation of Scheme forms directly by
; the Common Lisp evaluator.  Alsp, give some help to the pretty-printer
; by way of indicating where &bodies are.

(defun translate-scheme-form (form)
  (scheme-translator:translate form *target-environment*))

(defmacro scheme::case (key &body clauses)
  (translate-scheme-form `(scheme::case ,key ,@clauses)))

(defmacro scheme::define (pat &body body)
  (translate-scheme-form `(scheme::define ,pat ,@body)))

(defmacro scheme::define-syntax (pat &body body)
  (translate-scheme-form `(scheme::define-syntax ,pat ,@body)))

(defmacro scheme::do (specs end &body body)
  (translate-scheme-form `(scheme::do ,specs ,end ,@body)))

(defmacro scheme::lambda (bvl &body body)
  (translate-scheme-form `(scheme::lambda ,bvl ,@body)))

(defmacro scheme::let (specs &body body)
  (translate-scheme-form `(scheme::let ,specs ,@body)))

(defmacro scheme::let* (specs &body body)
  (translate-scheme-form `(scheme::let* ,specs ,@body)))

(defmacro scheme::letrec (specs &body body)
  (translate-scheme-form `(scheme::letrec ,specs ,@body)))

; Other trampolines...

(defmacro translate-me (&whole form &rest rest)
  (declare (ignore rest))
  (translate-scheme-form form))

(mapc #'(lambda (scheme-sym)
	  ;; Allow (LISP:EVAL '(SCHEME::AND ...))
	  (setf (macro-function scheme-sym)
		(macro-function 'translate-me)))
      '(scheme::and
	scheme::begin
	scheme::cond
	scheme::delay
	scheme::cons-stream
	scheme::if
	scheme::or
	scheme::quasiquote
	scheme::quote
	scheme::set!))

; Read-eval-print loop

(defvar *rep-state-vars* '())

(defun enter-scheme (&key (verbose t))
  (declare (special scheme-translator:translator-version))
  (set-scheme-value '*package* scheme-package)
  (set-scheme-value '*print-array* t)	     ;for #(...)
  (set-scheme-value '*print-case* :downcase)
  (set-scheme-value '*readtable* roadblock-readtable)
  (setq *non-scheme-readtable*
	(get '*readtable* 'non-scheme-value))
  (when verbose
    (format t "~&This is ~A.~%" (scheme-translator:translator-version)))
  (values))

(defun exit-scheme (&key (verbose t))
  (when verbose
    (format t "~&Leaving Pseudoscheme.~&"))
  (mapc #'(lambda (var)
	    (let ((probe (get var 'non-scheme-value 'no-such-property)))
	      (unless (eq probe 'no-such-property)
		(set-standard-value var probe))))
	*rep-state-vars*)
  (values))

(defun set-scheme-value (var value)
  (pushnew var *rep-state-vars*)
  (let ((old-value (symbol-value var)))
    (unless (eq value old-value)
      (setf (get var 'non-scheme-value) old-value))
    (set-standard-value var value)))

(defun set-standard-value (var value)
  #-Symbolics
  (setf (symbol-value var) value)
  #+Symbolics
  (if (member var '(*package* *readtable* *print-array* *print-case*))
      (setf (sys:standard-value var :setq-p t)
	    value)
      (setf (symbol-value var) value)))

;;; EVAL and PRINT functions to be used by the REP loop:

(defun scheme-rep-eval (exp)
  (roadblock-eval exp *current-rep-environment*))

(defvar *result-display-style* :normalize)  ;or :eval

(defun write-result (result &optional (stream *standard-output*))
  (if (and (eq *result-display-style* :normalize)
	   (not (or (eq result ps:true)  ;self-evaluating-p
		    (eq result ps:false)
		    (numberp result)
		    (characterp result)
		    (stringp result)
		    (photon-p result))))
      (write-char #\' stream))
  (funcall *scheme-write* result stream))

; (SCHEME) and (QUIT) are system-specific REP loop entry and exit
; routines.

(defvar *quitter* 'exit-scheme)
(defvar *schemer* 'enter-scheme)

(defun quit   () (funcall *quitter*))
(defun scheme () (funcall *schemer*))

#+LispWorks (progn

(defun scheme-repl-for-lw ()
  (when (find-restart 'quit-scheme-repl)
    (error "The LW Scheme REPL is not re-entrant."))
  (unless *rep-state-vars*
    (let ((*standard-output* (make-broadcast-stream)))
      ;; This is done without typeout since it's a mind's eye experiment.
      ;; It is for effect, so we can find out what variables to bind.
      ;; Kind of a kludge, I suppose...
      (enter-scheme :verbose nil)
      (exit-scheme :verbose nil)))
  (cl:progv *rep-state-vars* (mapcar #'symbol-value *rep-state-vars*)
    (enter-scheme)
    (unwind-protect (with-simple-restart (quit-scheme-repl "Quit Scheme")
                      (let ((*quitter* #'(lambda ()
                                           (invoke-restart 'quit-scheme-repl))))
                        (system::listener-top-loop ; Seems to work better than system::%top-level
                         ;; You could specify :PROMPT here (see LW:*PROMPT*) but the default is good.
                         :eval-print-hook
                         #'(lambda (values) 
                             (let ((*print-case* :downcase))
                               (loop for (result . more) on values
                                     do (write-result result)
                                     when more
                                     do (format t " ;~%"))
                               ;; Already printed, so have system print no values.
                               (values))))))
      (exit-scheme))))

(setq *schemer* 'scheme-repl-for-lw)

)

#-(or :DEC Symbolics) (progn

;; Nothing to do.
;; Generic definitions of QUIT and SCHEME suffice

) ;end (progn ...)

#+:DEC (progn

(defun scheme-repl-for-dec ()
  (unwind-protect
      (progn
	(enter-scheme)
        (let ((*quitter* 'vax-lisp:continue))
	  (system::read-eval-print-loop
	   "Scheme> "
	   :eval 'scheme-rep-eval
	   :print #'(lambda (vals stream)
		      (format stream "~&")
		      (do ((v vals (cdr v)))
			  ((null v) (values))
			(write-result (car v) stream)
			(if (not (null (cdr v)))
			    (format stream " ;~%"))))))
	(values))
    (exit-scheme)))

(setq *schemer* 'scheme-repl-for-dec)

) ;end #+:DEC (progn ...)

#+Symbolics (progn 'compile

(defun enter-scheme-for-symbolics ()
  "Initialize for execution of Scheme programs."
  (enter-scheme)
  (set-scheme-value 'si:*command-loop-eval-function*
		    'scheme-rep-eval)
  (set-scheme-value 'si:*command-loop-print-function*
		    #'(lambda (values)
			(mapc #'(lambda (value)
				  (zl:send zl:standard-output :fresh-line)
				  (write-result value))	;?
			      values)))
  (values))

(setq *schemer* 'enter-scheme-for-symbolics)

) ;end #+Symbolics (progn ...)

; Integrate built-ins in user environment

(defun benchmark-mode ()
  (scheme-translator:perform-usual-integrations! scheme-user-environment)
  (values))

; Mumble

(flet ((set-in-user-env (name val)
         (setf (symbol-value name) val)
	 (ps:set-function-from-value name)))
  (set-in-user-env 'scheme::quit  #'quit)
  (set-in-user-env 'scheme::compile        #'scheme-compile)
  (set-in-user-env 'scheme::compile-file   #'scheme-compile-file)
  (set-in-user-env 'scheme::translate-file #'translate-file)
  (set-in-user-env 'scheme::pp		   #'pp)
  (set-in-user-env 'scheme::error	   #'scheme-error)
  (set-in-user-env 'scheme::benchmark-mode #'benchmark-mode))

