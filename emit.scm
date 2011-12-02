; -*- Mode: Scheme; Syntax: Scheme; Package: Scheme; -*-
; File emit.scm / See file COPYING

;;;; Common Lisp code emission utilities

; This is intimately tied up with the GENERATE module, but is
; separated for the purpose of producing alternate implementations of
; GENERATE with different internal calling conventions.  Thus GENERATE
; may know a lot about this module, but not vice versa.


; If @TARGET-PACKAGE is #f, leave unqualified program (top-level)
; variables in the SCHEME package.  Otherwise, intern them in the
; target package.

(define @target-package (make-fluid #f))


; @TRANSLATING-TO-FILE? This controls a number of inconsequential code
; generation decisions, e.g. whether the (IF #F X) should return
; unspecific and whether local variables should be turned into
; symbols in the target package.

(define @translating-to-file? (make-fluid #f))


; Program variable management:

(define (emit-program-variable-set! var CL-sym rhs-code)
  (cond ((mutable-program-variable? var)
	 `(ps-lisp:setq ,CL-sym ,rhs-code))
	(else
	 `(ps:set!-aux
	   (ps-lisp:quote ,(program-variable-name var))
	   ,rhs-code
	   (ps-lisp:quote ,CL-sym)))))

; SUBSTITUTE-AND-PEEP
; PS-LISP:SUBLIS would suffice here, but this additionally does some
; peephole optimizations.  Careful -- this is semantically blind!
; In particular, never put lambda-bindings in SUBST-type definitions.

(define (substitute-and-peep alist cl-form)
  (cond ((symbol? cl-form)
	 (let ((probe (assq cl-form alist)))
	   (if probe (cdr probe) cl-form)))
	((pair? cl-form)
	 (let ((yow (map (lambda (z) (substitute-and-peep alist z)) cl-form)))
	   (case (car yow)
	     ((ps-lisp:funcall) (funcallify (cadr yow) (cddr yow)))
	     (else yow))))))

; Dinky utilities

(define (insert-&rest l)
  (if (null? (cdr l))
      `(ps-lisp:&rest ,@l)
      (cons (car l) (insert-&rest (cdr l)))))

(define (cl-externalize-locals vars env)
  (map (lambda (var)
	 (cl-externalize-local (local-variable-name var) env))
       vars))

(define (cl-externalize-local name env)
  (if (qualified-symbol? name)
      ;; Don't touch local variables that aren't named by ordinary
      ;; Scheme symbols.
      name
      (if (name-in-use? name env)
	  (in-target-package (make-name-from-uid name (generate-uid)))
	  (in-target-package (name->symbol name)))))

; The lexical environment keeps track of which names are in use so that
; we can know when it's safe not to rename.

(define (generation-env free-vars) ;Initial environment
  (map program-variable-name free-vars))

(define (bind-variables vars new-names env)
  (for-each (lambda (var new-name)
	      (set-substitution! var new-name))
	    vars
	    new-names)
  (gbind vars env))

(define (bind-functions vars new-names env)
  (for-each (lambda (var new-name)
	      (set-substitution! var `(fun ,new-name)))
	    vars
	    new-names)
  (gbind vars env))

(define (gbind vars env)
  (append (map local-variable-name vars) env))

(define name-in-use? memq)

; Kludge -- use it heuristically only!

(define (mutable-program-variable? var)
  (let ((name (program-variable-name var)))
    (and (not (qualified-symbol? name))
	 (let* ((s (symbol->string name))
		(n (string-length s)))
	   (and (>= n 3)
		(char=? (string-ref s 0) #\*)
		(char=? (string-ref s (- n 1)) #\*))))))


; Package crud

(define (in-target-package sym)		;For pretty output
  (if (fluid @translating-to-file?)
      (change-package sym (fluid @target-package))
      sym))

(define (change-package sym package)
  (if (and package (not (qualified-symbol? sym)))
      (intern-renaming-perhaps (symbol->string sym) package)
      sym))

; Code emission utilities; peephole optimizers

(define (prognify form-list)
  (if (null? (cdr form-list))
      (car form-list)
      `(ps-lisp:progn ,@form-list)))

(define (deprognify cl-form)
  (if (car-is? cl-form 'ps-lisp:progn)
      (cdr cl-form)
      (list cl-form)))

(define (deandify cl-form)
  (if (car-is? cl-form 'ps-lisp:and)
      (cdr cl-form)
      (list cl-form)))

(define (deorify cl-form)
  (if (car-is? cl-form 'ps-lisp:or)
      (cdr cl-form)
      (list cl-form)))

(define (funcallify fun args)
  (cond ((car-is? fun 'ps-lisp:function)
	 ;; Peephole optimization
	 (let ((fun (cadr fun)))
	   (cond ((and (car-is? fun 'ps-lisp:lambda)
		       (not (memq 'ps-lisp:&rest (cadr fun)))
		       (= (length (cadr fun))
			  (length args)))
		  (letify (map list (cadr fun) args)
			  (prognify (cddr fun))))
		 (else
		  `(,fun ,@args)))))
	(else
	 `(ps-lisp:funcall ,fun ,@args))))

;+++ To do: turn nested singleton LET's into LET*

(define (letify specs body)
  (if (null? specs)
      body
      `(ps-lisp:let ,specs ,@(deprognify body))))

(define (sharp-quote-lambda? exp)
  (and (car-is? exp 'ps-lisp:function)
       (car-is? (cadr exp) 'ps-lisp:lambda)))

; The following hack has the express purpose of suppressing obnoxious
; warnings from losing Common Lisp compilers.  The problem would be
; mitigated if Common Lisp had some way to proclaim a variable to be
; lexical (or "not misspelled", as Moon calls it), AND if compilers treated
; variables like they did functions, permitting forward references.

(define @CL-variable-references (make-fluid 'dont-accumulate))

(define (noting-variable-references thunk)
  (let-fluid @CL-variable-references '() thunk))

(define (locally-specialize form-list)
  (let ((vars (fluid @CL-variable-references)))
    (if (or (null? vars)
	    (and (pair? form-list)
		 (pair? (car form-list))
		 (memq (caar form-list)
		       '(ps-lisp:defun ps-lisp:defstruct ps-lisp:deftype))))
	form-list
	`((ps-lisp:locally (ps-lisp:declare
			  (ps-lisp:special ,@(map program-variable-CL-symbol
					       vars)))
	    ,@form-list)))))

(define (emit-top-level code)		;form* -> form
  (if (fluid @lambda-encountered?)
      `(ps:at-top-level ,@code)
      (prognify code)))

; Continuation management

(define cont/value  '(cont/value))
(define cont/return '(cont/return))
(define cont/test   '(cont/test))
(define cont/ignore '(cont/ignore))

(define continuation-type car)

(define (deliver-value-to-cont result-exp cont)
  (case (continuation-type cont)
    ((cont/value cont/ignore) result-exp)
    ((cont/return) `(ps-lisp:return ,result-exp)) ;not return-from?
    ((cont/test) (value-form->test-form result-exp))
    (else (error "unrecognized continuation" cont))))

; For deliver-test-to-cont, we know that the value is either T or NIL.
(define (deliver-test-to-cont test-exp cont)
  (case (continuation-type cont)
    ((cont/test cont/ignore) test-exp)
    ((cont/return) `(ps-lisp:return ,(test-form->value-form test-exp)))
    ((cont/value) (test-form->value-form test-exp))
    (else (error "unrecognized continuation" cont))))

(define (test-form->value-form cl-form)
  `(ps:true? ,cl-form))
      
; (truep (true? x)) is not equivalent to x in general, but as the result
; is being used as a test form, only its non-nilness matters.
; (truep (true? x))
;  == (not (eq (or x #f) #f))
;  == (not (eq (if x x #f) #f))
;  == (if x (not (eq x #f)) (not (eq #f #f)))
;  == (if x (not (eq x #f)) nil)
; so
; (if (truep (true? x)) y z)
;  == (if (if x (not (eq x #f)) x) y z)
;  == (if x (if (not (eq x #f)) y z) (if nil y z))
;  == (if x (if (eq x #f) z y) z)
;  == (if x y z)  whenever x is not #f.
; Now the result of calling test-form->value-form is never fed in as
; the argument to value-form->test-form, and the only other place a true?
; is introduced is by the primitives, and none of those can possibly pass
; #f as the argument to true?.  Therefore the transformation
; (truep (true? x)) => x  is safe for present purposes.

(define (value-form->test-form cl-form)
  (cond ((car-is? cl-form 'ps:true?)
	 (cadr cl-form))
	(else
	 `(ps:truep ,cl-form))))
