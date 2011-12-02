; File alpha.scm -*- Mode: Scheme; Syntax: Scheme; Package: Scheme; -*-
; See file COPYING

;;;; Alpha-conversion

; Contexts

(define (note-context! context node)
  (context node))

(define value-context     set-value-refs!)
(define procedure-context set-proc-refs!)
(define lvalue-context	  set-assigned!)
(define define-context	  (lambda (var) var 'define-context))
(define top-level-context (lambda (var) var 'top-level-context))

(define (lose context)  ;Ugh.   (let ((f (lambda () 1))) ((begin (foo) f)))
  context ;lose
  value-context)

(define @free-variables (make-fluid '()))


; Top-level entry point.

(define (alpha-top form s-env)
  (alpha form s-env top-level-context))

; Alphatization of a single scheme expression

(define @where (make-fluid '<top>))

(define (alpha form s-env context)
  (call-with-values (lambda () (classify form s-env))
    (lambda (class form s-env)
      ((vector-ref alphatizers class) form s-env context))))

(define alphatizers
  (make-vector number-of-classes
	       (lambda (form s-env context)
		 (error "don't know how to alphatize this class"
			form))))

(define (define-alphatizer class proc)
  (vector-set! alphatizers class proc))

(define-alphatizer class/literal
  (lambda (exp s-env context)
    s-env context			;ignored
    (make-constant exp #f)))

(define-alphatizer class/name
  (lambda (exp s-env context)
    (let ((denotation (lookup s-env exp)))
      (cond ((node? denotation)
	     (if (local-variable? denotation)
		 (note-context! context denotation)
		 (let ((free (fluid @free-variables)))
		   (if (not (memq denotation free))
		       (set-fluid! @free-variables (cons denotation free)))))
	     denotation)
	    (else
	     (alpha (syntax-error "syntactic keyword in invalid position" exp)
		    s-env context))))))

(define-alphatizer class/application
  (lambda (exp s-env context)
    context				;ignored
    (make-call (alpha (car exp) s-env procedure-context)
	       (map (lambda (arg) (alpha arg s-env value-context))
		    (cdr exp)))))


; The primitive special forms.

(define-alphatizer class/quote
  (lambda (exp s-env context)
    s-env context			;ignored
    (make-constant (strip (cadr exp)) #t)))

(define-alphatizer class/lambda
  (lambda (exp s-env context)
    (if (not (eq? context procedure-context))
	;; Not very accurate.  Improve later.
	(for-each-local set-closed-over!
			s-env))
    (let ((s-env (rename-vars (proper-listify (cadr exp)) s-env)))
      (make-lambda (new-names (cadr exp) s-env)
		   (alpha-body (cddr exp) s-env value-context)))))

(define-alphatizer class/letrec
  (lambda (exp s-env context)
    (let* ((specs (cadr exp))
	   (vars (map car specs))
	   (s-env (rename-vars vars s-env))
	   (new-vars (new-names vars s-env)))
      (make-letrec new-vars
		   (map (lambda (spec)
			  (alpha (cadr spec) s-env value-context))
			specs)
		   (alpha-body (cddr exp) s-env (lose context))))))

(define (alpha-body forms s-env context)
  (call-with-values (lambda () (scan-body forms s-env))
    (lambda (specs exps s-env)
      (if (null? specs)
	  (alpha-beginify exps s-env context)
	  (let ((new-vars (map (lambda (spec)
				 (make-local-variable (car spec)))
			       specs)))
	    (for-each (lambda (spec var)
			(define! s-env (car spec) var))
		      specs
		      new-vars)
	    (make-letrec new-vars
			 (map (lambda (spec)
				(alpha (cadr spec) (caddr spec) value-context))
			      specs)
			 (alpha-beginify exps s-env (lose context))))))))

(define-alphatizer class/if
  (lambda (exp s-env context)
    (let ((test (alpha (cadr exp) s-env value-context))
	  (con  (alpha (caddr exp) s-env (lose context)))
	  (alt (alpha (let ((tail (cdddr exp)))
			(if (null? tail)
			    'ps:unspecific
			    (car tail)))
		      s-env
		      (lose context))))
      (make-if test con alt))))

(define-alphatizer class/set!
  (lambda (exp s-env context)
    context				;ignored
    (let ((lhs (alpha (cadr exp) s-env lvalue-context)))
      (if (variable? lhs)
	  (make-set! lhs
		     (alpha (caddr exp) s-env value-context))
	  (alpha (syntax-error "invalid SET!" exp) s-env context)))))

(define-alphatizer class/begin
  (lambda (exp s-env context)
    (if (null? (cdr exp))
	(begin (if (not (eq? context top-level-context))
		   (note "(begin) disallowed in this context" exp))
	       (alpha 'ps:unspecific s-env context))
	(alpha-beginify (cdr exp) s-env context))))

(define (alpha-beginify exp-list s-env context)
  (cond ((null? (cdr exp-list))
	 (alpha (car exp-list) s-env context))
	(else
	 (make-begin
	  (alpha (car exp-list)
		 s-env
		 (if (eq? context top-level-context)
		     context
		     value-context))
	  (alpha-beginify (cdr exp-list)
			  s-env
			  (if (eq? context top-level-context)
			      context
			      (lose context)))))))

(define-alphatizer class/define
  (lambda (form s-env context)
    (cond ((eq? context top-level-context)
	   (let ((var (ensure-defined s-env (define-form-lhs form))))
	     ;; (set-status! var 'defined)
	     (let-fluid @where (program-variable-name var)
			(lambda ()
			  (make-define var
				       (alpha (define-form-rhs form)
					      s-env value-context))))))
	  (else
	   (alpha (syntax-error "(define ...) disallowed in this context" form)
		  s-env context)))))

(define-alphatizer class/define-syntax
  (lambda (form s-env context)
    (cond ((eq? context top-level-context)
	   (process-define-syntax form s-env) ;side effect
	   (make-call (alpha 'ps:%define-syntax! s-env value-context)
		      (list (make-constant (cdr form) #t))))
	  (else 
	   (alpha (syntax-error
		   "(define-syntax ...) disallowed in this context" form)
		  s-env context)))))

(define (initialize-core-syntax env)
  (define! env 'lambda	      (make-special-operator class/lambda))
  (define! env 'letrec	      (make-special-operator class/letrec))
  (define! env 'if	      (make-special-operator class/if))
  (define! env 'quote	      (make-special-operator class/quote))
  (define! env 'begin	      (make-special-operator class/begin))
  (define! env 'set!	      (make-special-operator class/set!))
  (define! env 'let-syntax    (make-special-operator class/let-syntax))
  (define! env 'letrec-syntax (make-special-operator class/letrec-syntax))
  (define! env 'define	      (make-special-operator class/define))
  (define! env 'define-syntax (make-special-operator class/define-syntax)))


; Revised^4 environment

(define revised^4-scheme-env
  (make-program-env 'revised^4-scheme '()))

(initialize-core-syntax revised^4-scheme-env)

(define revised^4-scheme-structure
  (make-structure 'revised^4-scheme	;Exports everything
		  revised^4-scheme-interface
		  revised^4-scheme-env))

(define (built-in name)
  (program-env-lookup revised^4-scheme-env name))


; Utilities:

(define (read-file filename)
  (call-with-input-file filename
    (lambda (i-port)
      (let loop ((l '()))
	(let ((form (read i-port)))
	  (cond ((eof-object? form) (reverse l))
		(else
		 (loop (cons form l)))))))))

(define (note msg node)
  (newline)
  (display "** ")
  (display msg)
  (if node
      (begin (display ": ")
	     (write (let-fluid @where '<note>
			       (lambda ()
				 (schemify-top node))))
	     (newline)
	     (display "   Location: ")
	     (write (fluid @where))))
  (newline))

(define (syntax-error msg form)
  (note msg form)
  `(ps:scheme-error ',msg ',form))

; Code generation utilities:

; Unique id's

(define @unique-id (make-fluid 0))

(define (with-uid-reset thunk)
  (let-fluid @unique-id 0 thunk))

(define (generate-uid)
  (let ((uid (fluid @unique-id)))
    (set-fluid! @unique-id (+ uid 1))
    uid))

(define (make-name-from-uid name uid)  ;Best if it's not a legal Scheme symbol.
  (intern
   (string-append "."
		  (name->string name)
		  "."
		  (number->string uid ))
   (fluid @target-package)))

(define (rename-vars names s-env)
  (bind names (map make-local-variable names) s-env))

(define (new-names bvl env)
  (map-bvl (lambda (var)
	     (lookup env var))
	   bvl))

(define (car-is? thing x)  ;useful for peephole optimizers
  (and (pair? thing)
       (eq? (car thing) x)))
