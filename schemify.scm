; -*- Mode: Scheme; Syntax: Scheme; Package: Scheme; -*-
; File schemify.scm / See file COPYING

;;;; SCHEMIFY

; SCHEMIFY is an inverse to alpha-conversion.
;  This generally keeps the user's original variable names whenever
;  there is no conflict.  That's the only thing the env argument is
;  used for.

(define (schemify-top node)
  (schemify node '()))

(define (schemify node env)
  (if (node? node)
      (case (node-type node)
	((program-variable)
	 (program-variable-name node))
	((local-variable)
	 (let ((probe (assq node env)))
	   (if probe
	       (cdr probe)
	       (local-variable-name node))))
	((call)
	 (schemify-call node env))
	((constant)
	 (let ((val (constant-value node)))
	   (if (or (number? val) (char? val) (string? val) (boolean? val))
	       val
	       `',val)))
	((lambda)
	 (let* ((vars (lambda-vars node))
		(new-vars (map (lambda (var) (externalize-variable var env))
			       vars)))
	   `(lambda ,new-vars
	      ,@(schemify-body (lambda-body node)
			       (schemify-bind vars new-vars env)))))
	((letrec)
	 (let* ((vars (letrec-vars node))
		(vals (letrec-vals node))
		(new-vars (map (lambda (var) (externalize-variable var env))
			       vars))
		(env (schemify-bind vars new-vars env)))
	   `(letrec ,(map (lambda (var val)
			    `(,var ,(schemify val env)))
			  new-vars
			  vals)
		    ,@(schemify-body (letrec-body node) env))))
	((if)
	 (let ((test (schemify (if-test node) env))
	       (con  (schemify (if-con node) env))
	       (alt  (schemify (if-alt node) env)))
	   ;;+++ Deal with an UNSPECIFIC alt
	   `(if ,test ,con ,alt)))
	((set!)
	 `(set! ,(schemify (set!-lhs node) env)
		,(schemify (set!-rhs node) env)))
	((begin)
	 `(begin ,(schemify (begin-first node) env)
		 ,@(unbeginify (schemify (begin-second node) env))))
	((define)
	 (let ((var (schemify (define-lhs node) env)))
	   (if (not (symbol? var))
	       (error "defining a non-variable -- shouldn't happen" var))
	   `(define ,var
	      ,(schemify (define-rhs node) env))))
	(else
	 `(unknown-node-type ,node)))
      node))

(define (schemify-call node env)
  (let* ((proc (call-proc node))
	 (args (call-args node))
	 (punt (lambda ()
		 `(,(schemify proc env)
		   ,@(map (lambda (subnode) (schemify subnode env))
			  args)))))
    (case (node-type proc)
      ((lambda)
       ;; +++ Check for mismatching # of args
       (let ((proc-exp (schemify proc env)))
	 `(let ,(map (lambda (var arg) `(,var ,(schemify arg env)))
		     (cadr proc-exp)
		     args)
	    ,@(cddr proc-exp))))
      ((program-variable)
       ;; Rather kludgey.
       (cond ((eq? (program-variable-location proc)
		   (program-env-lookup revised^4-scheme-env 'and-aux))
	      `(and ,(schemify (car args) env)
		    ,(dethunkify (cadr args) env)))
	     ((eq? (program-variable-location proc)
		   (program-env-lookup revised^4-scheme-env 'or-aux))
	      `(or ,(schemify (car args) env)
		   ,(dethunkify (cadr args) env)))
	     ((eq? (program-variable-location proc)
		   (program-env-lookup revised^4-scheme-env 'case-aux))
	      `(case ,(schemify (car args) env)
		 ,@(map (lambda (keys arg)
			  `(,keys ,@(unbeginify (dethunkify arg env))))
			(constant-value (cadr args))
			(cdddr args))
		 (else ,(dethunkify (caddr args) env))))
	     ;; make-promise
	     (else (punt))))
      (else (punt)))))

(define (dethunkify node env)
  (if (and (lambda? node)
	   (null? (lambda-vars node)))
      (schemify (lambda-body node) env)
      `(,(schemify node env))))

(define (schemify-body node env)
  (unbeginify (schemify node env)))

(define (unbeginify exp)
  (if (car-is? exp 'begin) (cdr exp) (list exp)))

; Generate a non-conflicting name

(define (externalize-variable var env)
  (let ((name (local-variable-name var)))
    (if (rassq name env)
	(make-name-from-uid name (generate-uid))
	name)))

(define (schemify-bind vars names env)
  (append (map cons vars names) env))
