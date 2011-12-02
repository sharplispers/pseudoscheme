
; Scheme translator environment and structure

(define scheme-translator-env
  (make-program-env
     'scheme-translator
     (list revised^4-scheme-structure)))

(define scheme-translator-interface
  (make-interface
    'scheme-translator
    '(make-program-env
      make-interface
      make-structure
      program-env-id
      program-env-package
      program-env-lookup
      program-env-define!
      translate
      translate-lambda
      really-translate-file
      translator-version
      perform-usual-integrations!
      scheme-translator-env
      scheme-translator-structure
      revised^4-scheme-structure
      make-scheme-user-environment
      intern-renaming-perhaps
      process-define-syntax
      )
    '()))

(define scheme-translator-structure
  (make-structure 'scheme-translator
		  scheme-translator-interface
		  scheme-translator-env))


; Add integrations ("benchmark mode")

(define (perform-usual-integrations! env)
  (for-each (lambda (name)
	      (let ((probe (get-integration
			     (program-env-lookup revised^4-scheme-env name))))
		(if probe
		    (define-integration! (program-env-lookup env name)
		      probe))))
	    (interface-names revised^4-scheme-interface)))


; A pristine user environment with no integrations.

(define (make-scheme-user-environment name)
  (let ((env (make-program-env name '())))
    (for-each (lambda (name)
		(move-value-or-denotation name
					  revised^4-scheme-env
					  env))
	      (interface-names revised^4-scheme-interface))

    (let ((env-for-syntax (get-environment-for-syntax env)))
      (eval-for-syntax `(define syntax-error ,#f) env-for-syntax)
      ((eval-for-syntax `(lambda (x) (set! syntax-error x)) env-for-syntax)
       syntax-error))

    env))

(define (move-value-or-denotation name from to)
  (let ((den (program-env-lookup from name)))
    (if (and (node? den)
	     (program-variable? den))
	(let ((from-sym (program-variable-location den)))
	  (ps-lisp:if (ps-lisp:boundp from-sym)
		   (let ((to-sym (program-variable-location
				  (program-env-lookup to name))))
		     (ps-lisp:setf (ps-lisp:symbol-value to-sym)
				(ps-lisp:symbol-value from-sym))
		     (ps:set-function-from-value to-sym))
		   ;; This case handles ELSE and =>.
		   (program-env-define! to name den)))
	(program-env-define! to name den))))


; These don't really belong anywhere

(define (eval-for-syntax form env)
  (ps-lisp:eval (translate form env)))

(define (error . rest)
  (apply #'ps:scheme-error rest))


(define (generate-structure-defpackage struct)
  (let ((env (structure-program-env struct)))
    (if (eq? (structure-id struct)
	     (program-env-id env))
	(generate-program-env-defpackage env (list struct))
	(begin
	  (warn "multiple structures over a package NYI")
	  `(ps-lisp:defpackage ,(symbol->string (structure-id struct))
	     (:use ,(symbol->string (program-env-id env)))
	     (:export ,@(map (lambda (name)
			       (perhaps-rename
				(symbol->string name)))
			     (interface-names
			      (structure-interface struct)))))))))

(define (generate-program-env-defpackage env structs)
  `(ps-lisp:defpackage ,(symbol->string (program-env-id env))
     (:use ,(package-name lisp-package)
	   ,@(map (lambda (struct)
		    (symbol->string (structure-id struct)))
		  (program-env-use-list env)))
     (:export
      ,@(apply append
	       (map (lambda (struct)
		      (map (lambda (name)
			     (perhaps-rename
			      (symbol->string name)))
			   (interface-names
			    (structure-interface struct))))
		    structs)))))

			 
(define (write-defpackages struct-list filename)
  (call-with-output-file filename
    (lambda (port)
      (newline)
      (display "Writing ")
      (display (true-name port))
      (for-each (lambda (struct)
		  (with-target-package lisp-package
		    (lambda ()
		      (write-form (generate-structure-defpackage struct) port)
		      (newline port))))
		struct-list)))
  (values))
