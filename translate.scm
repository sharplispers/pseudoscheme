; -*- Mode: Scheme; Syntax: Scheme; Package: Scheme; -*-
; File translate.scm / See file COPYING

;;;; Translation from Scheme to Common Lisp

; TRANSLATE translates a single Scheme expression into Common Lisp.

(define (translate form env)
  (with-target-package (program-env-package env)
    (lambda ()
      (translate-to-common-lisp (list form) env))))

; Used by translate and translate-file

(define (translate-to-common-lisp forms env)
  (prognify
   (let recur ((forms forms))
     (if (null? forms)
	 '()
	 (cons (with-uid-reset
		(lambda ()
		  (let-fluid @free-variables '()
		    (lambda ()
		      (let ((node (alpha-top (car forms) env)))
			(generate-top
			 node
			 (generation-env (fluid @free-variables))
			 (not (null? (cdr forms)))))))))
	       (recur (cdr forms)))))))

; Used by SCHEME-COMPILE.

(define (translate-lambda form env)
  (with-uid-reset
   (lambda ()
     (let-fluid @free-variables '()
       (lambda ()
	 (let ((node (alpha-top form env)))
	   (if (lambda? node)
	       (generate-lambda-top
		  node
		  (generation-env (fluid @free-variables)))
	       (error "not a lambda expression" form))))))))

; File transduction

(define (really-translate-file source-file-name
			       translated-file-name
			       program-env)
  (let ((source-code (read-file source-file-name)))
    (compiling-to-file
      translated-file-name
      (program-env-package program-env)
      (lambda (port)
	(display ";  from file " port)
	(display (true-name source-file-name) port)
	(newline port))
      (lambda (port)
	(for-each (lambda (form)
		    (write-flattened form port))
		  (translate-to-common-lisp source-code program-env))))))

; The following generates a file CLOSED.PSO from the information we
; have on how to open-code the built-in procedures.

(define (write-closed-definitions structure outfile)
  (compiling-to-file outfile
		     (structure-package structure)
		     (lambda (port) port)
		     (lambda (port)
		       (write-closed-definitions-1 structure port))))

(define (write-closed-definitions-1 structure port)
  (let ((sig (structure-interface structure))
	(env (structure-program-env structure)))
    (let ((funs '())
	  (defs '()))
      (let ((do-it
	     (lambda (name)
	       (let* ((den (program-env-lookup env name))
		      (info (get-integration den)))
		 (if info
		     (let ((sym (program-variable-location den)))
		       (case (car info)
			 ((val)
			  (write-form `(ps-lisp:locally
					   (ps-lisp:declare (ps-lisp:special ,sym))
					 (ps-lisp:setq ,sym ,(cadr info)))
				      port)
			  (write-form `(ps:set-function-from-value
					    (ps-lisp:quote ,sym))
				      port))
			 ((fun)
			  (if (not (memq name '(car cdr))) ;kludge
			      (set! funs (cons (list sym (cadr info))
					       funs))))
			 ((pred)
			  (write-form
			   (case (if (null? (cddr info))
				    'n
				    (caddr info))
			     ((1)
			      `(ps-lisp:defun ,sym (x)
				 (ps:true? (,(cadr info) x))))
			     ((2)
			      `(ps-lisp:defun ,sym (x y)
				 (ps:true? (,(cadr info) x y))))
			     (else
			      `(ps-lisp:defun ,sym (ps-lisp:&rest x)
				 (ps:true? (ps-lisp:apply #',(cadr info)
							 x)))))
			   port)
			  (set! defs (cons sym defs)))
			 ((subst lambda)
			  (write-form `(ps-lisp:defun ,sym ,@(cdr info)) port)
			  (set! defs (cons sym defs)))
			 ((special) 0) ;don't generate any definition
			 (else
			  (error "peculiar built-in" info)))))))))
	(for-each do-it (interface-names sig))
	(for-each do-it (interface-aux-names sig)))
      (write-form
        `(ps-lisp:mapc (ps-lisp:function ps:set-value-from-function)
		    (ps-lisp:quote ,(reverse defs)))
	port)
      (write-form
        `(ps-lisp:mapc #'(ps-lisp:lambda (z)
			(ps-lisp:let ((our-sym (ps-lisp:car z))
				   (cl-sym (ps-lisp:cadr z)))
			  (ps-lisp:setf (ps-lisp:symbol-function our-sym) 
				     (ps-lisp:symbol-function cl-sym))
			  (ps:set-value-from-function our-sym)))
		    (ps-lisp:quote ,(reverse funs)))
	port))))

; Utilities

(define (with-target-package package thunk)
  (let-fluid @target-package package
    thunk))

(define (compiling-to-file outfile package write-message proc)
  (let-fluid @translating-to-file? #t
    (lambda ()
      (with-target-package package
	(lambda ()
	  (call-with-output-file outfile
	    (lambda (port)
	      (write-file-identification port)
	      (write-message port)
	      (newline port)
	      (display "(ps:in-package " port)
	      (write (package-name package) port)
	      (display ")" port)
	      (newline port)
	      (write-form '(ps:begin-translated-file) port)
	      (newline port)
	      ;; Now do the real work.
	      (proc port)
	      (newline port)
	      outfile)))))))

(define (write-file-identification port)
  (newline)
  (display "Writing ")
  (display (true-name port))
  (display "; -*- Mode: Lisp; Syntax: Common-Lisp; Package: " port)
  (display (package-name (fluid @target-package)) port) ;Heuristic
  (display "; -*-" port)
  (newline port)
  (newline port)
  (display "; This file was generated by " port)
  (display (translator-version) port)
  (newline port)
  (display ";  running in " port)
  (display (scheme-implementation-version) port)
  (newline port))

(define (write-flattened form port)
  (cond ((not (pair? form))
	 (if (not (or (symbol? form)
		      (number? form)
		      (boolean? form)
		      (string? form)
		      (char? form)))
	     ;; Who knows, it might be important.
	     (write-form form port)))
        ((eq? (car form) 'ps-lisp:quote)
	 )				;do nothing
	((eq? (car form) 'ps-lisp:progn)
	 (for-each (lambda (form)
		     (write-flattened form port))
		   (cdr form)))
	(else
	 (write-form form port))))

(define (write-form form port)
  (write-pretty form port (fluid @target-package)))
