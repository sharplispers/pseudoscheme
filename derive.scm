; -*- Mode: Scheme; Syntax: Scheme; Package: Scheme; -*-
; File derive.scm / See file COPYING

;;;; Macro expanders for standard derived expression types

(define (define-usual-syntax name expander)
  (program-env-define! revised^4-scheme-env
		       name
		       (make-macro (lambda (form r c)
				     (apply expander r c (cdr form)))
				   revised^4-scheme-env)))

; syntax-rules is defined elsewhere

(program-env-define! revised^4-scheme-env
		     'syntax-rules
		     (make-macro rewrite-syntax-rules revised^4-scheme-env))

; The expanders:
;  r = rename
;  c = compare

(define-usual-syntax 'and
  (lambda (r c . conjuncts)
    c ;ignored
    (if (null? conjuncts)
	#t
	(let recur ((first (car conjuncts)) (rest (cdr conjuncts)))
	  (if (null? rest)
	      first
	      `(,(r 'and-aux) ,first
		  (,(r 'lambda) () ,(recur (car rest) (cdr rest)))))))))

(define-usual-syntax 'or
  (lambda (r c . disjuncts)
    c ;ignored
    (if (null? disjuncts)
	#f
	(let recur ((first (car disjuncts)) (rest (cdr disjuncts)))
	  (if (null? rest)
	      first
	      `(,(r 'or-aux) ,first
		  (,(r 'lambda) () ,(recur (car rest) (cdr rest)))))))))


; (case key ((a b) x) ((c) y) (else z))
;  ==>  (case-aux key
;		  '((a b) (c))
;		  (lambda () z)
;		  (lambda () x)
;		  (lambda () y))

(define-usual-syntax 'case
  (lambda (r c key . clauses)
    (let ((form-result
	   (lambda (else-thunk thunks key-lists)
	     `(,(r 'case-aux) ,key
			(,(r 'quote) ,(reverse key-lists))
			,else-thunk
			,@(reverse thunks)))))
      (let loop ((cs clauses) (thunks '()) (key-lists '()))
	(if (null? cs)
	    (form-result `(,(r 'lambda) () ,(r 'unspecific))
			 thunks key-lists)
	    (let* ((clause (car cs))
		   (key-list (car clause))
		   (body (cdr clause)))
	      (if (c key-list (r 'else))
		  (form-result `(,(r 'lambda) () ,@body) thunks key-lists)
		  (loop (cdr cs)
			(cons `(,(r 'lambda) () ,@body) thunks)
			(cons key-list key-lists)))))))))

(define-usual-syntax 'cond
  (lambda (r c . clauses)
    (let recur ((clauses clauses))
      (if (null? clauses)
	  (r 'unspecific)
	  (process-cond-clause r c
			       (car clauses)
			       (recur (cdr clauses)))))))

; Auxiliary also used by DO

(define (process-cond-clause r c clause rest)
  (cond ((null? (cdr clause))
	 `(,(r 'or-aux) ,(car clause)
		  (,(r 'lambda) () ,rest)))
	((c (car clause) (r 'else))
	 `(,(r 'begin) ,@(cdr clause)))
	((c (cadr clause) (r '=>))
	 `(,(r '=>-aux) ,(car clause)
		  (,(r 'lambda) () ,(caddr clause))
		  (,(r 'lambda) () ,rest)))
	(else
	 `(,(r 'if) ,(car clause)
	      (,(r 'begin) ,@(cdr clause))
	      ,rest))))

(define-usual-syntax 'delay
  (lambda (r c thing)
    c ;ignored
    `(,(r 'make-promise) (,(r 'lambda) () ,thing))))

(define-usual-syntax 'do
  (lambda (r c specs end . body)
    c ;ignored
    (let ((loop (r 'loop)))
      `(,(r 'letrec) ((,loop
		       (,(r 'lambda)
			,(map car specs)
			,(process-cond-clause
			  r c
			  end
			  `(,(r 'begin) ,@body
					(,loop ,@(map (lambda (y)
							(if (null? (cddr y))
							    (car y)
							    (caddr y)))
						      specs)))))))
		     (,loop ,@(map cadr specs))))))

(define-usual-syntax 'let
  (lambda (r c specs . body)
    c ;ignored
    (cond ((name? specs)
	   (let ((tag specs)
		 (specs (car body))
		 (body (cdr body)))
	     `(,(r 'letrec) ((,tag (,(r 'lambda) ,(map car specs) ,@body)))
			    (,tag ,@(map cadr specs)))))
	  (else
	   `((,(r 'lambda) ,(map car specs) ,@body)
	     ,@(map cadr specs))))))

(define-usual-syntax 'let*
  (lambda (r c specs . body)
    c ;ignored
    (let recur ((specs specs))
      (if (null? specs)
	  `(,(r 'let) () ,@body)
	  (let ((name (car (car specs)))
		(val-exp (cadr (car specs))))
	    `(,(r 'let) ((,name ,val-exp))
	       ,(recur (cdr specs))))))))

;;;; Quasiquote

(define-usual-syntax 'quasiquote
  (lambda (r c x)
    c ;ignored
    (qq-descend x 1 r)))

(define (qq-descend x level r)
  (cond ((vector? x)
	 (qq-descend-vector x level r))
	((not (pair? x))
	 (make-quotation x r))
	((qq-interesting? x 'quasiquote)
	 (qq-descend-pair x (+ level 1) r))
	((qq-interesting? x 'unquote)
	 (if (= level 1)
	     (cadr x)
	     (qq-descend-pair x (- level 1) r)))
	((qq-interesting? x 'unquote-splicing)
	 (if (= level 1)
	     (error ",@ in illegal position" x)
	     (qq-descend-pair x (- level 1) r)))
        (else
	 (qq-descend-pair x level r))))

(define (qq-descend-pair x level r)
  (let ((d-exp (qq-descend (cdr x) level r)))
    (if (and (qq-interesting? (car x) 'unquote-splicing)
	     (= level 1))
	(let ((sc (cadr (car x))))
	  (cond ((and (quotation? d-exp r)
		      (null? (quotation-value d-exp)))
		 sc)
		(else
		 `(,(r 'append) ,sc ,d-exp))))
	(let ((a-exp (qq-descend (car x) level r)))
	  (cond ((and (quotation? a-exp r)
		      (quotation? d-exp r))
		 (make-quotation x r))
		((and (quotation? d-exp r)
		      (eq? (quotation-value d-exp) '()))
		 `(,(r 'list) ,a-exp))
		((qq-interesting? d-exp 'list)
		 `(,(r 'list) ,a-exp ,@(cdr d-exp)))
		;;+++ Ought to use auxiliary CONS* procedure, for more
		;; readable output
		(else
		 `(,(r 'cons) ,a-exp ,d-exp)))))))

(define (qq-descend-vector x level r)
  (let ((result (qq-descend (vector->list x) level r)))
    (if (quotation? result r)
	(make-quotation x r)
	`(,(r 'list->vector) ,result))))

(define (qq-interesting? x marker)
  (and (pair? x)
       (eq? (car x) marker)
       (pair? (cdr x))
       (null? (cddr x))))

(define (quotation? x r)
  (qq-interesting? x (r 'quote)))

(define quotation-value cadr)

(define (make-quotation value r)
  `(,(r 'quote) ,value))
