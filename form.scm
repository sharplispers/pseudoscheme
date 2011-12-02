;==============================================================================
; Expressions
;
; Code for recognizing, destructuring, and checking the syntax of forms.

(define (literal? x)
  (or (number? x) (string? x) (boolean? x) (char? x)))


(define syntax-checkers
  (make-vector number-of-classes (lambda (form) form #t)))

(define (define-syntax-checker class proc)
  (vector-set! syntax-checkers class proc))

(define (check-special-form-syntax class form)
  ((vector-ref syntax-checkers class) form))


; (let-syntax (<syntax-spec>*) <body>)

(define let-syntax-form-dspecs cadr)
(define let-syntax-form-body caddr)

(define-syntax-checker class/let-syntax
  (lambda (exp)
    (and (= (careful-length exp) 3)
	 (careful-every check-syntax-spec (cadr exp)))))

; (letrec-syntax (<syntax-spec>*) <body>)

(define letrec-syntax-form-dspecs let-syntax-form-dspecs)
(define letrec-syntax-form-body   let-syntax-form-body)

(define-syntax-checker class/letrec-syntax
  (lambda (exp)
    (and (= (careful-length exp) 3)
	 (careful-every check-syntax-spec (cadr exp)))))

; Syntax specs (<name> <form>)

(define syntax-spec-name car)
(define syntax-spec-form cadr)
(define syntax-spec-free-names caddr)

(define (check-syntax-spec syntax-spec)
  (let ((len (careful-length syntax-spec)))
    (and (or (= len 2)
	     (and (= len 3)
		  ;; Hack for scheme48 linker
		  (list? (syntax-spec-free-names syntax-spec))))
	 (name? (syntax-spec-name syntax-spec)))))

; (define-syntax <name> <form>)

(define define-syntax-syntax-spec cdr)

(define-syntax-checker class/define-syntax
  (lambda (form)
    (check-syntax-spec (cdr form))))

; (begin <statement>*)

(define begin-form-statements cdr)

(define-syntax-checker class/begin
  (lambda (form)
    (>= (careful-length form) 1)))	;must be a proper list


; application

(define application-form-procedure car)
(define application-form-arguments cdr)


; (lambda (<name>*) <body>)

(define lambda-form-formals cadr)
(define lambda-form-body cddr)

(define-syntax-checker class/lambda
  (lambda (exp)
    (and (>= (careful-length exp) 3)
	 (check-formals (lambda-form-formals exp)))))

(define (check-formals formals)
  (or (null? formals)
      (name? formals)
      (and (name? (car formals)) (check-formals (cdr formals)))))


; (letrec ((<name> <exp>)) <body>)

(define letrec-form-bspecs cadr)
(define letrec-form-body cddr)

(define-syntax-checker class/letrec
  (lambda (exp)
    (and (>= (careful-length exp) 3)
	 (careful-every (lambda (syntax-spec)
			  (and (= (careful-length syntax-spec) 2)
			       (name? (syntax-spec-name syntax-spec))))
			(letrec-form-bspecs exp)))))


; (quote <text>)

(define quote-form-text cadr)

(define-syntax-checker class/quote
  (lambda (exp)
    (= (careful-length exp) 2)))


; (if <test> <con> <alt>)

(define if-form-test cadr)
(define if-form-consequent caddr)
(define (if-form-alternate? exp)
  (not (null? (cdddr exp))))
(define if-form-alternate cadddr)

(define-syntax-checker class/if
  (lambda (exp)
    (let ((len (careful-length exp)))
      (or (= len 3) (= len 4)))))


; (set! <lhs> <rhs>)

(define set!-form-lhs cadr)
(define set!-form-rhs caddr)

(define-syntax-checker class/set!
  (lambda (exp)
    (and (= (careful-length exp) 3)
	 (name? (cadr exp)))))


; (define name exp) or (define (name . args) . body)

(define-syntax-checker class/define
  (lambda (form)
    (and (pair? (cdr form))
	 (let ((pat (cadr form))
	       (len (careful-length form)))
	   (if (name? pat)
	       (or (= len 2) (= len 3))
	       (and (pair? pat)
		    (check-formals (cdr pat))
		    (>= len 3)))))))

(define (define-form-lhs form)
  (let ((pat (cadr form)))
    (if (pair? pat) (car pat) pat)))

(define (define-form-rhs form)
  (let ((pat (cadr form)))
    (cond ((pair? pat)
	   `(lambda ,(cdr pat) ,@(cddr form)))
	  ((null? (cddr form))
	   'ps:unspecific)		;(define foo)
	  (else
	   (caddr form)))))


; Versions of LENGTH and EVERY that do not assume that the lists they are
; handed are proper.

(define (careful-length l)
  (if (null? l)
      0
      (if (pair? l)
	  (+ 1 (careful-length (cdr l)))
	  -1)))

(define (careful-every pred l)
  (if (null? l)
      #t
      (and (pair? l)
	   (pred (car l))
	   (careful-every pred (cdr l)))))
