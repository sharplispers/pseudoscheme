; -*- Mode: Scheme; Syntax: Scheme; Package: Scheme; -*-
; File node.scm / See file COPYING

;;;; Node abstraction

;+++ Make it abstract at some point.

; Standard type order (8):
;  constant variable LAMBDA LETREC IF BEGIN SET! call

(define (node? obj)
  (and (vector? obj)
       (>= (vector-length obj) 1)
       (memq (vector-ref obj 0)
	     '(constant local-variable program-variable
			lambda letrec if begin set! call))))

(define (node-type node)
  (vector-ref node 0))

(define (node-predicate type)
  (lambda (node)
    (eq? (node-type node) type)))

(define (node-accessor type index)
  (lambda (node)
    (if (not (eq? (node-type node) type))
	(error "wrong node type" node type))
    (vector-ref node index)))

(define (node-modifier type index)
  (lambda (node new-val)
    (if (not (eq? (node-type node) type))
	(error "wrong node type" node type))
    (vector-set! node index new-val)))

; Constant

(define (make-constant val quoted?)
  (vector 'constant val quoted?))

(define constant? (node-predicate 'constant))

(define constant-value (node-accessor 'constant 1))
(define constant-quoted? (node-accessor 'constant 2))

; LAMBDA

(define (make-lambda vars body-node)
  (vector 'lambda vars body-node))

(define lambda? (node-predicate 'lambda))

(define lambda-vars (node-accessor 'lambda 1))
(define lambda-body (node-accessor 'lambda 2))

(define (n-ary? proc)
  (not (proper-list? (lambda-vars proc))))

(define (proper-list? thing)
  (or (null? thing)
      (and (pair? thing)
	   (null? (cdr (last-pair thing))))))

(define (proper-listify thing)
  (cond ((null? thing) '())
	((pair? thing) (cons (car thing) (proper-listify (cdr thing))))
	(else (list thing))))

(define (map-bvl proc bvl)
  (cond ((null? bvl) '())
	((pair? bvl)
	 (cons (proc (car bvl)) (map-bvl proc (cdr bvl))))
	(else (proc bvl))))

(define (for-each-bvl proc bvl)
  (cond ((null? bvl) #t)
	((pair? bvl)
	 (proc (car bvl))
	 (for-each-bvl proc (cdr bvl)))
	(else (proc bvl))))

; LETREC

(define (make-letrec vars val-nodes body-node)
  (vector 'letrec vars val-nodes body-node #f))

(define letrec? (node-predicate 'letrec))

(define letrec-vars (node-accessor 'letrec 1))
(define letrec-vals (node-accessor 'letrec 2))
(define letrec-body (node-accessor 'letrec 3))
(define letrec-strategy (node-accessor 'letrec 4))

(define set-letrec-strategy! (node-modifier 'letrec 4))

; IF

(define (make-if test con alt)
  (vector 'if test con alt))

(define if? (node-predicate 'if))

(define if-test (node-accessor 'if 1))
(define if-con  (node-accessor 'if 2))
(define if-alt  (node-accessor 'if 3))

; BEGIN

(define (make-begin first second)
  (vector 'begin first second))
(define begin? (node-predicate 'begin))
(define begin-first  (node-accessor 'begin 1))
(define begin-second (node-accessor 'begin 2))

; SET!

(define (make-set! lhs rhs)
  (vector 'set! lhs rhs))
(define set!? (node-predicate 'set!))
(define set!-lhs (node-accessor 'set! 1))
(define set!-rhs (node-accessor 'set! 2))

; Call

(define (make-call proc-node arg-nodes)
  (vector 'call proc-node arg-nodes))

(define call? (node-predicate 'call))
(define call-proc (node-accessor 'call 1))
(define call-args (node-accessor 'call 2))

; Definition

(define (make-define lhs rhs)
  (vector 'define lhs rhs))
(define define? (node-predicate 'define))
(define define-lhs (node-accessor 'define 1))
(define define-rhs (node-accessor 'define 2))

; Variables

(define (make-local-variable uname)
  (vector 'local-variable
	  uname				;1 user's name
	  #f     			;2 type (not used by pseudoscheme)
	  #f				;3 substitution
	  #f				;4 path - obsolete
	  #f				;5 value-refs?
	  #f				;6 proc-refs?
	  #f				;7 assigned?
	  #f				;8 closed-over?
	  ))

(define local-variable? (node-predicate 'local-variable))

(define local-variable-name   (node-accessor 'local-variable 1))
(define local-variable-type   (node-accessor 'local-variable 2))
(define variable-substitution (node-accessor 'local-variable 3))

(define set-local-variable-type! (node-modifier 'local-variable 2))
(define set-substitution! (node-modifier 'local-variable 3))

(define variable-value-refs?  (node-accessor 'local-variable 5))
(define variable-proc-refs?   (node-accessor 'local-variable 6))
(define variable-assigned?    (node-accessor 'local-variable 7))
(define variable-closed-over? (node-accessor 'local-variable 8))

(define (variable-incrementator n)
  (let ((ref (node-accessor 'local-variable n))
	(mod (node-modifier 'local-variable n)))
    (lambda (var)
      (mod var (+ (or (ref var) 0) 1)))))

(define set-value-refs!  (variable-incrementator 5))
(define set-proc-refs!   (variable-incrementator 6))
(define set-assigned!    (variable-incrementator 7))
(define set-closed-over! (variable-incrementator 8))

; Program (or "global" or "top-level") variables

(define (make-program-variable name loc)
  (vector 'program-variable name #f loc))

(define program-variable? (node-predicate 'program-variable))

(define program-variable-name     (node-accessor 'program-variable 1))
(define program-variable-type     (node-accessor 'program-variable 2))
(define program-variable-location (node-accessor 'program-variable 3))

(define set-program-variable-type! (node-modifier 'program-variable 2))

(define (variable? node)
  (or (local-variable? node)
      (program-variable? node)))
