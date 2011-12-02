; -*- Mode: Scheme; Syntax: Scheme; Package: Scheme; -*-
; File strategy.scm / See file COPYING

;;;; Compute strategy for compiling a LETREC

(define (get-letrec-strategy node)
  (or (letrec-strategy node)
      (let ((strategy
	     (let ((vars (letrec-vars node))
		   (vals (letrec-vals node)))
	       (cond ((or (null? vars)
			  (not (function-bindable? vars vals)))
		      'general)
		     ((or (some variable-value-refs? vars)
			  (some n-ary? vals)
			  (exists-losing-call? node))
		      'labels)
		     (else 'prog)))))
	(set-letrec-strategy! node strategy)
	strategy)))

; The following procedure does a tail recursion analysis to find calls
; to the labels functions that are in non-tail-recursive positions.

(define (exists-losing-call? node)
  (let ((vars (letrec-vars node)))
    (or (contains-loser? (letrec-body node) vars 'win)
	(some (lambda (proc)
		(call-will-lose? proc vars 'win))
	      (letrec-vals node)))))

(define (contains-loser? node vars k)
  (case (node-type node)
    ((local-variable program-variable constant) #f)
    ((letrec)
     (or (contains-loser? (letrec-body node) vars k)
	 (if (eq? (get-letrec-strategy node) 'prog)
	     (some (lambda (proc)
		     (call-will-lose? proc vars k))
		   (letrec-vals node))
	     (list-contains-loser? (letrec-vals node) vars 'lose))))
    ((if)
     (or (contains-loser? (if-test node) vars 'lose)
	 (contains-loser? (if-con node) vars k)
	 (contains-loser? (if-alt node) vars k)))
    ((begin)
     (or (contains-loser? (begin-first node) vars 'lose)
	 (contains-loser? (begin-second node) vars k)))
    ((set!)
     (contains-loser? (set!-rhs node) vars 'lose))
    ((lambda)
     (contains-loser? (lambda-body node) vars 'lose))
    ((call)
     (let ((proc (call-proc node)))
       (cond ((lambda? proc)
	      ;;+++ Could deal with (let ((p (lambda ...))) ... (p ...))
	      ;; here, but punt for now.
	      (or (call-will-lose? proc vars k)
		  (list-contains-loser? (call-args node) vars 'lose)))
	     ((program-variable? proc)
	      (let ((n (number-of-non-continuation-args proc)))
		(if n
		    (let loop ((a (call-args node)) (i 0))
		      (if (= i n)
			  (some (lambda (arg)
				  (call-will-lose? arg vars k))
				a)
			  (or (contains-loser? (car a) vars 'lose)
			      (loop (cdr a) (+ i 1)))))
		    (list-contains-loser? (call-args node) vars 'lose))))
	     (else
	      (or (if (memq proc vars)
		      (eq? k 'lose)
		      (contains-loser? proc vars 'lose))
		  (list-contains-loser? (call-args node) vars 'lose))))))
    (else (error "unknown node type" node))))

(define (list-contains-loser? node-list vars k)
  (some (lambda (node)
	  (contains-loser? node vars k))
	node-list))

; PROC-NODE will be evaluated and then immediately invoked.

(define (call-will-lose? proc-node vars k)
  (if (lambda? proc-node)
      (contains-loser? (lambda-body proc-node) vars k)
      (contains-loser? proc-node vars 'lose)))

(define (number-of-non-continuation-args var)
  ;; Kind of slow -- should speed this up somehow?  This information
  ;; ought to be in the integrations-table, at least.
  (cond ((or (eq? var (built-in 'and-aux))
	     (eq? var (built-in 'or-aux)))
	 1)
	((eq? var (built-in '=>-aux)) 2)
	((eq? var (built-in 'case-aux)) 1)
	(else #f)))

; True if it will be possible to bind the variables using FLET or LABELS.

(define (function-bindable? vars vals)
  (and (not (null? vars))
       (every (lambda (var)
		;; Maybe require that there be no non-function refs?
		(not (variable-assigned? var)))
	      vars)
       (every lambda? vals)))
