; -*- Mode: Scheme; Syntax: Scheme; Package: Scheme; -*-
; File list.scm / See file COPYING

;;;; List utilities

(define (some pred l)
  (and (not (null? l))
       (or (pred (car l)) (some pred (cdr l)))))

(define (every pred l)
  (or (null? l)
      (and (pred (car l)) (every pred (cdr l)))))

(define (rassq obj lst)
  (cond ((null? lst) #f)
	((eq? obj (cdar lst)) (car lst))
	(else (rassq obj (cdr lst)))))

(define (filter pred l)
  (cond ((null? l) '())
	((pred (car l)) (cons (car l) (filter pred (cdr l))))
	(else (filter pred (cdr l)))))

(define (right-reduce proc lst identity)
  (cond ((null? lst) identity)
	(else (right-reduce proc (cdr lst) (proc (car lst) identity)))))

(define reduce right-reduce)

; Set utilities

(define (setdiffq l1 l2)
  (cond ((null? l2) l1)
	((null? l1) l1)
	((memq (car l1) l2)
	 (setdiffq (cdr l1) l2))
	(else (cons (car l1)
		    (setdiffq (cdr l1) l2)))))

(define (unionq l1 l2)
  (cond ((null? l1) l2)
	((null? l2) l1)
	((memq (car l1) l2) (unionq (cdr l1) l2))
	(else (cons (car l1) (unionq (cdr l1) l2)))))

(define (intersectq l1 l2)
  (cond ((null? l1) l1)
	((null? l2) l2)
	((memq (car l1) l2)
	 (cons (car l1) (intersectq (cdr l1) l2)))
	(else (intersectq (cdr l1) l2))))

(define (intersectq? l1 l2)
  (and (not (null? l1))
       (not (null? l2))
       (or (memq (car l1) l2)
	   (intersectq? (cdr l1) l2))))
