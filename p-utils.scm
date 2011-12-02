; -*- Mode: Scheme; Syntax: Scheme; Package: Scheme; -*-
; File utils.scm / See file COPYING

;;;; Miscellaneous general and not-so-general utilities

; last-pair (was in r^3, flushed for r^4)

(define (last-pair x)
  (ps-lisp:last x))

; posq

(define (vector-posq thing v)
  (ps-lisp:or (ps-lisp:position thing (ps-lisp:the ps-lisp:simple-vector v))
	   #f))

(define (string-posq c s)
  (ps-lisp:or (ps-lisp:position c (ps-lisp:the ps-lisp:simple-string s))
	   #f))

; Fluids

(define (make-fluid top-level-value)
  (let ((f (ps-lisp:gensym "FLUID")))
    (ps-lisp:set f top-level-value)
    f))

(define (fluid f)
  (ps-lisp:symbol-value f))

(define (set-fluid! f val)
  (ps-lisp:set f val))

(define (let-fluid f val thunk)
  (ps-lisp:progv (list f) (list val) (thunk)))

; Tables

(define (make-table)
  ;; Default size in VAX LISP is 71, which seems rather large.
  (ps-lisp:values (ps-lisp:make-hash-table :size 20 :rehash-size 2.0)))

(define (table-set! table key val)
  (ps-lisp:setf (ps-lisp:gethash key table) val))

(define (table-ref table key)
  (ps-lisp:gethash key table #f))

; Pretty-printer used by translator
; Two cases:
;  - If package is scheme-package, then unqualified symbols must print
;    without package prefixes, and qualified ones must print with.
;  - Otherwise, the opposite, and the package prefix for unqualified
;    symbols ought to be 

(define cl-readtable (ps-lisp:copy-readtable 'ps-lisp:nil))

(ps-lisp:defun write-pretty (form port package)
  (ps-lisp:let ((ps-lisp:*package* package)
	     (ps-lisp:*print-case* :upcase)
	     (ps-lisp:*readtable* cl-readtable))
    (ps-lisp:declare (ps-lisp:special cl-readtable))
    (ps-lisp:format port "~&")
    (ps-lisp:write form :stream port
		     :pretty ps-lisp:t
		     :length 'ps-lisp:nil
		     :level 'ps-lisp:nil)
    (ps-lisp:values)))

; Package stuff, etc.

(define (intern-renaming-perhaps string package)
  (ps-lisp:intern (if (eq? package scheme-package)
		      string
		      (perhaps-rename string))
		  package))

(define (perhaps-rename string)  ;Cf. defune in rts.lisp
  (if (or (ps-lisp:multiple-value-bind (sym status) ;Good candidate for caching
	      (ps-lisp:find-symbol string lisp-package)
	      sym ;ignore
	    (eq? status :external))
	  (and (> (string-length string) 0)
	       (char=? (string-ref string 0) #\&)))
      (string-append "." string)
      string))

(define lisp-package (ps-lisp:find-package "PS-LISP"))

(define (qualified-symbol? sym)
  (ps-lisp:if (ps-lisp:symbolp sym)
	      (not (eq? (ps-lisp:symbol-package sym) scheme-package))
	      #f))

(define scheme-package (ps-lisp:symbol-package 'askdjfh))

(define (make-package-using id use-list)
  (let* ((name (symbol->string id))
	 (probe (ps-lisp:find-package name))
	 (package
	  (cond ((not (eq? probe 'ps-lisp:nil))
	         (for-each (lambda (use)
			     (if (not (or (eq? use lisp-package)
					  (memq use use-list)))
				 (ps-lisp:unuse-package use probe)))
			   (ps-lisp:package-use-list probe))
		 probe)
		(else (ps-lisp:make-package name :use use-list)))))
    (ps-lisp:use-package (if (eq? id 'scheme)
			  use-list	;Kludge
			  (cons lisp-package use-list))
		      package)
    package))

(define (make-package-exporting id syms)
  (let* ((name (symbol->string id))
	 (new (ps-lisp:or (ps-lisp:find-package name)
		       (ps-lisp:make-package name :use '()))))
    (ps-lisp:import syms new)
    (ps-lisp:export syms new)
    new))


; ps-lisp:namestring
; ps-lisp:truename
; ps-lisp:merge-pathnames
; ps-lisp:make-pathname
; ps-lisp:package-name

; Etc.

(define (scheme-implementation-version)
  (string-append (#'ps-lisp:lisp-implementation-type)
		 " "
		 (#'ps-lisp:lisp-implementation-version)))

(define (defined-as-CL-macro? CL-sym)
  (ps-lisp:if (ps-lisp:macro-function CL-sym)
	      #t
	      #f))

(define (true-name source-file-name)
  (ps-lisp:namestring (ps-lisp:truename source-file-name)))

(define package-name #'ps-lisp:package-name)

(define intern #'ps-lisp:intern)
