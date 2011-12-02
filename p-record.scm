; -*- Mode: Scheme; Syntax: Scheme; Package: Scheme; -*-
; File record.scm / Copyright (c) 1989 Jonathan Rees / See file COPYING

;;;; Record package for Pseudoscheme

(ps-lisp:defstruct (record-type-descriptor (:constructor make-rtd)
					(:print-function print-rtd)
					(:conc-name "RTD-"))
  identification
  unique-id
  field-names
  constructor-function
  predicate-function
  accessor-functions)

(define *record-type-unique-id* 0)

(define package-for-record-functions
  (ps-lisp:make-package
   (ps-lisp:if (ps-lisp:find-package ".RECORD")
	    (let loop ((n 0))
	      (let ((name (string-append ".RECORD-" (number->string n))))
		(ps-lisp:if (ps-lisp:find-package name)
			 (loop (+ n 1))
			 name)))
	    ".RECORD")
   :use '()))

(define (really-make-record-type type-id field-names)
  (let* ((conc
	  (lambda things
	    (ps-lisp:intern
	     (apply string-append
		    (map (lambda (thing)
			   (cond ((string? thing) thing)
				 ((number? thing)
				  (number->string thing))
				 ((symbol? thing)
				  (ps-lisp:symbol-name thing))
				 (else "?")))
			 things))
	     package-for-record-functions)))
	 (id-symbol
	  (conc type-id "#" *record-type-unique-id*))
	 (constructor-function
	  (conc 'make- id-symbol))
	 (predicate-function
	  (conc id-symbol '?))
	 (accessor-functions
	  (map (lambda (f)
		    (conc id-symbol '- f))
	       field-names))
	 (rtd (make-rtd :identification type-id
			:unique-id *record-type-unique-id*
			:field-names field-names
			:constructor-function constructor-function
			:predicate-function predicate-function
			:accessor-functions accessor-functions)))
    (ps-lisp:setf (ps-lisp:get id-symbol 'rtd) rtd)
    (let ((ps-lisp:*package* package-for-record-functions))
      ;; Careful -- :CONC-NAME NIL doesn't mean defstruct won't try to
      ;; intern new symbols in current package!
      (ps-lisp:eval `(ps-lisp:defstruct (,id-symbol
				   (:constructor ,constructor-function ())
				   (:print-function ,(ps-lisp:quote print-record))
				   (:predicate ,predicate-function)
				   (:copier ps-lisp:nil)
				   (:conc-name ps-lisp:nil))
		    ,@accessor-functions)))
    (set! *record-type-unique-id* (+ *record-type-unique-id* 1))
    rtd))

(define (record-constructor rtd . init-names-option)
  (let ((cfun (rtd-constructor-function rtd))
	(funs (map (lambda (name)
		     (rtd-accessor-function rtd name))
		   (if (null? init-names-option)
		       (rtd-field-names rtd)
		       (car init-names-option)))))
    (ps-lisp:unless (ps-lisp:compiled-function-p (ps-lisp:symbol-function cfun))
		 (ps-lisp:compile cfun))
    (ps-lisp:compile 'ps-lisp:nil
		  `(ps-lisp:lambda ,funs
		     (ps-lisp:let ((the-record (,cfun)))
		       ,@(map (lambda (fun)
				`(ps-lisp:setf (,fun the-record)
					    ,fun))
			      funs)
		       the-record)))))

(define (record-predicate rtd)
  (let ((fun (rtd-predicate-function rtd)))
;    (ps-lisp:unless (ps-lisp:compiled-function-p (ps-lisp:symbol-function fun))
;                 (ps-lisp:compile fun))
;    (ps-lisp:symbol-function fun)
    (ps-lisp:compile 'ps-lisp:nil
		  `(ps-lisp:lambda (x)
		     (ps-lisp:if (,fun x) ,#t ,#f))))) ;bootstrap subtlety

(define (record-accessor rtd name)
  (let ((fun (rtd-accessor-function rtd name)))
    (ps-lisp:unless (ps-lisp:compiled-function-p (ps-lisp:symbol-function fun))
		 (ps-lisp:compile fun))
    (ps-lisp:symbol-function fun)))

(define (record-modifier rtd name)
  (let ((fun (rtd-accessor-function rtd name)))
    (ps-lisp:compile 'ps-lisp:nil `(ps-lisp:lambda (x y)
			       (ps-lisp:setf (,fun x) y)))))

(define (rtd-accessor-function rtd name)
  (let loop ((l (rtd-field-names rtd))
	     (a (rtd-accessor-functions rtd)))
    (if (null? l)
	(ps-lisp:error "~S is not a field name for ~S records"
		    name
		    (rtd-identification rtd))
	(if (eq? name (car l))
	    (car a)
	    (loop (cdr l) (cdr a))))))

; make-record-type:

(define record-type-table (ps-lisp:make-hash-table :test 'ps-lisp:equal))

(define (make-record-type type-id field-names)
  (let* ((key (cons type-id field-names))
	 (existing (ps-lisp:gethash key record-type-table)))
    (if (and (not (eq? existing 'ps-lisp:nil))
	     (begin ; Harlequin doesn't like ~&
		    (common-lisp:fresh-line ps-lisp:*query-io*)
		    (ps-lisp:format ps-lisp:*query-io*
				    "Existing ~S has fields ~S.~%"
				    existing
				    field-names)
		    (not (eq?
			  (ps-lisp:y-or-n-p
			   "Use that descriptor (instead of creating a new one)? ")
			  'ps-lisp:nil))))
	existing
	(let ((new (really-make-record-type type-id field-names)))
	  (ps-lisp:setf (ps-lisp:gethash key record-type-table) new)
	  new))))

(define (record-type record)
  (ps-lisp:get (ps-lisp:type-of record) 'rtd))

; Printing

(define (print-rtd rtd stream escape?)
  escape? ;ignored
  (ps-lisp:format stream
	       "#{Record-type-descriptor ~S.~S}"
	       (rtd-identification rtd)
	       (rtd-unique-id rtd)))

(define (print-record record stream escape?)
  escape?				;ignored
  (let ((d (disclose-record record)))
    (display "#{" stream)
    (display (if (symbol? (car d))
		     (ps-lisp:string-capitalize (symbol->string (car d)))
		     (car d))
	     stream)
    (for-each (lambda (x)
		(write-char #\space stream)
		(write x stream))
	      (cdr d))
    (display "}" stream)))

(define record-disclosers (ps-lisp:make-hash-table))

(define (disclose-record record)
  ((ps-lisp:gethash (record-type record)
		    record-disclosers
		    default-record-discloser)
   record))

(define (default-record-discloser record)
  (list (rtd-identification (record-type record))))

(define (define-record-discloser rtd proc)
  (ps-lisp:setf (ps-lisp:gethash rtd record-disclosers) proc))
