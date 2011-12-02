; -*- Mode: Scheme; Syntax: Scheme; Package: Scheme; -*-
; File module.scm / See file COPYING

;;;; Interfaces, program environments, structures

; Interfaces

(define interface-rtd
  (make-record-type 'interface '(id names aux-names)))

(define make-interface
  (record-constructor interface-rtd '(id names aux-names)))

(define interface-names (record-accessor interface-rtd 'names))
(define interface-aux-names (record-accessor interface-rtd 'aux-names))

; INTERFACE-REF returns one of
;   #F       if the name is not exported
;   PUBLIC   if exported as a value
;   PRIVATE  if exported as an auxiliary value

;+++ This can be slow if SIG exports many variables (as does the r^4
; interface).  If this becomes a problem, change it so that it does a
; table lookup (after some threshold size?).

(define (interface-ref sig name)
  (cond ((memq name (interface-names sig)) 'public)
	(else #f)))

(define (interface-ref-aux sig name)
  (cond ((memq name (interface-names sig)) 'public)
	((memq name (interface-aux-names sig)) 'private)
	(else #f)))


; Program (i.e. top-level) environments contain macro definitions.

(define program-env-rtd
  (make-record-type 'program-env '(id use-list table package)))
(define program-env-id       (record-accessor program-env-rtd 'id))
(define program-env-use-list (record-accessor program-env-rtd 'use-list))
(define program-env-table    (record-accessor program-env-rtd 'table))
(define program-env-package  (record-accessor program-env-rtd 'package))
(define program-env? (record-predicate program-env-rtd))

(define make-program-env
  (let ((create (record-constructor program-env-rtd
				    '(id use-list table package))))
    (lambda (id use-list)
      (let ((env
	     (create id
		     use-list
		     (make-table)
		     (make-package-using id (map structure-package use-list)))))
	(init-environment-for-syntax! env)
	env))))

(define-record-discloser program-env-rtd
  (lambda (r) (list "Program-env" (program-env-id r))))

; Careful, name need not be a symbol

(define (program-env-lookup program-env name)
  (or (table-ref (program-env-table program-env) name)
      (program-env-new-variable program-env name)))

(define (program-env-define! program-env name binding)
  (table-set! (program-env-table program-env) name binding))

(define (program-env-ensure-defined program-env name)
  (let ((probe (table-ref (program-env-table program-env) name)))
    (if (and (node? probe)
	     (program-variable? probe))
	probe
	(program-env-new-variable program-env name))))

(define (program-env-new-variable program-env name)
  (let ((q? (and (symbol? name)
		 (qualified-symbol? name))))
    (or (and (not q?)
	     (let loop ((mods (program-env-use-list program-env)))
	       (and (not (null? mods))
		    (or (structure-ref (car mods) name)
			(loop (cdr mods))))))
	;; SIDE EFFECT!  Not so good.
	(let ((node (make-program-variable
		     name
		     (if q?
			 name
			 (intern-renaming-perhaps
			  (name->string name)
			  (program-env-package program-env))))))
	  (table-set! (program-env-table program-env) name node)
	  node))))

(define client-lookup program-env-lookup)   ;for classify
(define client-define! program-env-define!) ;for classify
(define client-ensure-defined program-env-ensure-defined)


; Get the environment in which to evaluate transformer procedure expressions.

(define environment-for-syntax-key
  (list 'environment-for-syntax-key))  ;any unique id

(define (get-environment-for-syntax env)
  (force (lookup env environment-for-syntax-key)))

(define (define-transformer-env! env t-env-promise)
  (define! env environment-for-syntax-key t-env-promise))

(define (init-environment-for-syntax! env)
  (define-transformer-env! env
    (delay (make-program-env
	    (string->symbol
	     (string-append (symbol->string (program-env-id env))
			    "[FOR-SYNTAX]"))
	    (list revised^4-scheme-structure)))))


; A structure is a pair <interface, program-environment>.
; Pavel Curtis would prefer to call these things "interfaces".

(define structure-rtd
  (make-record-type 'structure '(id sig program-env package)))
(define make-structure
  (let ((create
	 (record-constructor structure-rtd '(id sig program-env package))))
    (lambda (id sig env)
      (create id sig env
	      (make-package-exporting
	           id
		   (let ((ppackage (program-env-package env)))
		     (map (lambda (name)
			    (intern-renaming-perhaps
		                 (symbol->string name)
				 ppackage))
			  (interface-names sig))))))))


(define structure-id        (record-accessor structure-rtd 'id))
(define structure-interface (record-accessor structure-rtd 'sig))
(define structure-program-env (record-accessor structure-rtd 'program-env))
(define structure-package   (record-accessor structure-rtd 'package))

(define-record-discloser structure-rtd
  (lambda (r) (list "Structure" (structure-id r))))

(define (structure-ref mod name)
  (if (eq? (interface-ref (structure-interface mod) name)
	   'public)
      (program-env-lookup (structure-program-env mod) name)
      #f))
