; File bootit.scm / -*- Mode: Scheme; Syntax: Scheme; Package: Scheme; -*-
; Copyright (c) 1991-1994 Jonathan Rees / See file COPYING

; Bootstrapping a new Pseudoscheme

; You must do (SETQ *USE-SCHEME-READ* NIL) before loading Pseudoscheme
; in order for this to work!

; In a Scheme-in-Common-Lisp implementation, load this file and do
; (bootit).  This compiles and loads the translator, then invokes
; the translator to translate itself.

; Ultimately it would be nice to be able to boot from a Scheme that's
; not Common-Lisp based, but that would require a fair amount of
; hacking: e.g. all the routines in p-utils.scm would have to be rewritten.

(define *pseudoscheme-directory* #f)

(define (bootit . dir-option)
  (cond ((not (null? dir-option))
	 (set! *pseudoscheme-directory* (lisp:pathname (car dir-option))))
	((not *pseudoscheme-directory*)
	 (set! *pseudoscheme-directory*
	       (lisp:make-pathname :name 'lisp:nil
				   :type 'lisp:nil
				   :defaults
				     lisp:*default-pathname-defaults*))))
  (boot-initialize)
  (load-untranslated-translator)
  (fix-reader-if-necessary)
  (translate-run-time)
  (translate-translator))

(define clever-load #f)

(define (boot-initialize)
  ;; For LISP:LOAD to work, we need for *PACKAGE* to be bound to
  ;; something other than the SCHEME package.  Since all these files start
  ;; with IN-PACKAGE forms, it doesn't matter which package in particular, as
  ;; long as it contains IN-PACKAGE and DEFPACKAGE.
  (lisp:let ((lisp:*package* (or (lisp:find-package "USER")
				 (lisp:make-package "USER"))))

    ;; Create the PS package
    (lisp:load (pseudo-pathname "pack"))

    ;; Get clever file loader
    (lisp:load (pseudo-pathname "clever") :verbose 'lisp:nil)
    (set! clever-load
	  (lisp:symbol-function
	   (lisp:intern "CLEVER-LOAD"
			(lisp:find-package "CLEVER-LOAD"))))

    ;; Fix SCHEME package if necessary
;    (clever-load (pseudo-pathname "purify") :compile-if-necessary #t)
;    (lisp:funcall (lisp:symbol-function
;                   (lisp:intern "FIX-SCHEME-PACKAGE-IF-NECESSARY"
;                                (lisp:find-package "SCHEME-PURIFY")))
;                  (lisp:symbol-package 'askdjfh))
    ))


(define (pseudo-pathname name)
  (lisp:make-pathname :name (filename-preferred-case name)
		      :defaults *pseudoscheme-directory*))

(define (filename-preferred-case name)
  #+unix (lisp:string-downcase name)
  #-unix (lisp:string-upcase name)
  )

(define *scheme-file-type*     (filename-preferred-case "scm"))
(define *translated-file-type* (filename-preferred-case "pso"))
(define *boot-file-type*       (filename-preferred-case "boot"))

; Make sure the host system understands that files foo.boot are
; compiled.

#+Lucid
(if (not (member *boot-file-type*
		 lucid::*load-binary-pathname-types*))
    (lisp:setq lucid::*load-binary-pathname-types*
	       (append lucid::*load-binary-pathname-types*
		       (list *boot-file-type*))))

#+Symbolics
(begin
  (fs:define-canonical-type :boot-bin #,*boot-file-type*)

  (lisp:setq fs:*auxiliary-loadable-file-types*
	(cons '(:boot-bin :load-stream-function
			  si:load-binary-file-internal)
	      (lisp:remove :boot-bin fs:*auxiliary-loadable-file-types*
			   :key #'car)))

  (lisp:setf (lisp:get :boot-bin :binary-file-byte-size)
	     (lisp:get :bin :binary-file-byte-size)))

(define translator-files #f)

; ----- Load the translator into Scheme

(define (load-untranslated-translator)
  ;; Make sure we perform integrations!
  (lisp:if (lisp:fboundp 'benchmark-mode)
	   (benchmark-mode))
  (set! translator-files
	(call-with-input-file (pseudo-pathname "translator.files") read))
  (for-each load-scheme translator-files)
  'done)

(define (load-scheme file)
  (clever-load (pseudo-pathname file)
	       :source-type *scheme-file-type*
	       :object-type *boot-file-type*
	       :compile-if-necessary #t))

; ----- Translating the run-time system

(define (translate-run-time)
  ;; In principle, there could be more stuff here.
  (write-closed-definitions
     revised^4-scheme-structure
     (lisp:make-pathname :type *translated-file-type*
			 :defaults (pseudo-pathname "closed")))
  (for-each (lambda (f)
	      (translate-a-file f revised^4-scheme-env))
	    '(;; These are both optional.  Cf. load-run-time in loadit.scm.
	      "read"
	      "write"
	      )))

; ----- Translating the translator

(define (translate-translator)
  (let ((env (make-program-env 'scheme-translator
			       (list revised^4-scheme-structure))))
    (for-each (lambda (f)
		(translate-a-file f env))
	      translator-files)

    (write-defpackages (list revised^4-scheme-structure
			     scheme-translator-structure)
		       "spack.lisp")
    'done))

(define (translate-a-file f env)
  (let ((f (pseudo-pathname f)))
    (really-translate-file
     (lisp:make-pathname :type *scheme-file-type* :defaults f)
     (lisp:make-pathname :type *translated-file-type* :defaults f)
     env)))


; Make sure that quote and backquote read in properly.  Careful, this
; may cause them to stop working in the Scheme from which we're
; bootstrapping.  It should be done after the translator is loaded,
; but before the translator starts to read any files.

; This is probably no longer needed.

(define (fix-reader-if-necessary)
  (if (not (eq? (car ''foo) 'quote))
      (lisp:set-macro-character
        #\'
	(lambda (stream c)
	  (list ''quote (lisp:read stream 'lisp:t 'lisp:nil 'lisp:t)))))
  (if (not (eq? (car '`(foo)) 'quasiquote))
      (begin (lisp:set-macro-character
	      #\`
	      (lambda (stream c)
		(list ''quasiquote
		      (lisp:read stream 'lisp:t 'lisp:nil 'lisp:t))))
	     (lisp:set-macro-character
	      #\,
	      (lambda (stream c)
		(let* ((following-char
			(lisp:peek-char 'lisp:nil stream
					'lisp:t 'lisp:nil 'lisp:t))
		       (marker (cond ((char=? following-char #\@)
				      (lisp:read-char stream)
				      'unquote-splicing)
				     (else
				      'unquote))))
		  (list marker
			(lisp:read stream 'lisp:t 'lisp:nil 'lisp:t))))))))
