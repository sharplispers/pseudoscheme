; -*- Mode: Lisp; Syntax: Common-Lisp; Package: CL-USER; -*-
; Copyright (c) 1991-1994 Jonathan Rees / See file COPYING

;;;; Load script

; Load this into any package that inherits the usual culprits (DEFUN, etc.).

; Will not run in:
;  Symbolics versions older than Rel 7.1
;  VAX LISP versions older than V2.2
;  Explorer versions older than 3.0


; These are my personal favorite settings; you may want to change them.

;(proclaim '(optimize (speed 3)
;		     #-LispWorks (safety 2)
;		     #+LispWorks (safety 3)  ;sux
;		     (compilation-speed 0)))


(defvar *pseudoscheme-directory* 
  (make-pathname :name nil :type nil :version nil
		 :defaults *load-truename*))
(defvar *use-scheme-read* t)
(defvar *use-scheme-write* t)

(defun load-pseudoscheme (&optional (dir *pseudoscheme-directory*))
  (designate-pseudoscheme-directory dir)
  (load (pseudo-pathname "pack")) ; Create packages
  (load-clever-loader)
  (load-pseudoscheme-run-time)
  ;; Note that if you're only going to run a program that's already been
  ;; compiled and/or fed through the translator, you can omit the following
  ;; two steps.
  (load-pseudoscheme-translator)
  (load-pseudoscheme-eval))

(defun designate-pseudoscheme-directory (dir)
  (setq *pseudoscheme-directory*
	(let ((dir (pathname (or dir
				 *default-pathname-defaults*))))
	  (make-pathname :name nil
			 :type nil
			 :directory (pathname-directory dir)
			 :device    (pathname-device dir)
			 :host	    (pathname-host dir)))))

(defun pseudo-pathname (name)
  (make-pathname :name (filename-preferred-case name)
		 :defaults *pseudoscheme-directory*))

; These two definitions also occur in core.lisp:
(defun filename-preferred-case (name)
  #+unix (string-downcase name)
  #-unix (string-upcase name)
  )
; PSO stands for Pseudo-Scheme Object file
(defvar *translated-file-type* (filename-preferred-case "pso"))

; ----- Load silly file loader

(defvar clever-load)
(defun clever-load (&rest foo) (apply clever-load foo))

(defun load-clever-loader ()
  (load (pseudo-pathname "clever")) ;Get clever file loader
  (setq clever-load
	(symbol-function (intern "CLEVER-LOAD"
				 (find-package "CLEVER-LOAD")))))

; ----- Load Scheme run-time system

(defvar revised^4-scheme-package)

(defun load-pseudoscheme-run-time ()

  (load-pseudoscheme-run-time-file "core")

  (load (pseudo-pathname "spack")) ; Create more packages

  (setq revised^4-scheme-package
	(find-package "REVISED^4-SCHEME"))

  ;; This sets up the revised^4 scheme package, among other things
  (load-pseudoscheme-translated "closed" revised^4-scheme-package)

  ;; This loads into the revised^4 scheme package
  (load-pseudoscheme-run-time-file "rts")

  (load-pseudoscheme-run-time-file "readwrite")

  ;; read and write are optional.  Loading "read" gets you ... and
  ;; symbols with colons in their names.
  ;; Loading "write" gets you these plus (), #t, and #f.
  ;; The downside of using the Scheme reader is that it becomes nearly
  ;; impossible to use Common Lisp functions, variables, and symbols
  ;; from Scheme code.
  (if *use-scheme-read*
      (load-pseudoscheme-translated "read" revised^4-scheme-package))
  (if *use-scheme-write*
      (load-pseudoscheme-translated "write" revised^4-scheme-package))
  'done)

; ----- Load translator

(defparameter scheme-translator-package nil)
(defparameter translator-files nil)

(defun load-pseudoscheme-translator ()
  (setq scheme-translator-package
	(find-package "SCHEME-TRANSLATOR"))
  (setq translator-files
	(with-open-file (s (pseudo-pathname "translator.files"))
	  (read s)))
  (mapc #'(lambda (file)
	    (load-pseudoscheme-translated file scheme-translator-package))
	translator-files)
  'done)

(defun load-pseudoscheme-eval ()
  (load-pseudoscheme-run-time-file "eval")
  #+Lispm
  (load-pseudoscheme-run-time-file "custom"))



(defun load-pseudoscheme-run-time-file (filespec)
  (clever-load (pseudo-pathname (if (consp filespec)
				    (car filespec)
				    filespec))
	       :compile-if-necessary (not (consp filespec))))

(defun load-pseudoscheme-translated (file package)
  (declare (ignore package))    ;no longer used
  (clever-load (pseudo-pathname file)
	       :source-type *translated-file-type*
	       :compile-if-necessary t))


; Cope with vagaries of #+ in DEC's VAX LISP

(eval-when (eval load compile)
  (when (find-if #'(lambda (feature)
		     (and (symbolp feature)
			  (string= (symbol-name feature) "DEC")))
		 *features*)
    (pushnew ':DEC *features*)))
