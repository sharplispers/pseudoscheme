;;; -*- Mode: LISP; Syntax: ANSI-Common-Lisp; Package: CL-USER -*-
;;;
;;; Load Pseudoscheme.

(in-package "CL-USER")

(defvar pseudoscheme-directory
  (merge-pathnames "pseudo\\" load-utils:*support-directory*))

(load (merge-pathnames "loadit.lisp" pseudoscheme-directory))
(with-compilation-unit ()
  (load-pseudoscheme))
(ps:benchmark-mode)

; Use Common Lisp reader so that CL package prefix notation works in Scheme.
(setq ps:*scheme-read* #'ps:scheme-read-using-commonlisp-reader)

; See file pseudo/foo.lisp for suggestions on other things to load
; & how to load them (records, etc.).

(setf (get :scheme 'load-utils:load-file)
      #'(lambda (merged-path &key (compile load-utils:*compile-p*))
	  (ps:scheme-load 
	   merged-path
	   (revised^4-scheme:interaction-environment)
	   :compile-if-necessary compile)))

(with-compilation-unit ()

#+LispWorks
(eval-when (:execute :compile-toplevel :load-toplevel)
  (require "comm")) ;TCP support needed for sockets

(mapc #'(lambda (filename)
	  (ps:scheme-load (merge-pathnames filename pseudoscheme-directory)
			  (revised^4-scheme:interaction-environment)
			  :compile-if-necessary load-utils:*compile-p*))
      '("pseudoscheme-record"
	"pseudoscheme-features"
	"s48-socket" ;"socket"
	"s48-table"
	"s48-threads"))

(let ((ps:*scheme-read* revised^4-scheme::scheme-read))
  (ps:scheme-load (merge-pathnames "jar-defrecord" pseudoscheme-directory)
		  (revised^4-scheme:interaction-environment)))

)
