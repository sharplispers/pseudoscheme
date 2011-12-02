; -*- Mode: Lisp; Syntax: Common-Lisp; Package: CLEVER-LOAD; -*-
; File clever.lisp / See file COPYING

; This is really too clever for its own good.  Should phase it out.

(in-package "CLEVER-LOAD")

(export '(clever-load
	  *compile-if-necessary-p*))

(eval-when (eval load compile)
  (when (find-if #'(lambda (feature)
		     (and (symbolp feature)
			  (string= (symbol-name feature) "DEC")))
		 *features*)
    (pushnew ':DEC *features*)))

(eval-when (eval load compile)
  (when (find-if #'(lambda (feature)
		     (and (symbolp feature)
			  (string= (symbol-name feature) "VMS")))
		 *features*)
    (pushnew ':VMS *features*)))

; File loader

#|| ;something like this ought to work...
(defconstant local-file-type
  (macrolet ((my-file-type () `',(pathname-type *load-truename*)))
    (my-file-type)))
||#

(defun source-file-type (pathname)
  pathname
  (or #+Symbolics (car (zl:send pathname
				':types-for-canonical-type
				':lisp))
      #+(and :DEC :Ultrix) "lsp"
      #+:VMS "LSP"
      #+:ccl "LISP"			;Coral
      #+allegro "lisp" ;or cl ... hmm.
      "lisp"				;For Unix, Exploder, and anyone else
      ))

(defun object-file-type (pathname)
  pathname
  (or #+Symbolics (car (zl:send pathname
				':types-for-canonical-type
				si:*default-binary-file-type*))
      #+Explorer  "xld"
      #+(and :DEC :Ultrix) "fas"
      #+(and :DEC :VMS) "FAS"
      #+Lucid (car lucid::*load-binary-pathname-types*)  ;?
      #+KCL "o"
      #+:ccl "FASL"			;Coral
      #+LispWorks "fsl"
      #+allegro "fasl"
      #+(and cmu hpux) "hpf"
      ))   ;(or) => nil otherwise

(defvar *compile-if-necessary-p* nil)

(defvar *debug* nil)

(defun clever-load (filespec &rest keys
			     &key source-type
				  object-type
				  (compile-if-necessary
				       *compile-if-necessary-p*)
				  (verbose :not-very)
				  (message "")
			     &allow-other-keys)
  (let* ((path (merge-pathnames (if (symbolp filespec)
				    (symbol-name filespec)
				    filespec)
				(make-pathname :type nil
					       :defaults *default-pathname-defaults*)))
	 (source-type (or source-type (source-file-type path)))
	 (object-type (or object-type (object-file-type path))))
    (when *debug*
      (format *debug-io*
	      "~&Clever-load: path = ~s, keys = ~s~%"
	      path keys))
    (flet ((load-it (path)
	     (apply #'load
		    path
		    :verbose (cond ((eq verbose :not-very)
				    (format t "~&Loading ~A ~A~%"
					    (namestring path)
					    message)
				    nil)
				   (t
				    (format t "~&Loading ~A~%"
					    message)
				    verbose))
		    :allow-other-keys t
		    keys))
	   (compile-it (src obj)
	     (apply #'compile-file src
		    :output-file obj
		    #+:DEC :listing #+:DEC t
		    :allow-other-keys t
		    keys)))
	(cond ((and (pathname-type path)	;No ifs, ands, or buts
		    (not (eq (pathname-type path) :unspecific)))
	       (when *debug*
		 (format *debug-io*
			 "~&Pathname has a type - ~S~%"
			 (pathname-type path)))
	       (load-it (truename path)))
	      ((or (not source-type) (not object-type))
	       (when *debug*
		 (format *debug-io*
			 "~&No known source or object type~%"))
	       (when compile-if-necessary
		 (cerror "Load file ~S without checking to see whether ~
			  it needs to be compiled."
			 "CLEVER-LOAD improperly configured -- it doesn't ~
			  have necessary file type information."
			 (namestring path)))
	       (load-it path))
	      (t
	       (let* ((src (make-pathname :type source-type
					  :defaults path))
		      (src? (probe-file src))
		      (obj (make-pathname :type object-type
					  :defaults path))
		      (obj? (probe-file obj)))
		 (cond ((not src?)
			(warn "~A not found, attempting to load ~A."
			      (namestring src) (namestring obj))
			(load-it (or obj? obj)))
		       ((not obj?)
			(cond (compile-if-necessary
			       (when *debug*
				 (format *debug-io*
					 "~&No object.  Compiling ~s to ~s.~%"
					 src obj))
			       (compile-it src obj)
			       (load-it obj))
			      (t
			       (when *debug*
				 (format *debug-io*
					 "~&No object.  Loading source ~s.~%"
					 src))
			       (load-it src?))))
		       ((let ((obj-date (file-write-date obj?))
			      (src-date (file-write-date src?)))
			  (or (not obj-date)
			      (not src-date)
			      (>= obj-date src-date)))
			(when *debug*
			  (format *debug-io*
				  "~&Object up to date.  Loading ~s.~%"
				  obj))
			(load-it obj?))
		       (compile-if-necessary
			(when *debug*
			  (format *debug-io*
				 "~&Object out of date.  Compiling ~s to ~s.~%"
				  src obj))
			(compile-it src obj)
			(load-it obj))
		       (t
			(format *error-output*
				"~&There is an object file ~A,~
				 ~%but loading source because it's newer.~%"
				(namestring obj?))
			(load-it src?)))))))))

