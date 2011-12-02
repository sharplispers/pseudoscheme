; -*- Mode: Lisp; Syntax: Common-Lisp; Package: PS; -*-
; File readtable.lisp / See file COPYING

;;;; Scheme READ and WRITE

; Fudged, using a Common Lisp readtable.

(in-package "PS")

(defparameter scheme-readtable (copy-readtable nil))

(defun scheme-read-using-commonlisp-reader (port)
  (let ((*package* scheme-package)
	(*readtable* scheme-readtable))
    (read-preserving-whitespace port nil ps:eof-object)))

(setq ps:*scheme-read* #'scheme-read-using-commonlisp-reader)  ;Yow

(defun scheme-write-using-commonlisp-printer (obj port)
  (write-internal obj port t))

(defun scheme-display-using-commonlisp-printer (obj port)
  (write-internal obj port nil))

(setq ps:*scheme-write* #'scheme-write-using-commonlisp-printer)
(setq ps:*scheme-display* #'scheme-display-using-commonlisp-printer)

(defvar *non-scheme-readtable* (copy-readtable nil))

#+Symbolics
(pushnew scheme-readtable si:*valid-readtables*)

(defun quote-read-macro (stream c)
  (if (eq *package* scheme-package)
      (list (intern "QUOTE" scheme-package) (read stream t nil t))
      (funcall (get-macro-character #\' *non-scheme-readtable*) stream c)))

(defun quasiquote-read-macro (stream c)
  (if (eq *package* scheme-package)
      (list (intern "QUASIQUOTE" scheme-package) 
	    (read stream t nil t))
      (funcall (get-macro-character #\` *non-scheme-readtable*) stream c)))

(defun unquote-read-macro (stream c)
  (if (eq *package* scheme-package)
      (let* ((following-char (peek-char nil stream t nil t))
	     (marker (cond ((char= following-char #\@)
			    (read-char stream)
			    (intern "UNQUOTE-SPLICING" scheme-package))
			   (t
			    (intern "UNQUOTE" scheme-package)))))
	(list marker (read stream t nil t)))
      (funcall (get-macro-character #\, *non-scheme-readtable*) stream c)))

(defun sharp-F-read-macro (stream subchar arg)
  (declare (ignore stream subchar arg))
  ps:false)

(defun sharp-T-read-macro (stream subchar arg)
  (declare (ignore stream subchar arg))
  ps:true)

(defun sharp-D-read-macro (stream subchar arg)
  (declare (ignore subchar arg))
  (let ((*read-base* 10.))
    (read stream t nil t)))

(defun sharp-E-read-macro (stream subchar arg)
  (declare (ignore subchar arg))
  (let ((n (read stream t nil t)))
    (if (rationalp n)
	n
	(rationalize n))))

(defun sharp-I-read-macro (stream subchar arg)
  (declare (ignore subchar arg))
  (let ((n (read stream t nil t)))
    (if (floatp n)
	n
	(float n))))

(defvar *sharp-sharp* '(values-list /))

(defun sharp-sharp-read-macro (stream subchar arg)
  (cond (arg (funcall (get-dispatch-macro-character #\# #\#
						    *non-scheme-readtable*)
		      stream subchar arg))
	(t *sharp-sharp*)))

(defun illegal-read-macro (stream c)
  (unread-char c stream)		;won't work in general
  (when (eq *package* scheme-package)
    (cerror "Try to treat it as Common Lisp would."
	    "The character `~A' was encountered."
	    c))
  (let ((*readtable* *non-scheme-readtable*))
    (read stream nil 0 t)))

(let ((*readtable* scheme-readtable))
  (set-macro-character #\' #'quote-read-macro)
  (set-macro-character #\` #'quasiquote-read-macro)
  (set-macro-character #\, #'unquote-read-macro)
  (set-dispatch-macro-character #\# #\F #'sharp-F-read-macro)
  (set-dispatch-macro-character #\# #\T #'sharp-T-read-macro)
  (set-dispatch-macro-character #\# #\D #'sharp-D-read-macro)
  (set-dispatch-macro-character #\# #\E #'sharp-E-read-macro)
  (set-dispatch-macro-character #\# #\I #'sharp-I-read-macro)
  (set-dispatch-macro-character #\# #\# #'sharp-sharp-read-macro)
  ;; Don't mess with backslash, or strings will bite you.
  (mapc #'(lambda (c)
	    (set-macro-character c #'illegal-read-macro t))
	'(#\[ #\] #\{ #\} #\|)))



(defun write-internal (obj port escapep)
  (let ((*package* ps:scheme-package)
	(*readtable* ps:scheme-readtable))
    (cond ((null obj)
	   (princ "()" port))
	  ((eq obj ps:false)
	   (write-char #\# port)
	   ;; Respect *print-case*
	   (let ((*package* (symbol-package 'f)))
	     (prin1 'f port)))
	  ((eq obj ps:true)
	   (write-char #\# port)
	   ;; Respect *print-case*
	   (let ((*package* (symbol-package 't)))
	     (prin1 't port)))
	  ((and (consp obj)
		(eq (car obj) 'scheme::quote)
		(consp (cdr obj))
		(null (cddr obj)))
	   (write-char #\' port)
	   (write (cadr obj) :stream port :escape escapep :array t))
	  (t
	   (write obj :stream port :escape escapep :array t)))
    ps:unspecific))
