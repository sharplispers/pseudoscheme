; -*- Mode: Lisp; Syntax: Common-Lisp; Package: SCHEME-PURIFY; -*-
; File hacks.lisp / See file COPYING

(in-package "SCHEME-PURIFY")


; ----- Fix the SCHEME package, if necessary.  The home package of any
; symbol in the SCHEME package must be SCHEME so that Scheme symbols
; print as SCHEME::FOO when the Scheme package is not current.

(defun fix-scheme-package-if-necessary (package)
  (if (not (equal (package-name package) "SCHEME"))
      (rename-package package "SCHEME"))
  (cond ((pollutedp package)
	 (purify-scheme-package package))))

(defun pollutedp (package)
  (do-symbols (sym package)
    (when (not (eq (symbol-package sym) package))
      (return-from pollutedp t))))

; Things about whose EQ-ness we care:

(defparameter losers
  '("DEFINE"
    "ELSE" "=>" "UNQUOTE" "UNQUOTE-SPLICING"
    "HEUR" "B" "O" "D" "X"))

(defparameter ps-package (find-package "PS-LISP"))

(defun purify-scheme-package (package)
  (format t "~&Purifying...")
  ;; It shouldn't be necessary to bind *package* here, but it turns
  ;; out to be the way to work around some obscure Symbolics bug.
  (let ((*package* package))
    (let ((winners (mapcar #'(lambda (name)
			       (intern name package))
			   losers)))
      (unuse-package (package-use-list package) package)
      (import winners package)
      (do-symbols (sym package)
	(cond ((eq (symbol-package sym) package)
	       (unexport sym package)
	       ;; OK, do nothing.
	       )
	      ((eq sym (find-symbol (symbol-name sym) ps-package))
	       (let ((name (symbol-name sym)))
		 (if (member name losers :test #'string=)
		     (error "~S shouldn't be accessible in the LISP package, but it is."
			    sym))
		 (unintern sym package)
		 (let ((new-sym (intern name package)))
		   (assert (eq (symbol-package new-sym) package)
			   () "Lost on ~S" new-sym)
		   (symbol-forward sym new-sym))))
	      (t
	       (purify-symbol sym package)))))))

; Clobber the symbol's home package so that it prints
; as SCHEME::FOO.
(defun purify-symbol (sym package)
  (unexport sym package)
  (let ((name (symbol-name sym))
	(old-package (symbol-package sym)))
    (format t " ~S" sym)
    (unexport sym old-package)
    (unintern sym old-package)			;?
    (import sym package)
    #+Lispm					;?
    (setf (symbol-package sym) package)
    (multiple-value-bind (hucairz status)
	(find-symbol name old-package)
      (declare (ignore hucairz))
      (unless status	;inherited
	(import sym old-package)))
    (unless (and (eq sym (find-symbol name package))
		 (eq (symbol-package sym) package))
      (format t "~& (Failed to move ~S to ~A package)~%"
	      sym
	      (package-name package)))))

(defun symbol-forward (from-sym to-sym)
  (when (boundp from-sym)
    (setf (symbol-value to-sym) (symbol-value from-sym))
    (proclaim `(special ,to-sym)))
  (cond ((or (special-form-p from-sym)
	     (macro-function from-sym))
	 (setf (macro-function to-sym)
	       #'(lambda (form env)
		   (declare (ignore env))
		   (cons from-sym (cdr form)))))
	((fboundp from-sym)
	 (setf (symbol-function to-sym)
	       (symbol-function from-sym)))))

