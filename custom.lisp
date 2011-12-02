; -*- Mode: Lisp; Syntax: Common-Lisp; Package: PS; -*-
; File custom.lisp / See file COPYING

;;;; Customizations for particular Common Lisp implementations

(in-package "PS")

#+Symbolics
; ASSUME REL 7 OR AFTER

(progn

; Meta-. stuff adapted from OZ:<DAM.PROVER>METAP.LISP.2.  Thanks to
; DAM for figuring this out.

; The value of the property ZWEI:DEFINITION-FUNCTION-SPEC-FINDER
; should be a function which takes the ZWEI point after the definition
; symbol (e.g. DEFINE) and returns the point at the begining of the fspec.
; See the function ZWEI:GET-DEFINITION-FUNCTION-SPEC
;
; DEFINE-FSPEC-FINDER goes forward to the begining of the next atom.
; This means skipping white space and left parenthesis.

(defun define-fspec-finder (bp)
  (zwei:forward-over (cons #\(  zwei:*whitespace-chars*)
		     bp))

(mapc #'(lambda (definer)
	  (setf (get definer 'zwei:definition-function-spec-finder)
		#'define-fspec-finder)
	  (setf (get definer 'zwei:definition-function-spec-type)
		'defun))
      '(scheme::define
	scheme::define-macro))

(let ((type *scheme-file-type*))
  (cond ((not (member type fs:*its-uninteresting-types* :test #'equal))
	 (push type fs:*its-uninteresting-types*))))

(fs:define-canonical-type :scheme #,*scheme-file-type*) ;Scheme source

; Default mode for scheme source is lisp.

(unless (assoc :scheme fs:*file-type-mode-alist*)
  (setq fs:*file-type-mode-alist*
	(append fs:*file-type-mode-alist* (list (cons :scheme :lisp)))))

; Allow one to write  -*- Syntax: Scheme; -*-
; (Thanks to Alan Bawden and Thomas A. Russ.)

(defun (:scheme fs:syntax-attribute-handler) ()
  (values (list 'zl:readtable) (list roadblock-readtable)))

(si:define-lisp-syntax :scheme (:readtable-place roadblock-readtable  ;???
				:external-name "Scheme"
				:packages-must-use (("SCHEME")))
  (zl:ferror "Cannot Set Lisp Context to Scheme.  Call PS:SCHEME instead."))

; The following allows one to write -*- Mode: Scheme; -*-

(zl:defflavor scheme-mode () (zwei:lisp-syntax-mode-forms-mixin
				   zwei:lisp-language-mixin
				   zwei:major-mode))

(zl:defmethod (:mode-line-name scheme-mode) ()
  "Scheme")

(zl:defmethod (:get-default-attribute scheme-mode :base) ()
  10)

(zl:defmethod (:get-default-attribute scheme-mode :syntax) ()
  :scheme)

(zl:defmethod (:mode-forms scheme-mode) ()
  '((zwei:set-syntax-table-indirection zwei:*mode-list-syntax-table*
				       zwei:*cl-list-syntax-table*)
    (zwei:set-comtab zwei:*mode-comtab*
		     '(#\Meta-Z         zwei:com-compile-and-exit
		       #\Control-Meta-Z zwei:com-evaluate-and-exit
		       #\Meta-Q		zwei:com-fill-long-comment))))

(zl:defmethod (:eval-print-function scheme-mode) ()
  #'scheme-evaluate-and-print)

(defun scheme-evaluate-and-print (object)
  (declare (special zwei:*use-typeout*))
  (let ((val (roadblock-eval object
			     (get-file-context si:fdefine-file-pathname
					       nil
					       "Evaluating"))))
    (if zwei:*use-typeout*
	(write-result val)
	(zwei:typein-line "~A" (with-output-to-string (stream)
				 (write-result val stream))))
    (values val object)))

(zl:defmethod (:compiler-function scheme-mode) ()
  (zl:let-closed ((compiler:*correspondences* nil))
    #'scheme-compiler-function))

(defun scheme-compiler-function (request-type &rest args)
  (if (and (eq request-type :macro-expand)
	   (consp (car args))
	   (eq (caar args) 'scheme-form))
      (translate-in-context (cadr (car args))
			    (get-file-context si:fdefine-file-pathname
					      nil
					      "Compiling"))
      (apply #'compiler:compile-to-core request-type args)))

(zwei:defmode zwei:com-scheme-mode scheme-mode
  "Sets things up for editing Scheme.
Like Lisp Mode -- if you've only got 8 fingers..."
  :scheme)

; Why is this commented out?  I don't have the courage to test it right now...
;(setq zwei:*lisp-syntax-alist*
;      (cons '("Scheme" :scheme scheme-mode zwei:*cl-list-syntax-table*)
;	    (remove :scheme zwei:*lisp-syntax-alist* :key #'cadr)))

(zwei:set-comtab zwei:*standard-comtab*
		 '()
		 (zwei:make-command-alist '(zwei:com-scheme-mode)))

(zl:defmethod (:default-source-file-type scheme-mode) ()
  :scheme)

(setq fs:*file-type-mode-alist*
      (cons '(:Scheme . :Scheme)
	    (remove-if #'(lambda (x)
			   (or (eq (car x) :Scheme)
			       (eq (cdr x) :Scheme)))
		       fs:*file-type-mode-alist*)))
)  ;(... ngorp) scilobmyS+#

; Start Explorer specifics
; (Courtesy of Dan Cerys)

#+Explorer
(progn

; Define Scheme major mode for Zmacs  
zwei:
(defmajor com-scheme-mode scheme-mode "Scheme"
	  "Sets things up for editing Scheme code." ()
  (setq *space-indent-flag* t)
  (setq *paragraph-delimiter-list* '(#\. #\space #\tab #\"))
  (setq *comment-start* 'lisp-find-comment-start-and-end)
  ;;The following three are non-Zmacs vars that are made settable by the setf's below
  (setq *print-array* t)			;print arrays readably
  (setq ucl:*default-prompt* 'ps:scheme-prompt-when-appropriate)
  (setq ucl:*default-read-function* 'ps:scheme-read-when-apropriate)
  (set-char-syntax list-slash *mode-list-syntax-table* #\\)
  (set-char-syntax list-alphabetic *mode-list-syntax-table* #\/)
  (set-comtab *mode-comtab*
	      '(#\tab com-indent-for-lisp
		#\rubout com-tab-hacking-rubout
		#\c-rubout com-rubout
		#\m-z com-compile-and-exit
		#\c-m-z com-evaluate-and-exit))
  )

; This doesn't clobber any of the other Lisp modes (eg Common Lisp), since they
; set their readtable when the major mode switch occurs.
(defvar scheme-mode-hook #'(lambda ()
			     (setq *readtable* roadblock-readtable))
  "Simple function which specifies the use of the Scheme readtable within Scheme.")

; Make these variables settable (and thus undoable) in Zmacs modes.
(dolist (symbol '(*print-array* ucl:*default-prompt* ucl:*default-read-function*))
  (setf (get symbol 'zwei:mode-settable-p) t))

; The following must be :common-lisp rather than the more intuitive
; value of :scheme.  The purpose of this system variable is to tell the difference
; between kludgy Zetalisp and modern Common Lisp.  We are doing Scheme on top
; of Common Lisp.
(defvar *scheme-value-for-sys-lisp-mode* :common-lisp "Appropriate Scheme value for the variable sys:*lisp-mode*")

; Use the correct readtable when compiling Scheme forms/files
(ticl:advise (:property :mode fs:file-attribute-bindings) :around get-scheme-bindings-if-appropriate nil
  (let ((mode-keyword (third arglist)))
    (if (eq :scheme mode-keyword)
	(values '(sys:*lisp-mode* sys:*readtable* sys:*reader-symbol-substitutions* zwei::*default-major-mode*)
		`(,*scheme-value-for-sys-lisp-mode* ,roadblock-readtable nil :scheme))
	:do-it)))

; Compile the above advice
(eval-when (eval load compile)
  (ticl:compile-encapsulations '(:property :mode fs:file-attribute-bindings)))

(ticl:defprop scheme-mode t zwei:all-uppercase) ;;case is insignificant
(ticl:defprop scheme-mode :lisp zwei:editing-type)   ;;Scheme is Lisp

; Handle DEFINE... top-level forms for sectionizing (yes, a hack, but this is better than including the complete function)
(ticl:advise zwei:symbol-from-string :around check-for-define nil
  (let ((str (first arglist))
	     (line (second arglist))
	     (sym (fourth arglist)))
    (if (and (consp sym) ;;eg (foo a)
	     (not (null line))
	     (> (length line) 10.)
	     (string-equal "(define" line :end2 7))
	(values (first sym) str)
	:do-it)))

; Compile advise
(eval-when (eval load compile)
  (ticl:compile-encapsulations 'zwei:symbol-from-string))

; Treat "SCM" just like "LISP"
(let ((type (string *scheme-file-type*)))
  (cond ((not (member type fs:*its-uninteresting-types* :test #'equal))
	 (push type fs:*its-uninteresting-types*))))

; "One should always use canonical types"
(fs:define-canonical-type :scheme #,*scheme-file-type*) ;Scheme source

; Default Zmacs Major Mode for Scheme source is Scheme
(unless (assoc :scheme fs:*file-type-mode-alist*)
  (setq fs:*file-type-mode-alist*
	(append fs:*file-type-mode-alist* (list (cons :scheme :scheme)))))

; Make Scheme Mode accessible
(zwei:set-comtab zwei:*standard-comtab*
		 '()
		 (zwei:make-command-alist '(zwei:com-scheme-mode)))

(defun in-scheme? ()
  "Boolean, which is true when we are in Scheme, meaning that the current package
is the Scheme package."
  (eq? *package* scheme-package))

(defvar *saved-prompt*  ucl:*default-prompt*
  "Saved old prompt; used when we temporarily want to set it")     

(defun scheme-prompt-when-appropriate ()
  "When in Scheme (using the Scheme package), present the user with the Scheme prompt (==>),
else use the standard default prompt."
  (let* ((scheme-prompt "==> ")
	 (default-non-scheme-prompt "> ")
	 (prompt (if (eq *saved-prompt* 'scheme-prompt-when-appropriate)
		     default-non-scheme-prompt
		     *saved-prompt*))) ;avoid infinite loop
    (if (in-scheme?)
      scheme-prompt
      (if (stringp prompt)			; can either be a string or a function
	  prompt
	  (funcall prompt)))))

(defvar *saved-read-function* ucl:*default-read-function*)

(defun scheme-read-when-apropriate ()
  "When in Scheme (using the Scheme package), do Scheme preprocessing."
  (let* ((read-function (if (eq *saved-read-function*  ucl:*default-read-function*)
			    'ucl:read-for-ucl	;avoid endless recursion
			    *saved-read-function*))
	 (expression (funcall read-function)))
    (if (in-scheme?)
	(translate-in-context expression *current-rep-context*)
	expression)))

) ;(... ngorp) rerolpxE+#

; end Explorer specific things
