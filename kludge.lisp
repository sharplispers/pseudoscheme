
; Clobber some internal Lucid functions to make Pseudoscheme run
; better.  This will probably break in some future Lucid release, but
; it works in 4.1 on a Sparc.

; As far as I can tell, (lcl::top-level-read) is the same as (read)
; except that it also sets the variables -, +, ++, and +++.

(in-package "PS")

(defun kludge-top-level-read (original-top-level-read
			      preserve-whitespace-p
			      stream eof-error-p eof-value)
  (if (eq *package* scheme-package)
      (loop
       (let ((c (peek-char t stream eof-error-p stream)))
	 (cond ((eq c stream) ;eof
		(return c))
	       ((eq c #\;)
		(read-line stream eof-error-p stream))   ;loop
	       ((eq c #\:)
		(return (funcall original-top-level-read
				 stream eof-error-p eof-value)))
	       (t
		(return
		 (let ((form (funcall *scheme-read* stream)))
		   (if (eq form eof-object)
		       (if eof-error-p
			   (error "End of file on stream ~S" stream)
			   eof-value)
		       (let ((result
			      (if (or (consp form)
				      (and (symbolp form)
					   (eq (symbol-package form)
					       scheme-package)))
				  `(scheme-form ,form)
				  form)))
			 (setq +++ ++ ++ + + - - result)
			 result))))))))
      (funcall original-top-level-read stream eof-error-p eof-value)))


; Lucid can't be helped
#|
 (loop (if (listen stream)
	   (let ((c (peek-char nil stream eof-error-p)))
	     (if (eq c #\newline)
		 (return (read-char stream eof-error-p))))))

      (and (not (eq *readtable* scheme-readtable))
	   (not (eq *readtable* roadblock-readtable))
	   (not (eq *readtable* *non-scheme-readtable*)))
      (progn (setq *readtable* roadblock-readtable)   ;Invoke Scheme reader
	     (let ((form (apply original-top-level-read args)))
	       (format *debug-io*
		       "~&(Fixing Lucid lossage: *readtable* restored to ~s~%"
		       'roadblock-readtable)

      ))
|#

(defvar *original-top-level-read*
  #'lucid::top-level-read)

(defun lucid::top-level-read (&optional (stream *standard-input*)
					(eof-error-p t)
					(eof-value stream))
  ;; Should avoid preserving whitespace ?
  (kludge-top-level-read *original-top-level-read*
			 nil
			 stream eof-error-p eof-value))

(defvar *original-debugger-top-level-read*
  #'lucid::debugger-top-level-read)

(defun lucid::debugger-top-level-read (&optional (stream *debug-io*)
						 (eof-error-p t)
						 (eof-value stream))
  ;; Should read-preserving-whitespace ?
  (kludge-top-level-read *original-debugger-top-level-read*
			 t
			 stream eof-error-p eof-value))
