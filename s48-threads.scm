
;; See interface at bottom of file.

(lisp:defpackage "S48-THREADS-SUPPORT"
  (:use "COMMON-LISP")
  (:export "MAKE-LOCK"
           "PROCESS-LOCK"
	   "PROCESS-UNLOCK"
	   "PROCESS-RUN-FUNCTION"
	   "MAKE-PLACEHOLDER"
	   "PLACEHOLDER-REF"
	   "PLACEHOLDER-SET"))
(lisp:in-package "S48-THREADS-SUPPORT")

; KMP's code

(defun *nyi (x) (error "Not yet implemented: ~S" x))
(defmacro nyi (x) (warn "Not yet implemented: ~S" x) `(*nyi ',x))

(defun make-lock ()
  #+LispWorks (mp:make-lock)
  #+Allegro   (mp::make-process-lock)
  #-(or LispWorks Allegro) (nyi make-lock))

(defun process-lock (lock &optional whostate timeout)
  (check-type whostate (or null (real 0.1 20)) ;imposed by Franz - ugh
	      "a timeout between 0.1 and 20 seconds")
  #+LispWorks (mp:process-lock lock whostate timeout)
  #+Allegro   (mp::process-lock lock sys:*current-process* whostate timeout)
  #-(or LispWorks Allegro) (nyi process-lock))

(defun process-unlock (lock &optional errorp)
  #+LispWorks (mp:process-unlock lock errorp)
  #+Allegro   (if errorp 
	          (mp::process-unlock lock)
	          (ignore-errors (mp::process-unlock lock)))
  #-(or LispWorks Allegro) (nyi process-unlock))

(defmacro with-process-lock ((lock &rest keys &key whostate timeout)
			     &body forms)
  (declare (ignore whostate timeout)) ;they're used in keys
  #+LispWorks `(mp:with-lock (,lock ,@keys) ,@forms)
  #+Allegro   `(mp:with-process-lock (,lock ,@keys) ,@forms)
  #-(or LispWorks Allegro) 
      `(call-with-process-lock #'(lambda () ,@forms) ,@keys))

#-(or LispWorks Allegro)
(defun call-with-process-lock (fn &key whostate timeout)
  (nyi call-with-process-lock))

; 12 Multiprocessing
;
; 12.3 Locks
; Locks can be used to control access to shared data by several processes.
;
; A lock has the following components: name (a string), lock (t or nil,
; that is, whether the lock is set or not), owner (a process, or nil)
; and count (an integer showing the number of times the lock has been
; set).
;
; The two main symbols used in locking are the function make-lock, to
; create a lock, and the macro with-lock, to execute a body of code
; while holding the specified lock.
;
; mp:make-lock 
; Function
;
; mp:make-lock &key important-p &allow-other-keys
;
; Creates a lock object. If important-p is t the lock is added to the
; list held in the global variable mp:*important-locks*. The function
; mp:free-important-locks frees all important locks associated with a
; given process (or all the important locks if called on nil). Other
; keywords should be names of the lock components.
;
; mp:process-lock 
; Function
;
; mp:process-lock lock &optional whostate timeout
;
; Blocks the current process until the lock is claimed or timeout
; elapses if it has been specified. Returns t if lock was claimed, nil
; otherwise.
;
; mp:process-unlock 
; Function
;
; mp:process-unlock lock &optional errorp
;
; Releases the lock. If errorp is non-nil it signals an error if the
; current process does not own the lock. The default value of errorp is
; t.
;
; mp:with-lock 
; Macro
;
; mp:with-lock ((lock &rest lock-args) &body body
;
; Executes the body with lock held. Arguments to pass on to
; mp:process-lock are specified using lock-args.  The following
; accessors are available for locks: lock-owner, lock-count, lock-name
; and lock-lock.



;;; more kmp code

(defun process-run-function (name function &rest args)
  #+Genera
  (apply #'process:process-run-function name function args)
  #+LispWorks
  (apply #'mp:process-run-function name '() function args)
  #+Allegro
  (apply #'mp:process-run-function name function args))

(defun process-wait (whostate wait-function &rest args)
  #+Genera
  (apply #'process:process-wait whostate wait-function args)
  #+(or LispWorks Allegro)
  (apply #'mp:process-wait whostate wait-function args))
  
(defun process-kill (process)
  #+(or LispWorks Allegro) (mp:process-kill process)
  #-(or LispWorks Allegro) (nyi process-kill))




; some JAR code

(defun make-placeholder (id)
  (mp:make-mailbox :lock-name id))

(defun placeholder-ref (ph) (mp:mailbox-read ph "placeholder-ref"))
(defun placeholder-set (ph x) (mp:mailbox-send ph x))

;Scheme 48
;    make-placeholder [id]
;    placeholder-value p       -- should be called placeholder-ref
;    placeholder-set! p xx
;Lispworks - mailbox
;    mp:make-mailbox :size nn :lock-name xx
;      "The reader process is set to the current process."
;      Size argument appears to be meaningless.
;    mp:mailbox-read mm [wait-reason]
;    mp:mailbox-send mm xx



;;;; Interface

(lisp:in-package "SCHEME")

; Temp kludge

(define (make-lock)               (s48-threads-support:make-lock))
(define (obtain-lock lock)        (s48-threads-support:process-lock lock))
(define (release-lock lock)       (s48-threads-support:process-unlock lock))
(define (spawn thunk description)
  (s48-threads-support:process-run-function description thunk))

(define make-placeholder #'s48-threads-support:make-placeholder)
(define placeholder-ref #'s48-threads-support:placeholder-ref)
(define placeholder-set! #'s48-threads-support:placeholder-set)
