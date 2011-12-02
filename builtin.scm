; File builtin.scm / -*- Mode: Scheme; Syntax: Scheme; Package: Scheme; -*-
; See file COPYING

;;;; Compilation of calls to built-in Scheme procedures

; The usual integrations

; An entry in the integrations table is a pair, one of the following:
;   (FUN foo)            - translate as #'foo or (foo ...)
;   (PRED foo)		 - translate calls as (ps:true? (foo ...))
;   (SUBST bvl body)     - translate calls as appropriate
;   (LAMBDA bvl body)    - ditto
;   (CASE-AUX)	         - a special case kludge

; The integrations table is indexed by Common Lisp symbols.

(define integrations-table (make-table))

(define (define-integration! var int)
  (table-set! integrations-table var int))

(for-each (lambda (z)
	    (define-integration!
	      (program-env-lookup revised^4-scheme-env (car z))
	      (cadr z)))
    `(
      (*                              (fun ps-lisp:*))
      (+                              (fun ps-lisp:+))
      (-                              (fun ps-lisp:-))
      (/                              (fun ps-lisp:/))
      (<=                             (pred ps-lisp:<=))
      (<                              (pred ps-lisp:<))
      (=                              (pred ps-lisp:=))
      (>=                             (pred ps-lisp:>=))
      (>                              (pred ps-lisp:>))
      (abs                            (fun ps-lisp:abs))
      (acos                           (fun ps-lisp:acos))
      (angle                          (fun ps-lisp:phase))
      (append                         (fun ps-lisp:append))
      (apply                          (fun ps-lisp:apply))
      (asin                           (fun ps-lisp:asin))
      (assoc
       (subst (obj list)
	 (ps:true? (ps-lisp:assoc obj list
				 :test (ps-lisp:function ps:scheme-equal-p)))))
      (assq
       (subst (obj list)
	 (ps:true? (ps-lisp:assoc obj list :test (ps-lisp:function ps-lisp:eq)))))
      (assv                           (pred ps-lisp:assoc 2))
      (atan                           (fun ps-lisp:atan))
      (boolean?			      (pred ps:booleanp 1))
      (caaaar                         (fun ps-lisp:caaaar))
      (caaadr                         (fun ps-lisp:caaadr))
      (caaar                          (fun ps-lisp:caaar))
      (caadar                         (fun ps-lisp:caadar))
      (caaddr                         (fun ps-lisp:caaddr))
      (caadr                          (fun ps-lisp:caadr))
      (caar                           (fun ps-lisp:caar))
      (cadaar                         (fun ps-lisp:cadaar))
      (cadadr                         (fun ps-lisp:cadadr))
      (cadar                          (fun ps-lisp:cadar))
      (caddar                         (fun ps-lisp:caddar))
      (cadddr                         (fun ps-lisp:cadddr))
      (caddr                          (fun ps-lisp:caddr))
      (cadr                           (fun ps-lisp:cadr))
      (call-with-current-continuation
	  (subst (proc)
	    (ps-lisp:block continuation
	      (ps-lisp:funcall proc
		  (ps-lisp:function
		    (ps-lisp:lambda (ps-lisp:&rest vals)
		      (ps-lisp:return-from continuation
					   (ps-lisp:values-list vals))))))))
      (call-with-input-file
	  (lambda (string proc)
	    (ps-lisp:with-open-file (port (ps-lisp:merge-pathnames string)
					  :direction :input)
	      (ps-lisp:funcall proc port))))
      (call-with-output-file
	  (lambda (string proc)
	    (ps-lisp:with-open-file (port (ps-lisp:merge-pathnames string)
					  :direction :output
					  :if-exists :new-version)
	      (ps-lisp:funcall proc port))))
      (call-with-values
	  (subst (thunk proc)
	    (ps-lisp:multiple-value-call proc (ps-lisp:funcall thunk))))
      (car                            (fun ps-lisp:car))
      (cdaaar                         (fun ps-lisp:cdaaar))
      (cdaadr                         (fun ps-lisp:cdaadr))
      (cdaar                          (fun ps-lisp:cdaar))
      (cdadar                         (fun ps-lisp:cdadar))
      (cdaddr                         (fun ps-lisp:cdaddr))
      (cdadr                          (fun ps-lisp:cdadr))
      (cdar                           (fun ps-lisp:cdar))
      (cddaar                         (fun ps-lisp:cddaar))
      (cddadr                         (fun ps-lisp:cddadr))
      (cddar                          (fun ps-lisp:cddar))
      (cdddar                         (fun ps-lisp:cdddar))
      (cddddr                         (fun ps-lisp:cddddr))
      (cdddr                          (fun ps-lisp:cdddr))
      (cddr                           (fun ps-lisp:cddr))
      (cdr                            (fun ps-lisp:cdr))
      (ceiling                        (fun ps-lisp:ceiling))
      (char->integer                  (fun ps-lisp:char-code))
      (char-alphabetic?               (pred ps-lisp:alpha-char-p 1))
      (char-ci<=?                     (pred ps-lisp:char-not-greaterp))
      (char-ci<?                      (pred ps-lisp:char-lessp))
      (char-ci=?                      (pred ps-lisp:char-equal))
      (char-ci>=?                     (pred ps-lisp:char-not-lessp))
      (char-ci>?                      (pred ps-lisp:char-greaterp))
      (char-downcase                  (fun ps-lisp:char-downcase))
      (char-lower-case?               (pred ps-lisp:lower-case-p 1))
      (char-numeric?                  (pred ps-lisp:digit-char-p 1))
      (char-ready?		      (pred ps-lisp:listen))
      (char-upcase                    (fun ps-lisp:char-upcase))
      (char-upper-case?               (pred ps-lisp:upper-case-p 1))
      (char-whitespace?		      (pred ps:char-whitespace-p 1))
      (char<=?                        (pred ps-lisp:char<=))
      (char<?                         (pred ps-lisp:char<))
      (char=?                         (pred ps-lisp:char=))
      (char>=?                        (pred ps-lisp:char>=))
      (char>?                         (pred ps-lisp:char>))
      (char?                          (pred ps-lisp:characterp 1))
      (close-input-port               (fun ps-lisp:close))
      (close-output-port              (fun ps-lisp:close))
      (complex?                       (pred ps-lisp:numberp 1))
      (cons                           (fun ps-lisp:cons))
      (cos                            (fun ps-lisp:cos))
      (current-input-port
       (subst () ps-lisp:*standard-input*))
      (current-output-port
       (subst () ps-lisp:*standard-output*))
      (denominator                    (fun ps-lisp:denominator))
      (eof-object?
       (subst (obj)
	 (ps:true? (ps-lisp:eq obj ps:eof-object))))
      (eq?                            (pred ps-lisp:eq 2))
      (equal?			      (pred ps:scheme-equal-p 2))
      (eqv?                           (pred ps-lisp:eql 2))
      (even?                          (pred ps-lisp:evenp 1))
      (exact?                         (pred ps-lisp:rationalp 1))
      (exact->inexact                 (fun ps-lisp:float))
      (expt                           (fun ps-lisp:expt))
      (exp                            (fun ps-lisp:exp))
      (floor                          (fun ps-lisp:floor))
      (for-each                       (fun ps-lisp:mapc))
      (gcd                            (fun ps-lisp:gcd))
      (imag-part                      (fun ps-lisp:imagpart))
      (inexact?                       (pred ps-lisp:floatp 1))
      (inexact->exact                 (fun ps-lisp:rationalize))
      (input-port?		      (pred ps:input-port-p 1))
      (integer->char                  (fun ps-lisp:code-char))
      (integer?                       (pred ps-lisp:integerp 1))
      (lcm                            (fun ps-lisp:lcm))
      (length                         (fun ps-lisp:length))
      (list                           (fun ps-lisp:list))
      (list->string
       (subst (l) (ps-lisp:coerce (ps-lisp:the ps-lisp:list l)
				  (ps-lisp:quote ps-lisp:simple-string))))
      (list->vector
       (subst (l) (ps-lisp:coerce (ps-lisp:the ps-lisp:list l)
				  (ps-lisp:quote ps-lisp:simple-vector))))
      (list-ref
       (subst (list n) (ps-lisp:nth n list)))
      (list-tail
       (subst (list n) (ps-lisp:nthcdr n list)))
      (log                            (fun ps-lisp:log))
      (magnitude                      (fun ps-lisp:abs))
      (make-polar
       (subst (r th) (ps-lisp:* r (ps-lisp:cis th))))
      (make-rectangular               (fun ps-lisp:complex))
      (map                            (fun ps-lisp:mapcar))
      (max                            (fun ps-lisp:max))
      (member
       (subst (obj list)
	 (ps:true? (ps-lisp:member obj list
				  :test (ps-lisp:function ps:scheme-equal-p)))))
      (memq
       (subst (obj list)
	 (ps:true? (ps-lisp:member obj list :test (ps-lisp:function ps-lisp:eq)))))
      (memv                           (pred ps-lisp:member 2))
      (min                            (fun ps-lisp:min))
      (modulo                         (fun ps-lisp:mod))
      (negative?                      (pred ps-lisp:minusp 1))
      (newline                        (fun ps-lisp:terpri))
      (not			      (special))
      (null?                          (pred ps-lisp:null 1))
      (number?                        (pred ps-lisp:numberp 1))
      (numerator                      (fun ps-lisp:numerator))
      (odd?                           (pred ps-lisp:oddp 1))
      (open-input-file
       (subst (string)
	 (ps-lisp:open (ps-lisp:merge-pathnames string) :direction :input)))
      (open-output-file
       (subst (string)
	 (ps-lisp:open (ps-lisp:merge-pathnames string) :direction :output)))
      (output-port?                   (pred ps:output-port-p 1))
      ;; This isn't quite right; PAIR? wants to return false for
      ;; procedures.  (Some Common Lisps implement some functions as
      ;; pairs.)  But the run-time overhead of this check would be
      ;; prohibitively high.
      (pair?                          (pred ps-lisp:consp 1))
      (positive?                      (pred ps-lisp:plusp 1))
      (procedure?		      (pred ps:procedurep 1))
      (quotient
       (subst (n1 n2)
	 (ps-lisp:values (ps-lisp:truncate n1 n2))))
      (rational?                      (pred ps-lisp:rationalp 1))
      (real?			      (pred ps:realp 1))
      (real-part                      (fun ps-lisp:realpart))
      (remainder                      (fun ps-lisp:rem))
      (reverse                        (fun ps-lisp:reverse))
      (round                          (fun ps-lisp:round))
      (set-car!
       (subst (pair obj)
	 (ps-lisp:setf (ps-lisp:car pair) obj)
	 ps:unspecific))
      (set-cdr!
       (subst (pair obj)
	 (ps-lisp:setf (ps-lisp:cdr pair) obj)
	 ps:unspecific))
      (sin                            (fun ps-lisp:sin))
      (sqrt                           (fun ps-lisp:sqrt))
      (string->list
       (subst (string)
	 (ps-lisp:coerce (ps-lisp:the ps-lisp:simple-string string)
		      (ps-lisp:quote ps-lisp:list))))
      (string->symbol
       (subst (string)
	 (ps-lisp:values (ps-lisp:intern string ps:scheme-package))))
      (string-ci<=?                   (pred ps-lisp:string-not-greaterp 2))
      (string-ci<?                    (pred ps-lisp:string-lessp 2))
      (string-ci=?                    (pred ps-lisp:string-equal 2))
      (string-ci>=?                   (pred ps-lisp:string-not-lessp 2))
      (string-ci>?                    (pred ps-lisp:string-greaterp 2))
      (string-copy                    (fun ps-lisp:copy-seq))
      (string-fill!
       (subst (s val)
	 (ps-lisp:fill (ps-lisp:the ps-lisp:simple-string s) val)))
      (string-length
       (subst (s)
         (ps-lisp:length (ps-lisp:the ps-lisp:simple-string s))))
      (string-ref
       (subst (s k)
	 (ps-lisp:char (ps-lisp:the ps-lisp:simple-string s) k)))
      (string-set!
       (subst (s k obj)
	 (ps-lisp:setf (ps-lisp:char (ps-lisp:the ps-lisp:simple-string s) k) obj)
	 ps:unspecific))
      (string<=?                      (pred ps-lisp:string<= 2))
      (string<?                       (pred ps-lisp:string< 2))
      (string=?                       (pred ps-lisp:string= 2))
      (string>=?                      (pred ps-lisp:string>= 2))
      (string>?                       (pred ps-lisp:string> 2))
      (string?                        (pred ps-lisp:simple-string-p 1))
      (substring                      (fun ps-lisp:subseq))
      (symbol?			      (pred ps:scheme-symbol-p 1))
      (tan                            (fun ps-lisp:tan))
      (transcript-off
       (subst ()
         (ps-lisp:dribble)
	 ps:unspecific))
      (transcript-on
       (subst (filespec)
         (ps-lisp:dribble filespec)
	 ps:unspecific))
      (truncate                       (fun ps-lisp:truncate))
      (values                         (fun ps-lisp:values))
      (vector                         (fun ps-lisp:vector))
      (vector->list
       (subst (vec)
	 (ps-lisp:coerce (ps-lisp:the ps-lisp:simple-vector vec)
		      (ps-lisp:quote ps-lisp:list))))
      (vector-fill!
       (subst (vec val)
	 (ps-lisp:fill (ps-lisp:the ps-lisp:simple-vector vec) val)))
      (vector-length
       (subst (vec)
         (ps-lisp:length (ps-lisp:the ps-lisp:simple-vector vec))))
      (vector-ref                     (fun ps-lisp:svref))
      (vector-set!
       (subst (vec k obj)
	 (ps-lisp:setf (ps-lisp:svref vec k) obj)
	 ps:unspecific))
      (with-input-from-file
       (subst (string thunk)
      	 (ps-lisp:with-open-file (ps-lisp:*standard-input*
			         (ps-lisp:merge-pathnames string)
				 :direction :input)
	   (ps-lisp:funcall thunk))))
      (with-output-to-file
       (subst (string thunk)
	 (ps-lisp:with-open-file (ps-lisp:*standard-output*
			         (ps-lisp:merge-pathnames string)
				 :direction :output
				 :if-exists :new-version)
	    (ps-lisp:funcall thunk))))
      (write-char                     (fun ps-lisp:write-char))
      (zero?                          (pred ps-lisp:zerop 1))

      ;; Auxiliaries
      (unassigned (subst () ps:unassigned))
      (unspecific (val ps:unspecific))
      (and-aux (special))
      (or-aux (special))
      (=>-aux (special))
      (case-aux (special))
      ))
