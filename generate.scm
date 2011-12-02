; -*- Mode: Scheme; Syntax: Scheme; Package: Scheme; -*-
; File generate.scm / See file COPYING

;;;; Common Lisp back end

; Translation of a single expression

; Compare this with SCHEMIFY, which is a different back end.

;+++
; To do:
;  - Pass continuations around so that RETURN's can be propagated
;    inside of PROG statements (for readability)

(define program-variable-CL-symbol program-variable-location)

(define @lambda-encountered? (make-fluid #f))

; GENERATE

(define (generate-top node env ignore?)
  (case (node-type node)
    ((begin)
     (prognify (append (deprognify (generate-top (begin-first node) env #t))
		       (deprognify (generate-top (begin-second node)
						 env ignore?)))))
    ((define)
     (generate-define node env))
    (else
     (generate-expression-top node env ignore?))))

; DEFINE

(define (generate-define def env)
  (let ((lhs (define-lhs def)))
    (let-fluid @where (program-variable-name lhs)
      (lambda ()
	(let ((rhs (define-rhs def))
	      (CL-sym (program-variable-CL-symbol lhs))
	      (name (program-variable-name lhs)))
	(cond ((mutable-program-variable? lhs)
	       `(ps-lisp:progn
		 ,(generate-setq-top lhs rhs env)
		 (ps:set-forwarding-function (ps-lisp:quote ,CL-sym)
					       (ps-lisp:quote ,name))))
	      ((lambda? rhs)
	       `(ps-lisp:progn
		 (ps-lisp:defun ,CL-sym ,@(cdr (generate-lambda-top rhs env)))
		 (ps:set-value-from-function (ps-lisp:quote ,CL-sym)
					       (ps-lisp:quote ,name))))

	      (else
	       `(ps-lisp:progn
		 ,(generate-setq-top lhs rhs env)
		 (ps:set-function-from-value (ps-lisp:quote ,CL-sym)
					       (ps-lisp:quote ,name))))))))))

(define (generate-expression-top node env ignore?)
  (let-fluid @lambda-encountered? #f
    (lambda ()
      (noting-variable-references
       (lambda ()
	 ;; Don't beta-reduce this LET (?)
	 (let ((code (generate node env (if ignore? cont/ignore cont/value))))
	   (emit-top-level
	    (locally-specialize (deprognify code)))))))))

(define (generate-lambda-top node env)
  (let-fluid @lambda-encountered? #f
    (lambda ()
      (noting-variable-references
       (lambda ()
	 (let* ((bvl+body (generate-lambda-aux node env cont/value))
		(body (locally-specialize (cdr bvl+body))))
	   `(ps-lisp:lambda ,(car bvl+body)
	      ,@(if (and (pair? body)
			 (null? (cdr body))
			 (car-is? (car body) 'ps-lisp:locally))
		    (cdr (car body))
		    body))))))))

(define (generate-setq-top lhs rhs env)
  (let-fluid @lambda-encountered? #f
    (lambda ()
      (noting-variable-references
       (lambda ()
	 ;; Don't beta-reduce this LET (?)
	 (let ((code (generate rhs env cont/ignore)))
	   (note-variable-reference! lhs)
	   (emit-top-level
	    (locally-specialize `((ps-lisp:setq ,(program-variable-CL-symbol lhs)
					     ,code))))))))))



; Generate code for a single expressions

(define (generate node env cont)
  (case (node-type node)
    ((local-variable) (generate-local-variable node env cont))
    ((program-variable) (generate-program-variable node env cont))
    ((constant) (generate-constant node env cont))
    ((call)     (generate-call	   node env cont))
    ((lambda)   (generate-lambda   node env cont))
    ((letrec)   (generate-letrec   node env cont))
    ((if)       (generate-if	   node env cont))
    ((begin)	(generate-begin	   node env cont))
    ((set!)	(generate-set!	   node env cont))
    (else (note "don't know how to generate" node))))

(define (generate-list node-list env)
  (map (lambda (node) (generate node env cont/value))
       node-list))

(define (generate-body node env cont)
  (deprognify (generate node env cont)))

; Constant

(define (generate-constant node env cont)
  env					;ignored
  (let ((val (constant-value node)))
    (cond ((constant-quoted? node)
	   (deliver-value-to-cont `(ps-lisp:quote ,val) cont))
	  ;; Hack for bootstrap from Schemes that don't distinguish ()
	  ;; from #f: the expression #f or () is translated as ps:false,
	  ;; while '#f or '() is translated as '().
	  ((eq? val #t) (deliver-value-to-cont `ps:true cont))
	  ((eq? val #f)
	   (if (eq? (continuation-type cont) 'cont/test)
	       `ps-lisp:nil
	       (deliver-value-to-cont `ps:false cont)))
	  (else
	   ;; Not quoted in Scheme implies doesn't need quoting in Common Lisp
	   (deliver-value-to-cont val cont)))))

; Variable

(define (generate-local-variable var env cont)
  env ;ignored
  (let ((sub (variable-substitution var)))
    (deliver-value-to-cont
     (if (pair? sub)
	 (case (car sub)
	   ((val) (cadr sub))
	   ((fun) `(ps-lisp:function ,(cadr sub)))
	   (else (error "lossage in generate-local-variable" sub)))
	 sub)
     cont)))

(define (generate-program-variable var env cont)
  env ;ignored
  (let ((sub (get-integration var)))
    (deliver-value-to-cont
     (if (pair? sub)
	 (case (car sub)
	   ((val) (cadr sub))
	   ((fun) `(ps-lisp:function ,(cadr sub)))
	   (else
	    (note-variable-reference! var)
	    (program-variable-CL-symbol var)))
	 (begin (note-variable-reference! var)
		(program-variable-CL-symbol var)))
     cont)))

(define (get-integration var)
  (table-ref integrations-table var))

(define *declare-program-variables-special?* #t)

(define (note-variable-reference! var)
  (if (and (not (qualified-symbol? (program-variable-name var)))
	   *declare-program-variables-special?*)
      (let ((g (fluid @CL-variable-references)))
	(if (and (not (eq? g 'dont-accumulate))
		 (not (memq var g)))
	    (set-fluid! @CL-variable-references (cons var g))))))

; Combinations

(define (generate-call node env cont)
  (let ((proc (call-proc node))
	(args (call-args node)))
    (case (node-type proc)
      ((program-variable)
       (if (mutable-program-variable? proc)
	   (generate-general-call proc args env cont)
	   (generate-call-to-program-variable proc args env cont)))
      ((local-variable)
       (if (and (pair? (variable-substitution proc))
		(eq? (car (variable-substitution proc)) '--generate-call--))
	   ((cadr (variable-substitution proc))
	    (generate-list args env)
	    cont)
	   (generate-general-call proc args env cont)))
      ((lambda)
       (if (and (not (n-ary? proc))
		(= (length args) (length (lambda-vars proc))))
	   (generate-let proc args env cont)
	   (generate-general-call proc args env cont)))
      (else
       (generate-general-call proc args env cont)))))

(define (generate-general-call proc args env cont)
  (deliver-value-to-cont
   (funcallify (generate proc env cont/value)
	       (generate-list args env))
   cont))

(define (generate-call-to-program-variable pvar args env cont)
  (let ((sub (get-integration pvar)))
    (if (not (pair? sub))
	(generate-call-to-unknown pvar args env cont)
	(case (car sub)
	  ((subst)
	   (let ((params (cadr sub))
		 (body (prognify (cddr sub))))
	     (if (= (length args) (length params))
		 (substitute-and-peep (map cons
					   params
					   (generate-list args env))
				      ;; ??? kind of kludgey
				      (deliver-value-to-cont body cont))
		 (begin (note "wrong number of arguments"
			      (make-call pvar args))
			(generate-call-to-unknown pvar args env cont)))))
	  ((lambda)
	   (if (= (length args) (length (cadr sub)))
	       `(ps-lisp:let ,(map list (cadr sub) (generate-list args env))
		  ,@(deprognify
		     (deliver-value-to-cont (prognify (cddr sub))
					    cont)))
	       (generate-call-to-unknown pvar args env cont)))
	  ((fun)
	   (deliver-value-to-cont `(,(cadr sub) ,@(generate-list args env))
				  cont))
	  ((pred)
	   (deliver-test-to-cont `(,(cadr sub) ,@(generate-list args env))
				 cont))
	  ((val)
	   (deliver-value-to-cont (funcallify (cadr sub)
					      (generate-list args env))
				  cont))
	  ((special)
	   (case (program-variable-name pvar) ;lose
	     ((not)
	      (if (= (length args) 1)
		  (deliver-test-to-cont
		     `(ps-lisp:not ,(generate (car args) env cont/test))
		     cont)
		  (generate-call-to-unknown pvar args env cont)))
	     ((and-aux)
	      ;; We can assume that the arg count is OK.
	      (generate-and (car args)
			    (if (lambda? (cadr args))
				(lambda-body (cadr args))
				(make-call (cadr args) '()))
			    env
			    cont))
	     ((or-aux)
	      (generate-or (car args)
			   (if (lambda? (cadr args))
			       (lambda-body (cadr args))
			       (make-call (cadr args) '()))
			   env
			   cont))
	     ((case-aux)
	      (generate-case (car args) (cadr args) (caddr args) (cdddr args)
			     env cont))
	     ((=>-aux)
	      (let* ((proc-thunk (cadr args))
		     (proc (if (lambda? proc-thunk)
			       (lambda-body proc-thunk)
			       (make-call proc-thunk '()))))
		(if (and (lambda? proc)
			 (= (length (lambda-vars proc)) 1))
		    (generate-=> (car args)
				 (car (lambda-vars proc))
				 (lambda-body proc)
				 (caddr args)
				 env cont)
		    (let ((var (make-local-variable 'temp)))
		      (generate-=> (car args)
				   var
				   (make-call proc (list var))
				   (caddr args)
				   env cont)))))
	     (else
	      (error "losing built-in" pvar))))
	  (else
	   (error "losing CASE" sub))))))

;(and x y)         == (if x y #f)
;		   == (lisp:if (truep x) y #f)
;
;(and (true? x) y) == (if (true? x) y #f)
;                  == (lisp:if (truep (true? x)) y #f)
;                  == (lisp:if x y #f)
;
;(truep (and x y)) == (truep (if x y #f))
;                  == (truep (lisp:if (truep x) y #f))
;                  == (lisp:if (truep x) (truep y) nil)    [(truep #f) = nil]
;                  == (lisp:and (truep x) (truep y))

(define (generate-and first second env cont)
  (case (continuation-type cont)
    ((cont/test cont/ignore)
     `(ps-lisp:and ,(generate first env cont/test)
		,@(deandify (generate second env cont/test))))
    (else
     `(ps-lisp:if ,(generate first env cont/test)
	       ,(generate second env cont)
	       ,(deliver-value-to-cont `ps:false cont)))))

;(or x y)          == (let ((temp x)) (if temp temp y))
;		   == (let ((temp x)) (lisp:if (truep temp) temp y))
;
;(or (true? x) y)  == (if (true? x) (true? x) y)
;                  == (lisp:if (truep (true? x)) (true? x) y)
;                  == (lisp:if x (true? x) y)      [cf. value-form->test-form]
;                  == (lisp:if x (lisp:or x #f) y)
;                  == (lisp:if x x y)
;                  == (lisp:or x y)
;
;(truep (or x y))  == (truep (if x x y))
;                  == (truep (lisp:if (truep x) x y))
;                  == (truep (lisp:if (truep x) x y))
;                  == (lisp:if (truep x) (truep x) (truep y))
;                  == (lisp:or (truep x) (truep y))

(define (generate-or first second env cont)
  (case (continuation-type cont)
    ((cont/test cont/ignore)
     `(ps-lisp:or ,(generate first env cont/test)
	       ,@(deorify
		  (generate second env cont))))
    (else
     (let ((first-code (generate first env cont/value)))
       (if (car-is? first-code 'ps:true?)
	   ;; This assumes that #t = t.
	   `(ps-lisp:or ,(cadr first-code)
		     ,@(deorify (generate second env cont)))
	   (let* ((var (make-local-variable 'temp))
		  (new-name (cl-externalize-local 'temp env))
		  (new-env (bind-variables (list var) (list new-name) env)))
	     `(ps-lisp:let ((,new-name ,first-code))
		(ps-lisp:if (ps:truep ,new-name)
			 ,(deliver-value-to-cont new-name cont)
			 ,(generate second new-env cont)))))))))

(define (generate-case key key-lists else-thunk thunks env cont)
  `(ps-lisp:case ,(generate key env cont/value)
     ,@(map (lambda (key-list thunk)
	      `(,key-list
		,@(deprognify (generate (if (lambda? thunk)
					    (lambda-body thunk)
					    (make-call thunk '()))
					env
					cont))))
	    (if (constant? key-lists)
		(constant-value key-lists)
		(error "case: invalid key-lists" key-lists))
	    thunks)
     (ps-lisp:otherwise
      ,@(deprognify (generate (if (lambda? else-thunk)
				  (lambda-body else-thunk)
				  (make-call else-thunk '()))
			      env
			      cont)))))

(define (generate-=> test var then else-thunk env cont)
  (let* ((new-name (cl-externalize-local (local-variable-name var) env))
	 (new-env (bind-variables (list var) (list new-name) env)))
    `(ps-lisp:let ((,new-name ,(generate test env cont/value)))
       (ps-lisp:if (ps:truep ,new-name)
		,(generate then new-env cont)
		,(generate (if (lambda? else-thunk)
			       (lambda-body else-thunk)
			       (make-call else-thunk '()))
			   new-env
			   cont)))))

(define (generate-call-to-unknown pvar args env cont)
  ;; Go through scheme symbol's function cell
  (let ((CL-sym (program-variable-CL-symbol pvar))
	(args-code (generate-list args env)))
    (deliver-value-to-cont
     (if (and (not (qualified-symbol? CL-sym))
	      (defined-as-CL-macro? CL-sym))
	 ;; Prevent infinite compilation loops!
	 `(ps-lisp:funcall ,CL-sym ,@args-code)
	 `(,CL-sym ,@args-code))
     cont)))

; LAMBDA

(define (generate-lambda node env cont)
  (set-fluid! @lambda-encountered? #t)
  (deliver-value-to-cont
     `(ps-lisp:function (ps-lisp:lambda ,@(generate-lambda-aux node env cont/value)))
     cont))

; Returns (bvl . body)
(define (generate-lambda-aux node env cont)
  (let* ((bvl (lambda-vars node))
	 (vars (proper-listify bvl))
	 (new-names (cl-externalize-locals vars env))
	 (new-env (bind-variables vars new-names env))
	 (body-code (generate-body (lambda-body node) new-env cont)))
    (if (n-ary? node)
	(let* ((bvl (insert-&rest new-names))
	       (rest-var (car (last-pair bvl))))
	  `(,bvl
	    (ps:maybe-fix-&rest-parameter ,rest-var)
	    ,@body-code))
	`(,new-names ,@body-code))))

(define (generate-let proc args env cont)
  (let ((vars (lambda-vars proc)))
    (if (function-bindable? vars args)
	(let* ((new-names (cl-externalize-locals vars env))
	       (new-env (bind-functions vars new-names env)))
	  `(ps-lisp:flet ,(map (lambda (new-name proc)
			       `(,new-name
				 ,@(generate-lambda-aux proc env cont/value)))
			    new-names
			    args)
	     ,@(generate-body (lambda-body proc) new-env cont)))
	(let ((bvl+body (generate-lambda-aux proc env cont)))
	  `(ps-lisp:let ,(map list (car bvl+body) (generate-list args env))
	     ,@(cdr bvl+body))))))

; IF

(define (generate-if node env cont)
  (let ((test (generate (if-test node) env cont/test))
	(con  (generate (if-con node) env cont))
	(alt  (generate (if-alt node) env cont)))
    ;;+++ Reconstruct COND, WHEN, UNLESS ?
    (if (and (eq? alt 'ps:unspecific)
	     (or (eq? (continuation-type cont) 'cont/ignore)
		 (fluid @translating-to-file?)))
	`(ps-lisp:if ,test ,con)		;Make prettier code
	`(ps-lisp:if ,test ,con ,alt))))

; BEGIN

(define (generate-begin node env cont)
  (prognify (append (deprognify (generate (begin-first node) env cont/ignore))
		    (deprognify (generate (begin-second node) env cont)))))

; SET!

(define (generate-set! node env cont)
  (let ((var (set!-lhs node))
	(rhs-code (generate (set!-rhs node) env cont/value)))
    (cond ((program-variable? var)
	   (if (get-integration var)
	       (note "SET! of an integrated variable" node))
	   (let ((CL-sym (program-variable-CL-symbol var)))
	     (note-variable-reference! var)
	     (deliver-value-to-cont
	      (emit-program-variable-set! var CL-sym rhs-code)
	      cont)))
	  (else
	   (let ((the-setq
		  `(ps-lisp:setq ,(variable-substitution var) ,rhs-code)))
	     (if (eq? (continuation-type cont) 'cont/ignore)
		 the-setq
		 `(ps-lisp:progn ,the-setq
			      ,(deliver-value-to-cont `ps:unspecific
						      cont))))))))

; LETREC

(define (generate-letrec node env cont)
  (case (get-letrec-strategy node)
    ((general) (generate-general-letrec node env cont))
    ((labels)  (generate-labels-letrec node env cont))
    ((prog)    (generate-prog-letrec node env cont))
    (else (error "unknown strategy" (get-letrec-strategy node)))))

(define (generate-general-letrec node env cont)
  (let* ((vars (letrec-vars node))
	 (vals (letrec-vals node))
	 (new-names (cl-externalize-locals vars env))
	 (new-env (bind-variables vars new-names env)))
    `(ps-lisp:let ,(map (lambda (new-name)
		       `(,new-name ps:unassigned))
		     new-names)
       ,@(map (lambda (var val)
		`(ps-lisp:setq ,var ,(generate val new-env cont/value)))
	      new-names
	      vals)
       ,@(deprognify (generate (letrec-body node) new-env cont)))))

(define (generate-labels-letrec node env cont)
  (let* ((vars (letrec-vars node))
	 (vals (letrec-vals node))
	 (new-names (cl-externalize-locals vars env))
	 (new-env (bind-functions vars new-names env)))
    `(ps-lisp:labels ,(map (lambda (new-name proc)
			   `(,new-name
			     ,@(generate-lambda-aux proc new-env cont/value)))
			new-names
			vals)
       ,@(generate-body (letrec-body node) new-env cont))))

; Sorry, I guess this is pretty hairy.  So it goes.
; It would certainly be cleaner if there were a separate pass that
; transformed the code tree to change argument passing into assignment.

(define (generate-prog-letrec node env cont)
  (let* ((vars (letrec-vars node))
	 (procs (letrec-vals node))
	 (new-names (cl-externalize-locals vars env))
	 (new-env (bind-variables vars new-names env))
	 (temp-lists
	  (map (lambda (proc)
		 (map (lambda (var)
			(if (variable-closed-over? var)
			    (make-name-from-uid (local-variable-name var)
						(generate-uid))
			    #f))
		      (lambda-vars proc)))
	       procs))
	 (proc-new-nameses (map (lambda (proc)
				 (cl-externalize-locals (lambda-vars proc)
							new-env))
			       procs))
	 (proc-envs (map (lambda (proc new-names)
			   (bind-variables (lambda-vars proc)
					   new-names new-env))
			 procs
			 proc-new-nameses)))
    (for-each set-letrec-substitution!
	      vars new-names proc-new-nameses temp-lists)
    (deliver-value-to-cont		;Suboptimal.
     `(ps-lisp:prog ,(apply append (map (lambda (temp-list new-names)
				       (map (lambda (temp new-name)
					      (or temp new-name))
					    temp-list
					    new-names))
				     temp-lists
				     proc-new-nameses))
	,@(generate-body (letrec-body node) new-env cont/return)
	,@(apply append
		 (map (lambda (new-name proc temp-list proc-new-names proc-env)
			`(,new-name
			  ,(letify (filter cadr
					   (map list proc-new-names temp-list))
				   (generate (lambda-body proc)
					     proc-env
					     cont/return))))
		      new-names procs temp-lists proc-new-nameses proc-envs)))
     cont)))

(define (set-letrec-substitution! var new-name proc-new-names temp-list)
  (set-substitution!
   var
   (list '--generate-call--
	 (lambda (args cont)
	   ;; Return a CL expression to do the call.
	   ;; Args are already translated.
	   (if (not (eq? (continuation-type cont) 'cont/return))
	       (note "screwed-up LETREC" cont))
	   (if (null? args)
	       `(ps-lisp:go ,new-name)
	       `(ps-lisp:progn
		 ;; If we had free-variable information, we could
		 ;; optimize this PSETQ into a SETQ, sometimes.
		 (,(if (null? (cdr args)) 'ps-lisp:setq 'ps-lisp:psetq)
		  ,@(apply append
			   (map (lambda (new-name temp actual)
				  `(,(or temp new-name) ,actual))
				proc-new-names
				temp-list
				args)))
		 (ps-lisp:go ,new-name)))))))
