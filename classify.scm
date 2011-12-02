; Expression classification

; Entry points (not a complete list?):
;  classify
;  process-define-syntax
;  classify-let-syntax, classify-letrec-syntax
;  bind            ; for let, lambda
;  lookup	   ; for variable references
;  define!	   ; for define (internal and otherwise)
;  scan-body
;  make-special-operator  ; for initialization

;  classify : form * env -> class * form * env
;  env = name -> denotation
;  denotation = special + macro + variable
;  special = {begin, define, if, let-syntax, ...}
;  variable = [defined elsewhere]
;  macro = transformer * env
;  transformer = form * (name -> name) * (name * name -> bool) -> form

; A "form" is an expression, definition, or (syntax ...) form.

; Classify FORM in ENV, returning three values.
; This dispatches on FORM: it is either a literal, a name, a compound
; expression, or an already classified form.

(define (classify form env)
  (cond ((name? form)
	 (values class/name form env))
	((pair? form)
	 (if (name? (car form))
	     (let ((den (lookup env (car form))))
	       (cond ((special-operator? den)
		      (classify-special den form env))
		     ((macro? den)
		      (classify-macro-application den form env))
		     (else
		      (values class/application form env))))
	     (values class/application form env)))
	((literal? form)
	 (values class/literal form env))
	(else (classify (syntax-error "unknown expression type" form) env))))

(define (classify-special den form env)
  (let ((class (special-operator-class den)))
    (if (check-special-form-syntax class form)
	(cond ((= class class/let-syntax)
	       (classify-let-syntax form env))
	      ((= class class/letrec-syntax)
	       (classify-letrec-syntax form env))
	      (else
	       (values class form env)))
	(classify
	 (syntax-error "invalid special form syntax"
		       form)
	 env))))

;==============================================================================
; Macro application
; A macro has both the environment in which the macro was defined
; and an expansion procedure.  The expansion procedure is called on
; the form, a renaming procedure, and a procedure for comparing
; denotations in the current environment with the definition
; environment (which is to allow the for the overriding of keywords:
; (LET ((ELSE #F)) (COND (ELSE 1) (#T 2))) => 2).

(define (classify-macro-application den form use-env)
  (let ((def-env (macro-environment den)))
    (call-with-values (lambda () (make-renamer+env def-env use-env))
      (lambda (rename output-env)
	(let* ((compare
		(lambda (client-name macro-name)
		  (if (and (name? client-name)
			   (name? macro-name))
		      (same-denotation? (lookup output-env client-name)
					(lookup output-env macro-name))
		      (eq? client-name macro-name))))
	       (new-form
		((macro-transformer den) form rename compare)))
	  (classify new-form output-env))))))

; Macro abstraction.

(define (process-syntax-spec sspec env)
  (let* ((frob (eval-for-syntax sspec
				(get-environment-for-syntax env)))
	 (proc (if (pair? frob) (car frob) frob)))
    (if (procedure? proc)
	(make-macro proc env)
	(begin (warn "<thing> in (define-syntax foo <thing>) isn't a procedure" sspec)
	       (make-macro (lambda rest
			     (error "<thing> in (define-syntax foo <thing>) isn't a procedure"
				    sspec rest))
			   env)))))

; Process a define-syntax encountered at program top level.

(define (process-define-syntax form env)
  (define! env (cadr form) (process-syntax-spec (caddr form) env)))

;==============================================================================
; let-syntax and letrec-syntax
; Classifiers for the two forms that introduce local syntax bindings.
; These classify the bodies of the forms in the appropriately extended
; environment.

(define (classify-let-syntax form env)
  (let ((dspecs (let-syntax-form-dspecs form)))
    (classify (let-syntax-form-body form)
	      (bind (map syntax-spec-name dspecs)
		    (map (lambda (dspec)
			   (process-syntax-spec (syntax-spec-form dspec) env))
			 dspecs)
		    env))))

(define (classify-letrec-syntax form outer-env)
  (let ((new (new-environment outer-env)))
    (for-each (lambda (dspec)
		(define! new
		  (syntax-spec-name dspec)
		  (process-syntax-spec (syntax-spec-form dspec) new)))
	      (letrec-syntax-form-dspecs form))
    (classify (letrec-syntax-form-body form) new)))

;==============================================================================
; Environments

; Environments come in three varieties:
;  - local (lambda, letrec, let-syntax) bindings
;      represented as #(LOCAL outer ((name1 . den1) ...))
;  - diversion environments, for expansions of macros
;      represented as #(DIVERT outer generation env)
;  - other - classify's client determines representation and semantics.

(define (lookup env name)
  (cond ((local-environment? env)
	 (let ((probe (assq name (local-environment-bindings env))))
	   (if probe
	       (cdr probe)
	       (lookup (local-environment-parent env) name))))
	((diverted-environment? env)
	 (if (and (generated? name)
		  (same-generation? (generated-generation name)
				    (diverted-environment-generation env)))
	     (lookup (diverted-environment-macro-env env)
		     (generated-name name))
	     (lookup (diverted-environment-parent env) name)))
	(else
	 (client-lookup env name))))

(define (define! env name denotation)
  (cond ((local-environment? env)
	 (let* ((bs (local-environment-bindings env))
		(probe (assq name bs)))
	   (if probe
	       (set-cdr! probe denotation)
	       (set-local-environment-bindings!
		  env (cons (cons name denotation) bs)))))
	((diverted-environment? env)
	 ;; Not quite right.  Consider a macro that expands into
	 ;;  (define <generated> ... <generated> ...)
	 (define! (diverted-environment-parent env) name denotation))
	(else
	 (client-define! env name denotation))))

(define (ensure-defined env name)
  (cond ((local-environment? env)
	 (let* ((bs (local-environment-bindings env))
		(probe (assq name bs)))
	   (if probe
	       (cdr probe)
	       (begin (set-local-environment-bindings!
		         env (cons (cons name #t) bs))
		      #t))))
	((diverted-environment? env)
	 (ensure-defined (diverted-environment-parent env) name))
	(else
	 (client-ensure-defined env name))))

(define local-environment-rtd
  (make-record-type 'local-environment '(parent bindings)))
(define make-local-environment
  (record-constructor local-environment-rtd '(parent bindings)))
(define local-environment?
  (record-predicate local-environment-rtd))
(define local-environment-parent
  (record-accessor local-environment-rtd 'parent))
(define local-environment-bindings
  (record-accessor local-environment-rtd 'bindings))
(define set-local-environment-bindings!
  (record-modifier local-environment-rtd 'bindings))

(define diverted-environment-rtd
  (make-record-type 'diverted-environment '(parent generation macro-env)))
(define make-diverted-environment
  (record-constructor diverted-environment-rtd
		      '(generation macro-env parent)))
(define diverted-environment? (record-predicate diverted-environment-rtd))
(define diverted-environment-parent
  (record-accessor diverted-environment-rtd 'parent))
(define diverted-environment-generation
  (record-accessor diverted-environment-rtd 'generation))
(define diverted-environment-macro-env
  (record-accessor diverted-environment-rtd 'macro-env))

; bind

(define (bind names denotations outer-env)
  (make-local-environment outer-env (map cons names denotations)))

; Bindings to be stored using define!

(define (new-environment outer-env)
  (make-local-environment outer-env '()))


; Apply proc to each local variable in a given environment

(define (for-each-local proc env)
  (let ((doit (lambda (name+den)
		(let ((den (cdr name+den)))
		  (if (and (not (macro? den))
			   (not (special-operator? den)))
		      (proc den))))))
    (let loop ((env env))
      (cond ((local-environment? env)
	     (for-each doit (local-environment-bindings env))
	     (loop (local-environment-parent env)))
	    ((diverted-environment? env)
	     (loop (diverted-environment-parent env)))))))

;==============================================================================
; Denotations

; Denotation = special operator + macro + variable
; All of these can be compared using EQ?.

(define same-denotation? eq?)

; Special operators

(define type/special-operator (make-record-type "Special operator" '(class)))
(define make-special-operator
  (record-constructor type/special-operator '(class)))
(define special-operator?      (record-predicate type/special-operator))
(define special-operator-class (record-accessor type/special-operator 'class))

; Macros

(define type/macro (make-record-type "Macro" '(proc env)))
(define make-macro (record-constructor type/macro '(proc env)))
(define macro?            (record-predicate type/macro))
(define macro-transformer (record-accessor type/macro 'proc))
(define macro-environment (record-accessor type/macro 'env))

; Implementation of variables is specific to a particular client of
; the classifier.

;==============================================================================
; Names

(define (name? thing)
  (or (symbol? thing)
      (generated? thing)))

(define same-name? eq?)
(define name-member memq)
(define name-assoc assq)

(define (name->symbol name)
  (if (symbol? name)
      name
      (string->symbol (name->string name))))
       
(define (name->string name)
  (if (symbol? name)
      (symbol->string name)
      (string-append "."
		     (name->string (generated-name name))
		     "."
		     (number->string (generated-generation name)))))
      

; Generated names <name, generation>

(define type/generated (make-record-type "Generated" '(name generation)))
(define make-generated (record-constructor type/generated '(name generation)))
(define generated?      (record-predicate type/generated))
(define generated-name  (record-accessor type/generated 'name))
(define generated-generation (record-accessor type/generated 'generation))

(define-record-discloser type/generated
  (lambda (g)
    (list 'generated (generated-name g) (generated-generation g))))

; Create a new version of THING with all generated parts replaced with their
; names.  Pairs and vectors are recursively ungenerated.  This is for use in
; processing the QUOTE special form.

(define (strip thing)
  (cond ((generated? thing) (strip (generated-name thing)))
	((pair? thing)
	 (let ((x (strip (car thing)))
	       (y (strip (cdr thing))))
	   (if (and (eq? x (car thing))
		    (eq? y (cdr thing)))
	       thing
	       (cons x y))))
	((vector? thing)
	 (let ((new (make-vector (vector-length thing))))
	   (let loop ((i 0) (same? #t))
	     (if (>= i (vector-length thing))
		 (if same? thing new)
		 (let ((x (strip (vector-ref thing i))))
		   (vector-set! new i x)
		   (loop (+ i 1)
			 (and same? (eq? x (vector-ref thing i)))))))))
	(else thing)))

; Generated names are differentiated by their name and their generation.
; Generations are integers.

(define *generation* 1)

(define (new-generation)
  (set! *generation* (+ *generation* 1))
  *generation*)

(define same-generation? =)

(define (make-renamer+env macro-env client-env)
  (let ((alist '())			;list of name * generated
	(generation (new-generation)))
    (values (lambda (name)
	      (let ((probe (assq name alist)))
		(if probe
		    (cdr probe)
		    (let ((new-name (make-generated name generation)))
		      (set! alist (cons (cons name new-name)
					alist))
		      new-name))))
	    (make-diverted-environment generation macro-env client-env))))


;==============================================================================
; Processing internal definitions

; Three values:
;   definitions - a list of
;       (<defined name> <value-form> <environment>)
;     where <environment> is the environment in which <value-form>
;     should be classified
;   body-forms - a list of expressions
;   env - environment in which names should be define!d

(define (scan-body forms outer-env)

  (define env (new-environment outer-env))

  (define (scan-form form form-env)
    (call-with-values (lambda () (classify form form-env))
      (lambda (class form form-env)
	(cond ((= class class/define)
	       (ensure-defined env (define-form-lhs form))
	       (list (list (define-form-lhs form)
			   (define-form-rhs form)
			   form-env)))
	      ((= class class/begin)
	       (let ((stmts (begin-form-statements form)))
		 (if (null? stmts)
		     '()
		     (let ((first (scan-form (car stmts) form-env)))
		       (if first
			   (apply append
				  first
				  (map (lambda (form)
					 (or (scan-form form form-env)
					     (list (syntax-error "definitions and expressions are intermixed"
								 forms))))
				       (cdr stmts)))
			   #f)))))
	      (else
	       #f)))))

  (let loop ((forms forms) (specs '()))
    (if (null? forms)
	(values specs (list (syntax-error "null body" forms)) env)
	(let ((probe (scan-form (car forms) env)))
	  (if probe
	      (loop (cdr forms)
		    (append probe specs))
	      (values specs forms env))))))

; (put 'call-with-values 'scheme-indent-hook 1)
