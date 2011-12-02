; Compatibility mode for use with Abelson & Sussman's book,
; Structure & Interpretation of Computer Programs.
; This is intended to be loaded into Pseudoscheme.

(define-syntax cons-stream
  (lambda (e r c) `(,(r 'cons) ,(cadr e) (,(r 'delay) ,(caddr e)))))

(define head car)
(define (tail s) (force (cdr s)))
(define the-empty-stream '<the-empty-stream>)
(define (empty-stream? s) (eq? s the-empty-stream))

(define-syntax sequence
  (lambda (e r c) `(,(r 'begin) ,@(cdr e))))

(define t #t)
(define nil #f)
(define (atom? x) (not (pair? x)))

(define (print x)
  (write x)
  (newline))
(define princ display)
(define prin1 write)

(define (explode thing)
  (map (lambda (c) (string->symbol (string c)))
       (string->list (cond ((symbol? thing)
			    (symbol->string thing))
			   ((number? thing)
			    (number->string thing))
			   (else
			    (error "invalid argument to explode" thing))))))

(define (implode l)
  (string->symbol (list->string (map (lambda (s)
				       (string-ref (symbol->string s) 0))
				     l))))

(define mapcar map)
(define mapc for-each)

(define (1+ x) (+ x 1))
(define (-1+ x) (- x 1))

(define (get sym ind)
  (ps-lisp:or (ps-lisp:get sym ind) #f))

(define (put sym ind val)
  (ps-lisp:setf (ps-lisp:get sym ind) val))


; AND and OR are procedures according to SICP.  Replace references
; as needed with *AND and *OR.

(define (*and . rest)
  (let loop ((rest rest))
    (if (null? rest)
	#t
	(and (car rest) (loop (cdr rest))))))

(define (*or . rest)
  (let loop ((rest rest))
    (if (null? rest)
	#f
	(or (car rest) (loop (cdr rest))))))
