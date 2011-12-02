
; Scheme 48's table package, replicated for Pseudoscheme.
; Somewhat redundant with p-utils.scm.

(define (make-table) (lisp:make-hash-table))

(define (table-ref table key)
  (let ((probe (lisp:gethash key table)))
    (lisp:if probe
	     probe
	     #f)))

(define (table-set! table key val)
  (lisp:setf (lisp:gethash key table) val))

(define (table-walk proc table)
  (lisp:maphash proc table))
