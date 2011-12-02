
; Scheme48's WRITE module, adapted for use in Pseudoscheme.
; (compile-file "~/pseudo/write" scheme-translator::revised^4-scheme-env)
; (load "~/pseudo/write" scheme-translator::revised^4-scheme-env)
; (define write revised^4-scheme:.write)
; (define display revised^4-scheme:display)

; Problem: symbols come out in upper case.


(define (output-port-option port-option)
  (cond ((null? port-option) (current-output-port))
	((null? (cdr port-option)) (car port-option))
	(else (ps:scheme-error "write-mumble: too many arguments"
				 port-option))))

(define (disclose obj)
  obj ;ignored
  #f)

(define (write-string string port)
  (ps-lisp:princ string port))


; -*- Mode: Scheme; Syntax: Scheme; Package: Scheme; -*-
; Copyright (c) 1993 by Richard Kelsey and Jonathan Rees.  See file COPYING.


; This is file write.scm.

;;;; WRITE

; To use this with some Scheme other than Scheme48, do the following:
;  1. Copy the definition of output-port-option from port.scm
;  2. Define write-string as appropriate (as a write-char loop)
;  3. (define (disclose x) #f)

(define (scheme-write obj . port-option)
  (let ((port (output-port-option port-option)))
    (let recur ((obj obj))
      (recurring-write obj port recur))))

(define (recurring-write obj port recur)
  (cond ((null? obj) (write-string "()" port))
        ((pair? obj) (write-list obj port recur))
        ((eq? obj #t) (write-boolean 't port))
        ((eq? obj #f) (write-boolean 'f port))
        ((symbol? obj) (write-symbol obj port))
        ((number? obj) (write-number obj port))
        ((string? obj) (write-string-literal obj port))
        ((char? obj) (write-char-literal obj port))
	(else (write-other obj port recur))))

(define (write-symbol obj port)
  (write-string (symbol->string obj) port)) ;downcase or upcase if desired

(define (write-boolean mumble port)
  (write-char #\# port)
  (write-symbol mumble port))

(define (write-number n port)
  (write-string (number->string n 10) port))

(define (write-char-literal obj port)
  (let ((probe (character-name obj)))
    (write-string "#\\" port)
    (if probe
	(write-symbol probe port)
	(write-char obj port))))

(define (character-name char)
  (cond ((char=? char #\space) 'space)
        ((char=? char #\newline) 'newline)
	(else #f)))

(define (write-string-literal obj port)
  (write-char #\" port)
  (let ((len (string-length obj)))
    (do ((i 0 (+ i 1)))
	((= i len) (write-char #\" port))
      (let ((c (string-ref obj i)))
	(if (or (char=? c #\\) (char=? c #\"))
	    (write-char #\\ port))
	(write-char c port)))))

(define (write-list obj port recur)
  (cond ((quotation? obj)
         (write-char #\' port)
         (recur (cadr obj)))
        (else
         (write-char #\( port)
         (recur (car obj))
         (let loop ((l (cdr obj))
                    (n 1))
              (cond ((not (pair? l))
                     (cond ((not (null? l))
                            (write-string " . " port)
                            (recur l))))
                    (else
                      (write-char #\space port)
                      (recur (car l))
                      (loop (cdr l) (+ n 1)))))
         (write-char #\) port))))

(define (quotation? obj)
  (and (pair? obj)
       (eq? (car obj) 'quote)
       (pair? (cdr obj))
       (null? (cddr obj))))

(define (write-vector obj port recur)
   (write-string "#(" port)
   (let ((z (vector-length obj)))
     (cond ((> z 0)
            (recur (vector-ref obj 0))
            (let loop ((i 1))
              (cond ((>= i z))
                    (else
                     (write-char #\space port)
                     (recur (vector-ref obj i))
                     (loop (+ i 1))))))))
   (write-char #\) port))

; The vector case goes last just so that this version of WRITE can be
; used in Scheme implementations in which records, ports, or
; procedures are represented as vectors.  (Scheme48 doesn't have this
; property.)

(define (write-other obj port recur)
  (cond ((disclose obj)
	 => (lambda (l)
	      (write-string "#{" port)
	      (display-type-name (car l) port)
	      (for-each (lambda (x)
			  (write-char #\space port)
			  (recur x))
			(cdr l))
	      (write-string "}" port)))
	((procedure? obj) (write-string "#{Procedure}" port))
	((input-port? obj)  (write-string "#{Input-port}" port))
	((output-port? obj) (write-string "#{Output-port}" port))
	((eof-object? obj) (write-string "#{End-of-file}" port))
	((vector? obj) (write-vector obj port recur))
	((eq? obj (if #f #f)) (write-string "#{Unspecific}" port))
	(else
	 ;; (write-string "#{Random object}" port)
	 (ps-lisp:prin1 obj port)
	 )))

; Display the symbol WHO-CARES as Who-cares.

(define (display-type-name name port)
  (if (symbol? name)
      (let* ((s (symbol->string name))
	     (len (string-length s)))
	(if (and (> len 0)
		 (char-alphabetic? (string-ref s 0)))
	    (begin (write-char (char-upcase (string-ref s 0)) port)
		   (do ((i 1 (+ i 1)))
		       ((>= i len))
		     (write-char (char-downcase (string-ref s i)) port)))
	    (display name port)))
      (display name port)))

;(define (write-string s port)
;  (do ((i 0 (+ i 1)))
;      ((= i (string-length s)))
;    (write-char (string-ref s i) port)))



; DISPLAY

(define (scheme-display obj . port-option)
  (let ((port (output-port-option port-option)))
    (let recur ((obj obj))
      (cond ((string? obj) (write-string obj port))
	    ((char? obj) (write-char obj port))
	    (else
	     (recurring-write obj port recur))))))

(ps-lisp:setq ps:*scheme-write* scheme-write)
(ps-lisp:setq ps:*scheme-display* scheme-display)
