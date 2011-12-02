; -*- Mode: Scheme; Syntax: Scheme; Package: Scheme; -*-
; File ssig.scm / See file COPYING

;;;; Scheme interfaces

; The separation into core and non-core is very tentative for now.
; Could be split into two different interfaces at some point.

(define revised^4-scheme-interface
  (make-interface
   'revised^4-scheme
   '(
     ;; Syntax
     and begin case cond define
     delay do if lambda letrec let let* or
     quasiquote quote set! unquote unquote-splicing 
     define-syntax let-syntax letrec-syntax   ;New stuff
     syntax-rules			;Pattern-based macro definer
     ;; for-syntax ?
     => else

     ;; Core procedures
     * + - / < <= = > >=
     apply
     boolean?
     call-with-current-continuation
     car cdr
     char->integer
     char-ready?
     close-input-port close-output-port
     complex? cons current-input-port current-output-port
     denominator
     eof-object? eq? eqv? exact->inexact exact?
     imag-part inexact->exact inexact? input-port?
     integer->char integer?
     load
     magnitude make-polar make-rectangular make-string
     make-vector modulo
     newline not null? number? 
     numerator
     open-input-file open-output-file output-port?
     pair? peek-char procedure?
     quotient
     rational? read-char real-part real?
     remainder
     set-car! set-cdr!
     string
     string->symbol 
     string-length string-ref string-set!
     string? symbol->string symbol?
     transcript-on transcript-off
     vector-length vector-ref vector-set! vector?
     write-char

     ;; Non-core procedures (definable in terms of core)
     append assoc assq assv
     abs acos angle
     asin atan
     caaaar caaadr caadar caaddr caaar caadr caar
     cadaar cadadr caddar cadddr cadar caddr cadr
     call-with-input-file
     call-with-output-file
     cdaaar cdaadr cdadar cdaddr cdaar cdadr cdar
     cddaar cddadr cdddar cddddr cddar cdddr cddr
     ceiling
     char-alphabetic?
     char-ci<=? char-ci<? char-ci=? char-ci>=? char-ci>?
     char-downcase char-lower-case? char-numeric?
     char-upcase
     char-upper-case? char-whitespace? char<=? char<? char=?
     char>=? char>? char?
     cos
     display
     equal? even? exp expt
     floor for-each force
     gcd
     lcm list log
     length list->string list->vector list-ref list-tail
     list?
     map max member memq memv min
     negative? number->string
     odd?
     rationalize
     positive? read reverse round sin sqrt 
     string->list string->number string-append
     string-ci<=? string-ci<? string-ci=? string-ci>=? string-ci>?
     string-copy string-fill!
     string<=? string<? string=? string>=? string>? substring
     tan truncate
     vector vector->list vector-fill! with-input-from-file
     with-output-to-file write zero? 

     ;; Revised^5 stuff
     values call-with-values dynamic-wind
     eval interaction-environment scheme-report-environment
     )
   ;; Private variables
   '(and-aux case-aux make-promise or-aux
     unspecific =>-aux
     )))
