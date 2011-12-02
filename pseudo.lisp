
(in-package "USER")

(load "/zu/jar/pseudo/loadit.lisp")

(load-pseudoscheme "/zu/jar/pseudo/")

(format t
	"~2&Do (ps:scheme) to enter Scheme, after which:~
	 ~%  (benchmark-mode) - have calls to primitives open-coded~
	 ~%  (compile-file \"foo.scm\") - compile a file of Scheme code~
	 ~%  (quit) - return to Common Lisp~
	 ~%  ## is last value displayed~
	 ~%  #'lisp:foo evaluates to Common Lisp function foo~
	 ~%All of Revised^4.5 Scheme is available, including define-syntax,~
	 ~%values, dynamic-wind, and eval, but not things that are too~
	 ~%difficult to do in Common Lisp, e.g. upward continuations and~
	 ~%proper tail-recursion in all possible circumstances.  See the~
	 ~%user guide for more information.~%")
