
; Emulate Scheme 48's sockets (and various other things) in Common Lisp.

#-LispWorks
(define (open-socket portno)
  ;; #+LispWorks (comm::create-tcp-socket-for-service portno)
  #+LispWorks 
  (begin
   (cl:format 'cl:t "~&Foreground TCP service not available in LispWorks.~
                     ~%Try ~S instead.~%" 
	      `(spawn-server))     ;cf. flow/network.scm
   #f)
  #+Allegro
  (socket:make-socket :connect :passive :local-port portno)
  )

#-LispWorks
(define (close-socket sock)
  
  #+Allegro 'we-dont-need-no-close-here ;(lisp:close sock)
   )

#-LispWorks
(define (socket-listen sock)
  (let ((str ;#+LispWorks (comm::accept-connection-to-socket sock)
	     #+Allegro   (socket:accept-connection sock)))
    (values str str)))

(define (socket-client host portno)
  (let ((str #+LispWorks (comm:open-tcp-stream host portno :direction :io)
	     #+Allegro   (socket:make-socket :remote-host host :remote-port portno)))
    ;; It happens when we can't open a port that we get back NIL.
    ;; Trap that here for want of proceeding and reading from stream NIL.
    ;; Do something better later. -kmp 25-Apr-1999
    #+LispWorks (cl:check-type str (cl:not cl:null))
    (values str str)))


(define (start-up-server my-port proc)
  #+LispWorks
  (let ((the-server #f))
    (set! the-server
	  (comm:start-up-server 
	    :service my-port
	    :function
	           (lambda (handle)
		     (let ((stream (cl:make-instance 'comm:socket-stream
						     :socket handle
						     :direction :io
						     :element-type
						     'cl:base-char)))
		       (if (eq? (cl:unwind-protect (proc stream stream) (cl:close stream))  'shutdown)
			   (mp:process-kill the-server))))))
    the-server)
  #-LispWorks
  ;; This obviously won't work.  Fix later.
  (spawn #'(lambda ()
	     (call-with-socket my-port
	       #'(lambda (sock)
		   (format *debug-io* "~&Serving ERA connections on port ~s~%"
			   my-port)
		   (loop
		    ;; foo -- cf. http-util.scm
		    (let ((a (call-with-server-ports sock proc)))
		      (when (eq a 'shutdown)
			    (return 'done)))))))
	 "Server"))


;;; THESE DON'T BELONG HERE, but I don't want to create new files...

(define (force-output oport)
  (cl:finish-output oport))

(define (error? x) (ps:true? (cl:typep x 'cl:error)))

#+LispWorks
(map 'cl:eval '(

(cl:defun with-handler (handler thunk)
  (cl:handler-bind ((cl:condition #'(cl:lambda (condition)
				      (cl:funcall handler 
						  condition 
						  (cl:lambda () 
						    (cl:signal condition))))))
    (cl:funcall thunk)))
(ps:set-value-from-function 'with-handler)

));pam


(define (display-condition x oport) 
  (lisp:format oport "~&~A~&" x))

(define (disclose-condition x) (cl:princ-to-string x))
  

; CALL-WITH-STRING-OUTPUT-PORT

(map 'cl:eval '(

(cl:defun call-with-string-output-port (proc)
  (cl:with-output-to-string (str)
    (cl:funcall proc str)))
(ps:set-value-from-function 'call-with-string-output-port)

));pam

; Hmm.  What if machine-instance is already fully qualified?
; Should see if it contains a "." and omit suffix if so.

(define local-host-name #f)

(define local-host-name-is-localhost #f)

(define (initialize-local-host-name)
  (set! local-host-name 
	(if local-host-name-is-localhost
	    "localhost"
	    (string-append (cl:string-downcase (cl:machine-instance))
			   ".crystaliz.com"))))

(initialize-local-host-name) ;might need to be redone if image dumped!!


