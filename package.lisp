(defpackage unix-sockets
  (:use #:cl
        #:trivial-gray-streams)
  (:export #:make-unix-socket
           #:close-unix-socket
           #:connect-unix-socket
           #:shutdown-unix-socket
           #:unix-socket-error
           #:unix-socket-stream
           #:accept-unix-socket
           #:with-unix-socket
	   #:fd))
