(in-package :unix-sockets)

(let ((output (asdf:output-file 'asdf:compile-op
                                (asdf:find-component :unix-sockets "unix_sockets"))))
  #-lispworks
  (cffi:load-foreign-library
   output)

  ;; This is LW-specific impl is probably not required, I'm using this
  ;; name in an external app to fli:disconnect-module
  #+lispworks
  (fli:register-module :unix-sockets
                       :real-name output))

(cffi:defcstruct sockaddr-un)

(cffi:defcfun "socket" :int
  (domain :int)
  (type :int)
  (protocol :int))

(cffi:defcfun "bind" :int
  (fd :int)
  (sockaddr (:pointer (:struct sockaddr-un))) ;; lies
  (addr-len :unsigned-int))

(cffi:defcfun "connect" :int
  (fd :int)
  (sockaddr (:pointer (:struct sockaddr-un))) ;; lies
  (addr-len :unsigned-int))

(cffi:defcfun "strerror" :string
  (errnum :int))

(cffi:defcfun ("listen" %listen) :int
  (fd :int)
  (backlog :int))

(cffi:defcfun ("accept" %accept) :int
  (fd :int)
  (sockaddr (:pointer :void))
  (addr-len (:pointer :void)))

(cffi:defcfun ("write" %write) :int
  (fd :int)
  (buf (:pointer :unsigned-char))
  (count :unsigned-int))

(cffi:defcfun ("recv" %recv) :int
  (fd :int)
  (buf (:pointer :unsigned-char))
  (count :unsigned-int)
  (flags :int))

(cffi:defcfun ("shutdown" %shutdown) :int
  (fd :int)
  (how :int))

(cffi:defcfun "unix_socket_is_ready" :boolean
  (fd :int))

(cffi:defcfun "unix_socket_make_sockaddr" (:pointer sockaddr-un)
  (path :string))

(cffi:defcfun ("unix_socket_errno" %unix-socket-errno)
    :int)

(cffi:defcfun ("close" %close) :void
  (fd :int))

(cffi:defcfun "unix_socket_sockaddr_size" :int)

(defun errno ()
  #-lispworks
  (%unix-socket-errno)
  #+lispworks
  (lw:errno-value))
