(in-package :unix-sockets)

(defun native-so ()
  (asdf:output-file 'asdf:compile-op
                    (asdf:find-component :unix-sockets "unix_sockets")))

(let ((output (native-so)))
  #-lispworks
  (cffi:load-foreign-library
   output)

  #+lispworks
  (fli:register-module :unix-sockets
                       :real-name output))

#+lispworks
(unless (hcl:delivered-image-p)
  (lw:define-action "Delivery Actions" "Embed cl-unix-sockets"
    (lambda ()
      (fli:get-embedded-module :unix-sockets (native-so)))))

#+lispworks
(lw:define-action "When starting image" "Load embedded cl-unix-sockets"
  (lambda ()
    (fli:install-embedded-module :unix-sockets)))

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
