(in-package :unix-sockets)

(defconstant +af-unix+ 1)
(defconstant +sock-stream+ 1)
(defconstant +max-path-len+ 108)
(defconstant +buf-size+ 10240)
(defconstant +econnreset+ 104)

(defconstant +shut-rdrw+ 2)

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

(cffi:defcstruct sockaddr-un
  #+darwin
  (len :unsigned-byte)
  #+darwin
  (family :unsigned-byte)
  #-darwin
  (family :unsigned-short)
  (path (:array :char 108)))

(cffi:defcfun "strncpy" (:pointer :char)
  (dest (:pointer :unsigned-char))
  (src :string)
  (n :unsigned-int))

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

(define-condition unix-socket-error (simple-error) ())

(defmethod print-object ((e unix-socket-error) out)
  (format out "unix-socket-error: ")
  (call-next-method))

(defun unix-socket-error (fmt &rest args)
  (error 'unix-socket-error
         :format-string fmt
         :format-arguments args))

#-lispworks
(cffi:defcfun (#+darwin "__error" #+linux "__errno_location" %errno-location)
    ()
  :returning (:pointer :int))

(cffi:defcfun ("close" %close) :void
  (fd :int))

(defun errno ()
  #-lispworks
  (cffi:deref-pointer (%errno-location) (:pointer :int))
  #+lispworks
  (lw:errno-value))

(defclass internal-stream (fundamental-binary-input-stream
                           fundamental-binary-output-stream)
  ((sock :initarg :sock
         :accessor sock)))


(defmethod stream-write-byte ((stream internal-stream)
                              byte)
  (handler-bind ((error (lambda (e)
                          (log:info "Errr while writing byte: ~a" e))))
   (let ((buf (buf (sock stream))))
     (setf (cffi:mem-ref buf :unsigned-char  0)
           byte)
     (pcheck (%write (fd (sock stream)) (char-array-to-pointer buf) 1)))))

(defmethod close ((stream internal-stream) &key abort)
  (declare (ignore abort))
  (shutdown-unix-socket (sock stream))
  (funcall (close-fn (sock stream))))

#+lispworks
(defun wait-till-fd-ready (fd)
  "Calling UNIX recv/read can block. At least on LW, this would
non-interruptible, so we want to avoid this, even though it would
otherwise be correct. I'm sure there are such considerations on non-LW
systems"
  (mp:notice-fd fd)
  (unwind-protect
       (mp:process-wait
        "Waiting for UNIX socket to be ready"
        #'unix-socket-is-ready
        fd)
    (mp:unnotice-fd fd)))

(defmethod stream-read-byte ((stream internal-stream))
  (handler-bind ((error (lambda (e)
                          (log:info "Error while reading byte: ~a" e))))
   (let ((buf (buf (sock stream)))
         (fd (fd (sock stream))))
     #+lispworks
     (wait-till-fd-ready fd)
     (cond
       ((< fd 0)
        (log:trace "Sending eof because fd < 0")
        :eof)
       (t
        (let ((num-bytes (%recv fd
                                buf 1 0)))
          (cond
            ((eql num-bytes 0)
             (log:trace "standard eof")
             :eof)
            ((> num-bytes 0)
             (cffi:mem-ref buf :unsigned-char 0))
            ((= (errno) +econnreset+)
             ;; AFAICT, this is just like an EOF, at least for the purpose
             ;; of the stream.
             ;; https://stackoverflow.com/questions/2974021/what-does-econnreset-mean-in-the-context-of-an-af-local-socket
             (log:trace "Sending :eof")
             :eof)
            (t
             (log:trace "throwing error ~a" (errno))
             (throw-errno)))))))))


(defclass unix-socket ()
  ((fd :initarg :fd
       :accessor fd)
   (id :initform (random 10000000)
       :accessor id)
   (close-fn :accessor close-fn)
   (buf :accessor buf)))

(defmethod print-object ((s unix-socket) stream)
  (format stream "#<UNIX-SOCKET id:~a>" (id s)))

(defmethod initialize-instance :after ((s unix-socket) &key fd &allow-other-keys)
  (let ((buf (cffi:foreign-alloc :unsigned-char :count +buf-size+))
        (close-fn (let ((closed-p nil)
                        (lock (bt:make-lock))
                        (id (id s)))
                    (lambda ()
                      (bt:with-lock-held (lock)
                        (unless closed-p
                          (%close fd)
                          (setf closed-p t)))))))
    (setf (buf s) buf)
    (Setf (close-fn s) (lambda ()
                         (setf (fd s) -1)
                         (funcall close-fn)))
    (trivial-garbage:finalize s (lambda ()
                                  (funcall close-fn)
                                  (cffi:foreign-free buf)))))

(defun char-array-to-pointer (x)
  x)

(defun %make-unix-socket (path bind-fn)
  (let* ((fd (socket +af-unix+ +sock-stream+ 0))
         (path (namestring path))
         (sockaddr (cffi:foreign-alloc '(:struct sockaddr-un))))
    (cffi:with-foreign-string (path path)
      (let ((dest (cffi:foreign-slot-pointer sockaddr '(:struct sockaddr-un) 'path)))
        (strncpy (char-array-to-pointer dest) path +max-path-len+)))
    (setf (cffi:foreign-slot-value sockaddr
                                   '(:struct sockaddr-un)
                                   'family)
          +af-unix+)

    (let ((ret (funcall bind-fn fd sockaddr (cffi:foreign-type-size 'sockaddr-un))))
      (unless (eql 0 ret)
        (let ((errno (errno)))
         (unix-socket-error "Failed to ~a address (errno: ~a: ~a)" bind-fn errno (strerror errno)))))

    (make-instance 'unix-socket :fd fd)))

(defun throw-errno ()
  (let ((errno (errno)))
    (unix-socket-error "Error: ~a: ~a"
                       errno
                       (strerror errno))))

(defun pcheck (e)
  (when (< e 0)
    (throw-errno))
  e)

(defun %%listen (sock backlog)
  (pcheck (%listen (fd sock) backlog)))

(defun make-unix-socket (path &key (backlog 50))
  (let ((sock (%make-unix-socket path 'bind)))
    (%%listen sock backlog)
    sock))

(defun connect-unix-socket (path)
  (%make-unix-socket path 'connect))

(defun close-unix-socket (sock)
  (funcall (close-fn sock))
  (setf (fd sock) -1))

(defmacro with-unix-socket ((sck fn) &body body)
  `(let ((,sck ,fn))
     (unwind-protect
          (progn ,@body)
       (close-unix-socket ,sck))))

(defun accept-unix-socket (sock)
  (let ((cl-fd (pcheck (%accept (fd sock) nil nil))))
    (make-instance 'unix-socket :fd cl-fd)))

(defun shutdown-unix-socket (sock)
  (pcheck (%shutdown (fd sock)
                     +shut-rdrw+)))


;; (close (make-unix-socket "/tmp/foo91"))

(defun unix-socket-stream (sock)
  (flexi-streams:make-flexi-stream
   (make-instance 'internal-stream :sock sock)))
