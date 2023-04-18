(in-package :unix-sockets)

(defconstant +af-unix+ 1)
(defconstant +sock-stream+ 1)
(defconstant +max-path-len+ 108)
(defconstant +buf-size+ 10240)
(defconstant +econnreset+ 104)

(defconstant +shut-rdrw+ 2)

#+windows
(error "Only Mac and Linux supported for the moment. Maybe FreeBSD, not sure")

(defvar *use-internal-stream-p*
  (or #-(or sbcl lispworks) t)
  "Documentation, a flag whether to use the internal stream or use native
socket IO code. Do not modify this. This is mainly used a means for
testability.")

(define-condition unix-socket-error (simple-error) ())

(defmethod print-object ((e unix-socket-error) out)
  (format out "unix-socket-error: ")
  (call-next-method))

(defun unix-socket-error (fmt &rest args)
  (error 'unix-socket-error
         :format-string fmt
         :format-arguments args))


(defclass unix-socket ()
  ((fd :initarg :fd
       :accessor fd)
   (id :initform (random 10000000)
       :accessor id)
   (stream :initform nil)
   (buf :accessor buf)))

(defmethod print-object ((s unix-socket) stream)
  (format stream "#<UNIX-SOCKET id:~a>" (id s)))

(defmethod initialize-instance :after ((s unix-socket) &key fd &allow-other-keys)
  (declare (ignore fd))
  (let ((buf (cffi:foreign-alloc :unsigned-char :count +buf-size+)))
    (setf (buf s) buf)))

(defun char-array-to-pointer (x)
  x)

(defun %make-unix-socket (path bind-fn)
  (let* ((fd (socket +af-unix+ +sock-stream+ 0))
         (path (namestring path))
         (sockaddr (unix-socket-make-sockaddr path)))
    (let ((ret (funcall bind-fn fd sockaddr (unix-socket-sockaddr-size))))
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
    (cond
      #+sbcl
      ((not *use-internal-stream-p*)
       ;; File descriptors will always be converted to SBCL sockets
       (make-instance 'sb-bsd-sockets:local-socket
                      :descriptor sock))
      (t
       sock))))

(defun connect-unix-socket (path)
  (%make-unix-socket path 'connect))

(defun close-unix-socket (sock)
  (when (>= (fd sock) 0)
    (cond
      (*use-internal-stream-p*
       (%close (fd sock)))
      (t
       #+lispworks
       (close (unix-socket-stream sock))
       #+sbcl
       (sb-bsd-sockets:socket-close (fd sock))))
    (setf (fd sock) -1)))

(defmacro with-unix-socket ((sck fn) &body body)
  `(let ((,sck ,fn))
     (unwind-protect
          (progn ,@body)
       (close-unix-socket ,sck))))

(defun accept-unix-socket (sock)
  (let ((cl-fd
          (cond
            (*use-internal-stream-p*
             (pcheck (%accept (fd sock) (cffi:null-pointer)
                              (cffi:null-pointer))))
            (t
             #+lispworks
             (comm::get-fd-from-socket (fd sock))
             #+sbcl
             (sb-bsd-sockets:socket-accept (fd sock))))))
    (make-instance 'unix-socket :fd cl-fd)))

(find-class 'sb-bsd-sockets:socket)

(defun shutdown-unix-socket (sock)
  "Graceful shutdown the socket"
  (pcheck (%shutdown (fd sock)
                     +shut-rdrw+)))


;; (close (make-unix-socket "/tmp/foo91"))

(defun %unix-socket-stream (sock)
  (cond
    (*use-internal-stream-p*
     (flexi-streams:make-flexi-stream
      (make-instance 'internal-stream :sock sock)))
    (t
     #+lispworks
     (make-instance 'comm:socket-stream :socket (fd sock)
                                        :direction :io))))


(defmethod unix-socket-stream (sock)
  (with-slots (stream) sock
    (or
     stream
     (setf stream (%unix-socket-stream sock)))))
