(in-package :unix-sockets)

(defclass internal-stream (fundamental-binary-input-stream
                           fundamental-binary-output-stream)
  ((sock :initarg :sock :accessor sock)))

(defmethod stream-element-type ((stream internal-stream)) 'integer)

(defmethod stream-write-byte ((stream internal-stream) byte)
  (handler-bind ((error (lambda (e)
                          (log:info "Errr while writing byte: ~a" e))))
   (let ((buf (buf (sock stream))))
     (setf (cffi:mem-ref buf :unsigned-char  0)
           byte)
     (pcheck (%write (fd (sock stream)) (char-array-to-pointer buf) 1)))))

(defmethod close ((stream internal-stream) &key abort)
  (declare (ignore abort))
  (shutdown-unix-socket (sock stream)))

(defmethod stream-read-byte ((stream internal-stream))
  (handler-bind ((error (lambda (e)
                          (log:info "Error while reading byte: ~a" e))))
   (let ((buf (buf (sock stream)))
         (fd (fd (sock stream))))
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


;; ┌─┐┌┐┌┌─┐┬┬  ┬  ┌─┐┬─┐┬ ┬
;; ├─┤││││  ││  │  ├─┤├┬┘└┬┘
;; ┴ ┴┘└┘└─┘┴┴─┘┴─┘┴ ┴┴└─ ┴
;; Same as internal-stream, but when reading - provides the possibility to
;; read the ancillary data from the socket.
;; TODO: Since i am lazy - this is implemented only to support the cmsg_type SCM_RIGHTS
;; Which would enable passing file descriptors between processes.
;; TODO: Ideally this should iterate through CMSG_NXTHDR and read all the ancillary data

(defclass ancillary-stream (fundamental-binary-input-stream
                           fundamental-binary-output-stream)
  ((sock :initarg :sock :accessor sock)
   (ancillary-fd :initarg :ancillary-fd :accessor ancillary-fd)))

(defmethod stream-element-type ((stream ancillary-stream)) 'integer)

(defmethod stream-write-byte ((stream ancillary-stream) byte)
  (handler-bind ((error (lambda (e)
                          (log:info "Errr while writing byte: ~a" e))))
   (let ((buf (buf (sock stream))))
     (setf (cffi:mem-ref buf :unsigned-char  0)
           byte)
     (pcheck (%write (fd (sock stream)) (char-array-to-pointer buf) 1)))))

(defmethod close ((stream ancillary-stream) &key abort)
  (declare (ignore abort))
  (shutdown-unix-socket (sock stream)))

(defmethod stream-read-byte ((stream ancillary-stream))
  (handler-bind ((error (lambda (e)
                          (log:info "Error while reading byte: ~a" e))))
   (let ((buf (buf (sock stream)))
         (fd (fd (sock stream)))
	 (msghdr (cffi:foreign-alloc 'msghdr))
	 (cmsghdr (cffi:foreign-alloc 'cmsghdr)))
     (setf (cffi:slot-value msghdr 'msg_iov) buf)
     (setf (cffi:slot-value msghdr 'msg_control) cmsghdr)
     (setf (cffi:slot-value msghdr 'msg_controllen) (cffi:foreign-sizeof 'cmsghdr))
     (cond
       ((< fd 0)
        (log:trace "Sending eof because fd < 0")
        :eof)
       (t
        (let ((num-bytes (%recvmsg fd msghdr 0))
	      (cmsg (%cmsg-firsthdr msghdr)))
	  (if (eq (cffi:slot-value cmsg 'cmsg_type) +scm_rights+)
	      (let ((fd (cffi:mem-ref (cffi:slot-value cmsg 'cmsg_data) :int)))
		(setf (ancillary-fd stream) fd))
	      ;; TODO: This might actually kill off the file descriptor on each new byte read
	      (setf (ancillary-fd stream) nil))

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
