(in-package :unix-sockets)

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
