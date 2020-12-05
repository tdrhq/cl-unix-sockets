(defpackage :test-unix-sockets
  (:use :cl
        :unix-sockets
        :alexandria
   :fiveam))
(in-package :test-unix-sockets)

(def-suite* :test-unix-sockets)

(test simple-open-and-close
  (tmpdir:with-tmpdir (dir)
    (let ((sock (make-unix-socket (path:catfile dir "sock"))))
      (close-unix-socket sock))))

(test connect
  (tmpdir:with-tmpdir (dir)
    (let ((file (path:catfile dir "sock")))
      (with-unix-socket (sock (make-unix-socket file))
        (with-unix-socket (client (connect-unix-socket file)))))))

(test send-and-recv
  (tmpdir:with-tmpdir (dir)
    (let ((file (path:catfile dir "sock")))
      (with-unix-socket (server-sock (make-unix-socket file))
        (let ((thread (bt:make-thread (lambda ()
                                        (assert (not (symbolp server-sock)))
                                        (let ((client (accept-unix-socket server-sock))))
                                        ))))
          (connect-unix-socket file)
          (bt:join-thread thread)
          (pass))))))

(test send-and-recv-a-single-byte
  (tmpdir:with-tmpdir (dir)
    (let ((file (path:catfile dir "sock"))
          (read-value 0))
      (with-unix-socket (server-sock (make-unix-socket file))
        (let ((thread (bt:make-thread (lambda ()
                                        (with-unix-socket (client (accept-unix-socket server-sock))
                                          (let ((stream (unix-socket-stream client)))
                                            (setf read-value
                                                  (read-byte stream))
                                            (setf eof-value
                                                  (read-byte stream nil :eoff))))))))
          (with-unix-socket (client (connect-unix-socket file))
            (let ((Stream (unix-socket-stream client)))
              (write-byte 22 stream)))

          (bt:join-thread thread)
          (is (eql 22 read-value))
          (is (eql :eoff eof-value)))))))

(test close-from-another-thread
  (tmpdir:with-tmpdir (dir)
    (let ((file (path:catfile dir "sock")))
      (let ((client)
            (lock (bt:make-lock))
            (cv (bt:make-condition-variable)))
        (bt:with-lock-held (lock)
         (with-unix-socket (server-sock (make-unix-socket file))
           (let ((thread (bt:make-thread (lambda (&aux stream)
                                           (setf client (accept-unix-socket server-sock))
                                           (setf stream (unix-socket-stream client))
                                           (read-byte stream nil nil)))))
             (with-unix-socket (client-end (connect-unix-socket file))
               (sleep 0.1)
               (shutdown-unix-socket client))
             (bt:join-thread thread))))))))

(define-condition test-error (error) ())

(test interrupt-recv
  (tmpdir:with-tmpdir (dir)
    (let ((file (path:catfile dir "sock"))
          (read-value 0))
      (with-unix-socket (server-sock (make-unix-socket file))
        (let ((thread (bt:make-thread (lambda ()
                                        (handler-case
                                            (with-unix-socket (client (accept-unix-socket server-sock))
                                              (let ((stream (unix-socket-stream client)))
                                                (setf read-value
                                                      (read-byte stream))))
                                          (test-error (e)
                                            (setf read-value -1)))))))

          (with-unix-socket (client (connect-unix-socket file))
            (let ((Stream (unix-socket-stream client)))
              (sleep 0.1)
              (bt:interrupt-thread thread (lambda () (error 'test-error)))))

          (bt:join-thread thread)
          (is (eql -1 read-value)))))))
