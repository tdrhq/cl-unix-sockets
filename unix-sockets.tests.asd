#-(:or :mswindows :windows)
(defsystem :unix-sockets.tests
    :serial t
    :depends-on (:tmpdir
                 :unix-sockets
                 :fiveam
                 :cl-fad
                 :trivial-timeout)
    :components ((:file "test-unix-sockets")))

#+(:or :mswindows :windows)
(defsystem :unix-sockets.tests)
