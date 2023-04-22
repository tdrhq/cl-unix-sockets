(defpackage :unix-sockets-asdf
  (:use :cl
   :asdf))
(in-package :unix-sockets-asdf)

(defclass lib-source-file (c-source-file)
  ())

(defparameter *library-file-dir*
  (make-pathname :name nil :type nil
                 :defaults *load-truename*))

(defmethod output-files ((o compile-op) (c lib-source-file))
  (let ((library-file-type
          (funcall (intern (symbol-name'#:default-foreign-library-type)
                           (symbol-name '#:uffi)))))
    (list (make-pathname :name (component-name c)
                         :type library-file-type
                         :defaults *library-file-dir*))))

(defmethod perform ((o load-op) (c lib-source-file))
  t)

(defmethod perform ((o compile-op) (c lib-source-file))
  (uiop:run-program (list "/usr/bin/gcc" "-shared" "-o" (namestring (car (output-files o c)))
                          "-Werror"
                          "-fPIC"
                          (namestring
                           (merge-pathnames (format nil "~a.c" (component-name c))
                                            *library-file-dir*)))
                    :output :interactive
                    :error-output :interactive))

(defsystem :unix-sockets
    :description "UNIX Domain socket"
    :author "Arnold Noronha <arnold@tdrhq.com>"
    :license  "Apache License, Version 2.0"
  :serial t
  :defsystem-depends-on (:uffi #| todo: remove |#)
  :depends-on (:cffi
               :trivial-gray-streams
               :flexi-streams
               :log4cl
               :trivial-garbage)
  :components ((:file "package")
               (lib-source-file "unix_sockets")
               (:file "ffi")
               (:file "streams")
               (:file "unix-sockets")))
