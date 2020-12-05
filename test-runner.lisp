(require :asdf)

(load "~/quicklisp/setup.lisp")

(pushnew #P "./" ql:*local-project-directories*)

(ql:quickload "unix-sockets.tests")

(defun main ()
 (when (not (fiveam:run-all-tests))
   (uiop:quit 1)))

#-lispworks
(main)

#+lispworks
(mp:initialize-multiprocessing :main nil #'main)
