(load "~/quicklisp/setup.lisp")

(push #P "./" asdf:*central-registry*)

(ql:quickload :unix-sockets.test)

(unless (fiveam:run-all-tests)
  (uiop:quit 1))

(uiop:quit 0)
