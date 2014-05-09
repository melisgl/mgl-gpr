;;;; -*- mode: Lisp -*-

(asdf:defsystem mgl-gpr-test
  :depends-on (#:mgl-gpr)
  :components ((:module "test"
                :serial t
                :components ((:file "test-tree")
                             (:file "test-genetic-programming")
                             (:file "test")))))
