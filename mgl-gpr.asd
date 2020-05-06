;;;; -*-mode: Lisp; coding: utf-8;-*-

;;; See MGL-GPR:@GPR-MANUAL for the user guide.
(asdf:defsystem :mgl-gpr
  :licence "MIT, see COPYING."
  :version "0.0.1"
  :author "Gábor Melis <mega@retes.hu>"
  :mailto "mega@retes.hu"
  :homepage "http://melisgl.github.io/mgl-gpr"
  :bug-tracker "https://github.com/melisgl/mgl-gpr/issues"
  :source-control (:git "https://github.com/melisgl/mgl-gpr.git")
  :description "MGL-GPR is a library of evolutionary algorithms such
  as Genetic Programming (evolving typed expressions from a set of
  operators and constants) and Differential Evolution."
  :depends-on (#:cl-random #:mgl-pax)
  :components ((:module "src"
                :serial t
                :components ((:file "package")
                             (:file "evolutionary-algorithm")
                             (:file "util")
                             (:file "tree")
                             (:file "genetic-programming")
                             (:file "differential-evolution")
                             (:file "doc"))))
  :in-order-to ((asdf:test-op (asdf:test-op "mgl-gpr/test"))))

(asdf:defsystem mgl-gpr/test
  :licence "MIT, see COPYING."
  :author "Gábor Melis <mega@retes.hu>"
  :mailto "mega@retes.hu"
  :description "Test system for MGL-GPR."
  :depends-on (#:mgl-gpr)
  :components ((:module "test"
                :serial t
                :components ((:file "test-tree")
                             (:file "test-genetic-programming")
                             (:file "test"))))
  :perform (asdf:test-op (o s)
             (uiop:symbol-call '#:mgl-gpr '#:test)))
