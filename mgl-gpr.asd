;;;; -*-mode: Lisp; coding: utf-8;-*-

;;; See MGL-GPR:@GPR-MANUAL for the user guide.
(asdf:defsystem :mgl-gpr
  :licence "MIT, see COPYING."
  :version "0.0.1"
  :author "Gábor Melis"
  :mailto "mega@retes.hu"
  :homepage "http://quotenil.com"
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
                             (:file "doc")))))

(defmethod asdf:perform ((o asdf:test-op)
                         (c (eql (asdf:find-system '#:mgl-gpr))))
  (asdf:oos 'asdf:load-op '#:mgl-gpr-test)
  (funcall (intern (symbol-name '#:test) (find-package '#:mgl-gpr))))
