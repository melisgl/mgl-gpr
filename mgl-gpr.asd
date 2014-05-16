;;;; -*-mode: Lisp; coding: utf-8;-*-

;;; See MGL-GPR:@GPR-MANUAL for the user guide.
(asdf:defsystem :mgl-gpr
  :licence "MIT, see COPYING."
  :version "0.0.1"
  :author "Gábor Melis"
  :mailto "mega@retes.hu"
  :homepage "http://quotenil.com"
  :description "MGL-GPR is a library for genetic programming: evolving
  typed expressions for a particular purpose from a set of operators
  and constants."
  :depends-on (#:mgl-pax)
  :components ((:module "src"
                :serial t
                :components ((:file "package")
                             (:file "util")
                             (:file "tree")
                             (:file "genetic-programming")))))

(defmethod asdf:perform ((o asdf:test-op)
                         (c (eql (asdf:find-system '#:mgl-gpr))))
  (asdf:oos 'asdf:test-op '#:mgl-gpr-test)
  (funcall (intern (symbol-name '#:test) (find-package '#:mgl-gpr))))

(defmethod asdf:operation-done-p ((o asdf:test-op)
                                  (c (eql (asdf:find-system '#:mgl-gpr))))
  (values nil))
