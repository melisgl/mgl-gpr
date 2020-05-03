(in-package :mgl-gpr)

;;;; Register in PAX World

(defun pax-sections ()
  (list @gpr-manual))
(defun pax-pages ()
  `((:objects
     (, @gpr-manual)
     :source-uri-fn ,(make-github-source-uri-fn
                      :mgl-gpr
                      "https://github.com/melisgl/mgl-gpr"))))
(register-doc-in-pax-world :mgl-gpr (pax-sections) (pax-pages))

#+nil
(progn
  (update-asdf-system-readmes @gpr-manual :mgl-gpr)
  (update-asdf-system-html-docs @gpr-manual :mgl-gpr
                                :pages (pax-pages)))
