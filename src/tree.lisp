(in-package :mgl-gpr)

;;;; Utility functions for sexps treated as trees and indexed in
;;;; pre-order. This is slow and cumbersome.

(defun map-tree (fn tree)
  (funcall fn tree)
  (when (listp tree)
    (dolist (child (rest tree))
      (funcall fn child))))

(defmacro dotree ((node tree &optional result-form) &body body)
  `(progn (map-tree (lambda (,node) ,@body) ,tree)
          ,result-form))

(defun count-nodes (tree &key internal)
  "Count the nodes in the sexp TREE. If INTERNAL then don't count the
  leaves."
  (if (atom tree)
      (if internal 0 1)
      (1+ (loop for child in (rest tree)
                summing (count-nodes child :internal internal)))))

(defun node-at (x index &key internal)
  (let ((n index))
    (labels ((traverse (node)
               (unless (and internal (atom node))
                 (when (zerop n)
                   (return-from node-at node))
                 (decf n))
               (when (listp node)
                 (mapc #'traverse (rest node)))))
      (traverse x)))
  (error "Index ~S out of bounds in tree ~S." index x))

(defun parent-of-node-at (x index &key internal)
  (let ((n 0)
        *parent-index*
        *child-index*)
    (declare (special *parent-index* *child-index*))
    (labels ((traverse (node)
               (unless (and internal (atom node))
                 (when (= n index)
                   (return-from parent-of-node-at (values *parent-index*
                                                          *child-index*)))
                 (incf n))
               (when (listp node)
                 (let ((*parent-index* (1- n))
                       (*child-index* 0))
                   (declare (special *parent-index* *child-index*))
                   (dolist (child (rest node))
                     (traverse child)
                     (incf *child-index*))))))
      (traverse x)))
  (error "Index ~S out of bounds in tree ~S." index x))

(defun change-node-at (x index value &key internal)
  (let ((n -1))
    (labels ((traverse (node)
               (unless (and internal (atom node))
                 (incf n)
                 (when (= n index)
                   (return-from traverse value)))
               (if (atom node)
                   node
                   (cons (first node)
                         (mapcar #'traverse (rest node))))))
      (let ((new-tree (traverse x)))
        (when (< n index)
          (error "Index ~S out of bounds in tree ~S." index x))
        new-tree))))
