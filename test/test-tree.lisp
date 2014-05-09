(in-package :mgl-gpr)

(defmacro assert-error (&body body)
  (let ((name (gensym)))
    `(assert (block ,name
               (handler-case (progn ,@body)
                 (error () (return-from ,name t)))
               nil))))

(defun test-node-at ()
  (assert (eql (node-at nil 0) nil))
  (assert-error (node-at nil 1))
  (let ((x '(0 (1 2 (3 4 5)) (6))))
    (assert (= 7 (count-nodes x)))
    (assert (= 4 (count-nodes x :internal t)))

    (assert (equal (node-at x 0) x))
    (assert (equal (list nil nil)
                   (multiple-value-list (parent-of-node-at x 0))))

    (assert (equal (node-at x 1) (elt x 1)))
    (assert (equal (list 0 0) (multiple-value-list (parent-of-node-at x 1))))
    
    (assert (equal (node-at x 2) 2))
    (assert (equal (list 1 0) (multiple-value-list (parent-of-node-at x 2))))
    
    (assert (equal (node-at x 3) '(3 4 5)))
    (assert (equal (node-at x 4) 4))
    (assert (equal (node-at x 5) '5))
    (assert (equal (node-at x 6) '(6)))
    (assert-error (node-at x 7))
    (assert (equal (node-at x 0 :internal t) x))
    (assert (equal (node-at x 1 :internal t) (elt x 1)))
    (assert (equal (node-at x 2 :internal t) '(3 4 5)))

    (assert (equal (node-at x 3 :internal t) '(6)))
    (assert (equal (list 0 1)
                   (multiple-value-list (parent-of-node-at x 3 :internal t))))))

(defun test-change-node-at ()
  (assert (equal 1 (change-node-at nil 0 1)))
  (assert-error (change-node-at nil 1 1))
  (assert (equal 1 (change-node-at '(+) 0 1 :internal t)))
  (assert-error (change-node-at nil 1 1 :internal t))
  (assert (equal '(0 1 (2 3)) (change-node-at '(0 1 (2)) 1 '(2 3)
                                           :internal t)))
  (assert (equal '(x 3 4)
                 (change-node-at '(x (y 1 2) 4) 1
                              3
                              :internal t)))
  (assert (equal '(f 3 2)
                 (change-node-at '(f 1 2) 1 '3))))

(defun test-tree ()
  (test-node-at)
  (test-change-node-at))
