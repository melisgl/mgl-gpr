(defpackage differential-evolution-example
  (:use :common-lisp :mgl-gpr))

(in-package :differential-evolution-example)

;;; General dimension rosenbrock function, given by:
;;;
;;;   f(x) = sum_{i=1:D-1} 100*(x(i+1) - x(i)^2)^2 + (1-x(i))^2
;;;
;;; where D is the dimension of x. The true minimum is 0 at x = (1 1
;;; ... 1).
(defun rosenbrock (x)
  (let ((d (length x)))
    (loop for i below (1- d)
          summing (+ (* 100
                        (expt (- (aref x (1+ i))
                                 (expt (aref x i) 2))
                              2))
                     (expt (- 1 (aref x i)) 2)))))

(defun create-individual (n)
  (let ((w (make-array n :element-type 'double-float :initial-element 0d0)))
    (dotimes (i n)
      (setf (aref w i) (- (random 32d0) 16d0)))
    w))

(defun evaluate (de weights)
  (declare (ignore de))
  (- (rosenbrock weights)))

(defun report-fittest (de fittest fitness)
  (declare (ignore fittest))
  (format t "Best fitness until generation ~S: ~S~%"
          (generation-counter de) fitness))

(defun advance-de (de)
  (when (zerop (mod (generation-counter de) 20))
    (format t "Generation ~S~%" (generation-counter de)))
  (advance de))

(defun test-differential-evolution ()
  (let* ((n-rosenbrock-dimensions 100)
         (de (make-instance
              'sansde
              :population-size 100
              :evaluator 'evaluate
              :fittest-changed-fn 'report-fittest
              :create-individual-fn
              (lambda (de)
                (declare (ignore de))
                (create-individual n-rosenbrock-dimensions)))))
    (loop repeat (population-size de) do
      (add-individual de (create-individual n-rosenbrock-dimensions)))
    (loop repeat 10000 do
      (advance-de de))
    (destructuring-bind (fittest . fitness) (fittest de)
      (format t "Best fitness: ~S for~%  ~S~%" fitness fittest))
    (assert (< -0.01 (cdr (fittest de))))))

#|

(test-differential-evolution)

|#
