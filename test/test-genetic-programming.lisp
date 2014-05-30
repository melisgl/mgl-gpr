(in-package :mgl-gpr)

(defvar *x*)

(defparameter *literal-symbols* '(*x*))

(defparameter *operators* (list (operator (+ real real) real)
                                (operator (- real real) real)
                                (operator (* real real) real)
                                (operator (sin real) real)))

(defparameter *literals* (list (literal (real)
                                 (- (random 32.0) 16.0))
                               (literal (real)
                                 (alexandria:random-elt *literal-symbols*))))


(defparameter *test-expr-0* '(+ (* 3 )
                              (* -3 *x*)
                              1))

(defparameter *test-expr-1* '(+ (sin (* *x* pi 0.1))
                              (* -3 *x*)
                              1))

;;; This is more difficult because there is no EXPT operator and the
;;; base has to be found twice.
(defparameter *test-expr-2* '(sin (expt (* *x* 2 pi) 2)))

(defun evaluate (gp expr target-expr)
  (declare (ignore gp))
  (let ((min-penalized-size 20))
    (/ 1
       (1+ (/ (loop for x from 0d0 to 10d0 by 0.5d0
                    summing
                    (let ((*x* x))
                      (abs (- (eval expr)
                              (eval target-expr)))))
              21))
       (let ((size (count-nodes expr)))
         (if (< size min-penalized-size)
             1
             (exp (min 120 (/ (- size min-penalized-size) 10d0))))))))

(defun randomize (gp type expr)
  (if (and (numberp expr)
           (< (random 1.0) 0.5))
      (+ expr (random 1.0) -0.5)
      (random-gp-expression gp (lambda (level)
                                 (<= 3 level))
                            :type type)))

(defun select (gp fitnesses)
  (declare (ignore gp))
  (hold-tournament fitnesses :n-contestants 2))

(defun report-fittest (gp fittest fitness)
  (format t "Best fitness until generation ~S: ~S for~%  ~S~%"
          (generation-counter gp) fitness fittest))

(defun advance-gp (gp)
  (when (zerop (mod (generation-counter gp) 20))
    (format t "Generation ~S~%" (generation-counter gp)))
  (advance gp))

(defun test-genetic-programming ()
  (dolist (target-expr (list *test-expr-0* *test-expr-1* *test-expr-2*))
    (let ((gp (make-instance
               'gp
               :toplevel-type 'real
               :operators *operators*
               :literals *literals*
               :population-size 1000
               :copy-chance 0.0
               :mutation-chance 0.5
               :evaluator (lambda (gp expr)
                            (evaluate gp expr target-expr))
               :randomizer 'randomize
               :selector 'select
               :fittest-changed-fn 'report-fittest)))
      (loop repeat (population-size gp) do
        (add-individual gp (random-gp-expression gp (lambda (level)
                                                      (<= 5 level)))))
      (loop repeat 5000 do
        (advance-gp gp))
      (destructuring-bind (fittest . fitness) (fittest gp)
        (format t "Best fitness: ~S for~%  ~S~%" fitness fittest))
      (assert (< 0.7 (cdr (fittest gp)))))))
