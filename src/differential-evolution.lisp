(in-package :mgl-gpr)

(defsection @gpr-de (:title "Differential Evolution")
  "The concepts in this section are covered by [Differential
  Evolution: A Survey of the State-of-the-Art][1].

    [1]: http://107.167.189.191/~piak/teaching/ec/ec2012/das-de-sota-2011.pdf"
  (differential-evolution class)
  (map-weights-into-fn (reader differential-evolution))
  (create-individual-fn (reader differential-evolution))
  (mutate-fn (reader differential-evolution))
  (crossover-fn (reader differential-evolution))
  (mutate/rand/1 function)
  (mutate/best/1 function)
  (mutate/current-to-best/2 function)
  (crossover/binary function)
  (select-distinct-random-numbers function)
  (@gpr-de-sansde section))

(defclass differential-evolution (evolutionary-algorithm)
  ((map-weights-into-fn
    :initform #'map-into
    :initarg :map-weights-into-fn
    :reader map-weights-into-fn
    :documentation "The vector of numbers (the 'weights') are most
    often stored in some kind of array. All individuals must have the
    same number of weights, but the actual representation can be
    anything as long as the function in this slot mimics the semantics
    of MAP-INTO that's the default.")
   (create-individual-fn
    :initarg :create-individual-fn
    :reader create-individual-fn
    :documentation "Holds a function of one argument, the DE, that
    returns a new individual that needs not be initialized in any way.
    Typically this just calls MAKE-ARRAY.")
   (mutate-fn
    :initarg :mutate-fn
    :reader mutate-fn
    :documentation "One of the supplied mutation functions:
    MUTATE/RAND/1 MUTATE/BEST/1 MUTATE/CURRENT-TO-BEST/2.")
   (crossover-fn
    :initform #'crossover/binary
    :initarg :crossover-fn
    :reader crossover-fn
    :documentation "A function of three arguments, the DE and two
    individuals, that destructively modifies the second individual by
    using some parts of the first one. Currently, the implemented
    crossover function is CROSSOVER/BINARY.")
   (autotune-fn
    :initform nil
    :initarg :autotune-fn
    :reader autotune-fn))
  (:documentation "Differential evolution (DE) is an evolutionary
  algorithm in which individuals are represented by vectors of
  numbers. New individuals are created by taking linear combinations
  or by randomly swapping some of these numbers between two
  individuals."))

(defmethod advance ((de differential-evolution))
  (incf (slot-value de 'generation-counter))
  (let ((population (population de))
        (nursery (nursery de))
        (fitnesses (fitnesses de))
        (fitness-key (fitness-key de))
        (create-individual-fn (create-individual-fn de))
        (mutate-fn (mutate-fn de))
        (crossover-fn (crossover-fn de))
        (fittest (fittest de))
        (changedp nil)
        (improved-indices ())
        (evaluator (evaluator de))
        (autotune-fn (autotune-fn de)))
    (flet ((maybe-update-fittest (individual fitness)
             (let ((numeric-fitness (funcall fitness-key fitness)))
               (when (or (null fittest)
                         (< (funcall fitness-key (cdr fittest))
                            numeric-fitness))
                 (setq fittest (cons individual fitness))
                 (setf (slot-value de 'fittest) fittest)
                 (setq changedp t)))))
      (if (= 1 (generation-counter de))
          (let ((n (length (population de))))
            (setf (slot-value de 'nursery) (make-array n :initial-element nil))
            (setq nursery (nursery de))
            (dotimes (i n)
              (setf (aref nursery i) (funcall create-individual-fn de)))
            (setf (slot-value de 'fitnesses) (make-array n))
            (setq fitnesses (fitnesses de))
            (dotimes (i n)
              (setf (aref fitnesses i)
                    (setf (aref fitnesses i)
                          (funcall evaluator de (aref population i))))
              (maybe-update-fittest (aref population i) (aref fitnesses i))))
          (let ((best (nth-value
                       1 (max-position/vector fitnesses :key fitness-key))))
            (if (mass-evaluator de)
                (error "not implementated")
                (dotimes (i (length population))
                  (let ((mutation-info nil)
                        (crossover-info nil))
                    (setf (values (aref nursery i) mutation-info)
                          (funcall mutate-fn de i best population nursery))
                    (setf (values (aref nursery i) crossover-info)
                          (funcall crossover-fn de (aref population i)
                                   (aref nursery i)))
                    (let* ((numeric-parent-fitness
                             (funcall fitness-key (aref fitnesses i)))
                           (child-fitness
                             (funcall evaluator de (aref nursery i)))
                           (numeric-child-fitness
                             (funcall fitness-key child-fitness)))
                      (when (<= numeric-parent-fitness numeric-child-fitness)
                        (push i improved-indices)
                        (setf (aref fitnesses i) child-fitness)
                        (maybe-update-fittest (aref nursery i)
                                              child-fitness))
                      (when autotune-fn
                        (funcall autotune-fn de mutation-info crossover-info
                                 numeric-parent-fitness
                                 numeric-child-fitness)))))))))
    (when (and changedp (fittest-changed-fn de))
      (funcall (fittest-changed-fn de) de
               (car (fittest de)) (cdr (fittest de))))
    (dolist (i improved-indices)
      (rotatef (aref population i) (aref nursery i)))))

(defun crossover/binary (de individual-1 individual-2 &key (crossover-rate 0.5))
  "Destructively modify INDIVIDUAL-2 by replacement each element with
  a probability of 1 - CROSSOVER-RATE with the corresponding element
  in INDIVIDUAL-1. At least one, element is changed. Return
  INDIVIDUAL-2."
  (let ((n 0)
        (m 0))
    (let ((r (funcall (map-weights-into-fn de) individual-2
                      (lambda (w1 w2)
                        (incf n)
                        (if (< (random 1.0) crossover-rate)
                            (progn
                              (incf m)
                              w2)
                            w1))
                      individual-1 individual-2)))
      (if (plusp m)
          r
          (let ((i (random n))
                (n 0))
            (funcall (map-weights-into-fn de) individual-2
                     (lambda (w1 w2)
                       (incf n)
                       (if (= i n)
                           w2
                           w1))
                     individual-1 individual-2))))))

(defun mutate/rand/1 (de current best population nursery &key (f 0.5))
  (declare (ignore best))
  (let ((n (length population)))
    (destructuring-bind (i1 i2 i3)
        (select-distinct-random-numbers (list current) 3 n)
      (values (funcall (map-weights-into-fn de) (aref nursery current)
                       (lambda (w1 w2 w3)
                         (+ w1 (* f (- w2 w3))))
                       (aref population i1)
                       (aref population i2)
                       (aref population i3))))) )

(defun mutate/best/1 (de current best population nursery &key (f 0.5))
  (let ((n (length population)))
    (destructuring-bind (i1 i2)
        (select-distinct-random-numbers (list current) 2 n)
      (values (funcall (map-weights-into-fn de) (aref nursery current)
                       (lambda (best-w w1 w2)
                         (+ best-w (* f (- w1 w2))))
                       (aref population best)
                       (aref population i1)
                       (aref population i2))))) )

(defun mutate/current-to-best/2 (de current best population nursery
                                 &key (f 0.5))
  (let ((n (length population)))
    (destructuring-bind (i1 i2)
        (select-distinct-random-numbers (list current best) 2 n)
      (values (funcall (map-weights-into-fn de) (aref nursery current)
                       (lambda (current-w best-w w1 w2)
                         (+ current-w
                            (* f (- best-w current-w))
                            (* f (- w1 w2))))
                       (aref population current)
                       (aref population best)
                       (aref population i1)
                       (aref population i2))))))


(defsection @gpr-de-sansde (:title "\\SANSDE")
  "See the paper [Self-adaptive Differential Evolution with
  Neighborhood Search][1].

    [1]: http://staff.ustc.edu.cn/~ketang/papers/YangTangYao_CEC08a.pdf"
  (sansde class))

(defclass sansde (differential-evolution)
  ;; -PR stands for probability
  ((mutation-strategy-pr :initform 0.5 :accessor mutation-strategy-pr)
   (mutation-factor-pr :initform 0.5 :accessor mutation-factor-pr)
   (crossover-rate-mean :initform 0.5 :accessor crossover-rate-mean)
   (stats :initform () :accessor stats)
   (mutate-fn :initform 'mutate/sansde)
   (crossover-fn :initform 'crossover/sansde)
   (autotune-fn :initform 'tune/sansde))
  (:documentation "SaNSDE is a special DE that dynamically adjust the
  crossover and mutation are performed. The only parameters are the
  generic EA ones: POPULATION-SIZE, EVALUATOR, etc. One also has to
  specify MAP-WEIGHTS-INTO-FN and CREATE-INDIVIDUAL-FN."))

(defun draw-cauchy ()
  (let ((x (random 1d0)))
    (tan (* pi (- x 0.5d0)))))

(defun draw-positive-cauchy ()
  (loop for x = (draw-cauchy)
        until (plusp x)
        finally (return x)))

(defun draw-positive-normal (mean stddev)
  (let ((r (cl-random:r-normal mean (expt stddev 2))))
    (loop for x = (cl-random:draw r)
          until (plusp x)
          finally (return x))))

(defun draw-rate (mean stddev)
  (let ((r (cl-random:r-normal mean (expt stddev 2))))
    (loop for x = (cl-random:draw r)
          until (< 0 x 1)
          finally (return x))))

(defun mutate/sansde (de current best population nursery)
  (let* ((mutation-factor-choice (if (< (random 1.0) (mutation-factor-pr de))
                                     'normal
                                     'cauchy))
         (f (if (eq mutation-factor-choice 'normal)
                (draw-positive-normal 0.5 0.3)
                (draw-positive-cauchy))))
    (if (< (random 1.0) (mutation-strategy-pr de))
        (values (mutate/rand/1 de current best population nursery :f f)
                (list 'mutate/rand/1 mutation-factor-choice))
        (values (mutate/current-to-best/2 de current best population nursery
                                          :f f)
                (list 'mutate/current-to-best/2 mutation-factor-choice)))))

(defun crossover/sansde (de individual-1 individual-2)
  (let ((rate (draw-rate (crossover-rate-mean de) 0.1)))
    (values (crossover/binary de individual-1 individual-2
                              :crossover-rate rate)
            rate)))

(defun tune/sansde (de mutation-info crossover-info
                    numeric-parent-fitness numeric-child-fitness)
  (push (list mutation-info crossover-info
              numeric-parent-fitness numeric-child-fitness)
        (stats de)))

(defmethod advance ((de sansde))
  (update-sansde-strategy de)
  #+nil
  (format t "sansde: ~,2F ~,2F ~,2F~%" (mutation-strategy-pr de)
          (mutation-factor-pr de) (crossover-rate-mean de))
  (call-next-method))

(defun update-sansde-strategy (de)
  (when (stats de)
    (let ((p-ns1 1) (p-ns2 1) (p-nf1 1) (p-nf2 1)
          (f-ns1 1) (f-ns2 1) (f-nf1 1) (f-nf2 1)
          (rates-and-deltas ()))
      (dolist (stat (stats de))
        (destructuring-bind ((mutation-strategy-choice mutation-factor-choice)
                             crossover-rate
                             numeric-parent-fitness numeric-child-fitness)
            stat
          (if (<= numeric-parent-fitness numeric-child-fitness)
              (ecase mutation-strategy-choice
                ((mutate/rand/1) (incf p-ns1))
                ((mutate/current-to-best/2) (incf p-ns2)))
              (ecase mutation-strategy-choice
                ((mutate/rand/1) (incf p-nf1))
                ((mutate/current-to-best/2) (incf p-nf2))))
          (if (<= numeric-parent-fitness numeric-child-fitness)
              (ecase mutation-factor-choice
                ((normal) (incf f-ns1))
                ((cauchy) (incf f-ns2)))
              (ecase mutation-factor-choice
                ((normal) (incf f-nf1))
                ((cauchy) (incf f-nf2))))
          ;; Note that it's not <= to avoid division by 0 problems.
          (when (< numeric-parent-fitness numeric-child-fitness)
            (push (cons crossover-rate
                        (- numeric-child-fitness numeric-parent-fitness))
                  rates-and-deltas))))
      (setf (mutation-strategy-pr de)
            (+ (* 0.95 (mutation-strategy-pr de))
               (* 0.05 (sansde-weighted-average p-ns1 p-ns2 p-nf1 p-nf2))))
      (setf (mutation-factor-pr de)
            (+ (* 0.95 (mutation-factor-pr de))
               (* 0.05 (sansde-weighted-average f-ns1 f-ns2 f-nf1 f-nf2))))
      (let* ((sum-deltas (reduce #'+ rates-and-deltas :key #'cdr))
             (weighted-rate (reduce #'+ rates-and-deltas
                                    :key (lambda (rate-and-delta)
                                           (* (car rate-and-delta)
                                              (/ (cdr rate-and-delta)
                                                 sum-deltas))))))
        ;; (format t "sum-deltas: ~S~%" sum-deltas)
        ;; (format t "rates-and-deltas: ~S~%" rates-and-deltas)
        ;; (format t "weighted-rate: ~S~%" weighted-rate)
        (setf (crossover-rate-mean de)
              (+ (* 0.95 (crossover-rate-mean de))
                 (* 0.05 weighted-rate)))))
    (setf (stats de) ())))

(defun sansde-weighted-average (ns1 ns2 nf1 nf2)
  (let ((a (* ns1 (+ ns2 nf2)))
        (b (* ns2 (+ ns1 nf1))))
    (/ a (+ a b))))

(defun select-distinct-random-numbers (taboos n limit)
  (let ((taboos taboos))
    (loop repeat n do
      (loop for i = (random limit)
            when (not (find i taboos))
              do (push i taboos)
                 (return)))
    (subseq taboos 0 n)))
