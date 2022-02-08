(in-package :mgl-gpr)

(defsection @gpr-gp (:title "Genetic Programming")
  (@gpr-gp-background section)
  (@gpr-gp-tutorial section)
  (@gpr-gp-expressions section)
  (@gpr-gp-basics section)
  (@gpr-gp-search-space section)
  (@gpr-gp-reproduction section)
  (@gpr-gp-environment section))

(defsection @gpr-gp-background (:title "Background")
  "What is Genetic Programming? This is what Wikipedia has to say:

  > In artificial intelligence, genetic programming (GP) is an
  > evolutionary algorithm-based methodology inspired by biological
  > evolution to find computer programs that perform a user-defined
  > task. Essentially GP is a set of instructions and a fitness
  > function to measure how well a computer has performed a task. It
  > is a specialization of genetic algorithms (GA) where each
  > individual is a computer program. It is a machine learning
  > technique used to optimize a population of computer programs
  > according to a fitness landscape determined by a program's ability
  > to perform a given computational task.

  Lisp has a long history of Genetic Programming because \\GP involves
  manipulation of expressions which is of course particularly easy
  with sexps.")

(defsection @gpr-gp-tutorial (:title "Tutorial")
  "GPR works with typed expressions. Mutation and crossover never
  produce expressions that fail with a type error. Let's define a
  couple of operators that work with real numbers and also return a
  real:

      (defparameter *operators* (list (operator (+ real real) real)
                                      (operator (- real real) real)
                                      (operator (* real real) real)
                                      (operator (sin real) real)))

  One cannot build an expression out of these operators because they
  all have at least one argument. Let's define some literal classes
  too. The first is produces random numbers, the second always returns
  the symbol `*X*`:

      (defparameter *literals* (list (literal (real)
                                       (- (random 32.0) 16.0))
                                     (literal (real)
                                       '*x*)))

  Armed with `*OPERATORS*` and `*LITERALS*`, one can already build
  random expressions with RANDOM-EXPRESSION, but we also need to
  define how good a certain expression is which is called *fitness*.

  In this example, we are going to perform symbolic regression, that
  is, try to find an expression that approximates some target
  expression well:

      (defparameter *target-expr* '(+ 7 (sin (expt (* *x* 2 pi) 2))))

  Think of `*TARGET-EXPR*` as a function of `*X*`. The evaluator
  function will bind the special `*X*` to the input and simply EVAL
  the expression to be evaluated.

      (defvar *x*)

  The evaluator function calculates the average difference between
  `EXPR` and `TARGET-EXPR`, penalizes large expressions and returns
  the fitness of `EXPR`. Expressions with higher fitness have higher
  chance to produce offsprings.

      (defun evaluate (gp expr target-expr)
        (declare (ignore gp))
        (/ 1
           (1+
            ;; Calculate average difference from target.
            (/ (loop for x from 0d0 to 10d0 by 0.5d0
                     summing (let ((*x* x))
                               (abs (- (eval expr)
                                       (eval target-expr)))))
               21))
           ;; Penalize large expressions.
           (let ((min-penalized-size 40)
                 (size (count-nodes expr)))
             (if (< size min-penalized-size)
                 1
                 (exp (min 120 (/ (- size min-penalized-size) 10d0)))))))

  When an expression is to undergo mutation, a randomizer function is
  called. Here we change literal numbers slightly, or produce an
  entirely new random expression that will be substituted for `EXPR`:

      (defun randomize (gp type expr)
        (if (and (numberp expr)
                 (< (random 1.0) 0.5))
            (+ expr (random 1.0) -0.5)
            (random-gp-expression gp (lambda (level)
                                       (<= 3 level))
                                  :type type)))

  That's about it. Now we create a GP instance hooking everything up,
  set up the initial population and just call ADVANCE a couple of
  times to create new generations of expressions.

      (defun run ()
        (let ((*print-length* nil)
              (*print-level* nil)
              (gp (make-instance
                   'gp
                   :toplevel-type 'real
                   :operators *operators*
                   :literals *literals*
                   :population-size 1000
                   :copy-chance 0.0
                   :mutation-chance 0.5
                   :evaluator (lambda (gp expr)
                                (evaluate gp expr *target-expr*))
                   :randomizer 'randomize
                   :selector (lambda (gp fitnesses)
                               (declare (ignore gp))
                               (hold-tournament fitnesses :n-contestants 2))
                   :fittest-changed-fn
                   (lambda (gp fittest fitness)
                     (format t \"Best fitness until generation ~S: ~S for~%  ~S~%\"
                             (generation-counter gp) fitness fittest)))))
          (loop repeat (population-size gp) do
            (add-individual gp (random-gp-expression gp (lambda (level)
                                                          (<= 5 level)))))
          (loop repeat 1000 do
            (when (zerop (mod (generation-counter gp) 20))
              (format t \"Generation ~S~%\" (generation-counter gp)))
            (advance gp))
          (destructuring-bind (fittest . fitness) (fittest gp)
            (format t \"Best fitness: ~S for~%  ~S~%\" fitness fittest))))

  Note that this example can be found in
  example/symbolic-regression.lisp.")

(defsection @gpr-gp-expressions (:title "Expressions")
  "Genetic programming works with a population of individuals. The
  individuals are sexps that may be evaluated directly by EVAL or by
  other means. The internal nodes and the leafs of the sexp as a tree
  represent the application of operators and literal objects,
  respectively. Note that currently there is no way to represent
  literal lists."
  (expression-class class)
  (result-type (reader expression-class))
  (weight (reader expression-class))
  (operator class)
  (name (reader operator))
  (argument-types (reader operator))
  (operator macro)
  (literal class)
  (builder (reader literal))
  (literal macro)
  (random-expression function))

(defclass expression-class ()
  ((result-type
    :initarg :result-type
    :reader result-type
    :documentation "Expressions belonging to this expression class
    must evaluate to a value of this lisp type.")
   (weight
    :initform 1
    :initarg :weight
    :reader weight
    :documentation "The probability of an expression class to be
    selected from a set of candidates is proportional to its
    weight."))
  (:documentation "An object of EXPRESSION-CLASS defines two things:
  how to build a random expression that belongs to that expression
  class and what lisp type those expressions evaluate to."))

(defun random-expression-class (expression-classes)
  (random-element expression-classes :key #'weight))

(defclass operator (expression-class)
  ((name
    :initarg :name
    :reader name
    :documentation "A symbol that's the name of the operator.")
   (argument-types
    :initarg :argument-types
    :reader argument-types
    :documentation "A list of lisp types. One for each argument of
    this operator."))
  (:documentation "Defines how the symbol NAME in the function
  position of a list can be combined arguments: how many and of what
  types. The following defines [`+`][dislocated] as an operator that
  adds two `FLOAT`s:

      (make-instance 'operator 
                     :name '+
                     :result-type float
                     :argument-types '(float float))

  See the macro [OPERATOR][macro] for a shorthand for the above.

  Currently no lambda list keywords are supported and there is no way
  to define how an expression with a particular operator is to be
  built. See RANDOM-EXPRESSION."))

(defmethod print-object ((operator operator) stream)
  (print-unreadable-object (operator stream :type t)
    (format stream "(~S ~{~S~^ ~}) ~S" (name operator)
            (argument-types operator) (result-type operator))))

(defmacro operator ((name &rest arg-types) result-type &key (weight 1))
  "Syntactic sugar for instantiating operators. The example given for
  [OPERATOR][class] could be written as:

      (operator (+ float float) float)

  See [WEIGHT][(reader expression-class)] for what WEIGHT means."
  `(make-instance 'operator :name ',name :result-type ',result-type
                  :argument-types ',arg-types
                  :weight ,weight))

(defclass literal (expression-class)
  ((builder
    :initarg :builder
    :reader builder
    :documentation "A function of no arguments that returns a random
    literal that belongs to its literal class."))
  (:documentation "This is slightly misnamed. An object belonging to
  the LITERAL class is not a literal itself, it's a factory for
  literals via its BUILDER function. For example, the following
  literal builds bytes:

      (make-instance 'literal
                     :result-type '(unsigned-byte 8)
                     :builder (lambda () (random 256)))

  In practice, one rarely writes it out like that, because the LITERAL
  macro provides a more convenient shorthand."))

(defmethod print-object ((literal literal) stream)
  (print-unreadable-object (literal stream :type t :identity t)
    (format stream "~S" (result-type literal))))

(defmacro literal ((result-type &key (weight 1)) &body body)
  "Syntactic sugar for defining literal classes. The example given for
  [LITERAL][class] could be written as:

      (literal ((unsigned-byte 8))
        (random 256))

  See [WEIGHT][(reader expression-class)] for what WEIGHT means."
  `(make-instance 'literal :result-type ',result-type
                  :weight ,weight
                  :builder (lambda () ,@body)))

(defun random-expression (operators literals type terminate-fn)
  "Return an expression built from OPERATORS and LITERALS that
  evaluates to values of TYPE. TERMINATE-FN is a function of one
  argument: the level of the root of the subexpression to be generated
  in the context of the entire expression. If it returns T then a
  [LITERAL][class] will be inserted (by calling its BUILDER function),
  else an [OPERATOR][class] with all its necessary arguments.

  The algorithm recursively generates the expression starting from
  level 0 where only operators and literals with a RESULT-TYPE that's
  a subtype of TYPE are considered and one is selected with the
  unnormalized probability given by its WEIGHT. On lower levels, the
  ARGUMENT-TYPES specification of operators is similarly satisfied and
  the resulting expression should evaluate without without a type
  error.

  The building of expressions cannot backtrack. If it finds itself in
  a situation where no literals or operators of the right type are
  available then it will fail with an error."
  (labels ((expression-classes-of-type (expression-classes type)
             (loop for expression-class in expression-classes
                   when (subtypep (result-type expression-class) type)
                     collect expression-class))
           (random-literal (type)
             (let ((literals (expression-classes-of-type literals type)))
               (assert literals () "No literals of type ~S available."
                       type)
               (funcall (builder (random-expression-class literals)))))
           (random-operator (type level)
             (let ((operators (expression-classes-of-type operators type)))
               (if (endp operators)
                   (random-literal type)
                   (let ((operator (random-expression-class operators)))
                     (cons (name operator)
                           (mapcar #'(lambda (type)
                                       (random-operator-or-literal
                                        type (1+ level)))
                                   (argument-types operator)))))))
           (random-operator-or-literal (type level)
             (if (funcall terminate-fn level)
                 (random-literal type)
                 (random-operator type level))))
    (random-operator-or-literal type 0)))


(defsection @gpr-gp-basics (:title "Basics")
  "To start the evolutionary process one creates a GP object,
  adds to it the individuals (see ADD-INDIVIDUAL) that make up the
  initial population and calls ADVANCE in a loop to move on to the
  next generation."
  (genetic-programming class)
  (random-gp-expression function))

(defclass genetic-programming (evolutionary-algorithm)
  ((operators
    :initarg :operators
    :reader operators
    :documentation "The set of [OPERATOR][class]s from which (together
    with [LITERAL][class]s) individuals are built.")
   (literals
    :initarg :literals
    :reader literals
    :documentation "The set of [LITERAL][class]s from which (together
    with [OPERATOR][class]s) individuals are built.")
   (toplevel-type
    :initform t
    :initarg :toplevel-type
    :reader toplevel-type
    :documentation "The type of the results produced by individuals.
    If the problem is to find the minimum a 1d real function then this
    may be the symbol REAL. If the problem is to find the shortest
    route, then this may be a vector. It all depends on the
    representation of the problem, the operators and the literals.")
   (randomizer
    :initarg :randomizer
    :reader randomizer
    :documentation "Used for mutations, this is a function of three
    arguments: the GP object, the type the expression must produce and
    current expression to be replaced with the returned value. It is
    called with subexpressions of individuals.")
   (selector
    :initarg :selector
    :reader selector
    :documentation "A function of two arguments: the GP object and a
    vector of fitnesses. It must return the and index into the fitness
    vector. The individual whose fitness was thus selected will be
    selected for reproduction be it copying, mutation or crossover.
    Typically, this defers to HOLD-TOURNAMENT.")
   (copy-chance
    :initform 0
    :initarg :copy-chance
    :accessor copy-chance
    :documentation "The probability of the copying reproduction
    operator being chosen. Copying simply creates an exact copy of a
    single individual.")
   (mutation-chance
    :initform 0
    :initarg :mutation-chance
    :accessor mutation-chance
    :documentation "The probability of the mutation reproduction
    operator being chosen. Mutation creates a randomly altered copy of
    an individual. See RANDOMIZER.")
   (keep-fittest-p
    :initform t
    :initarg :keep-fittest-p
    :accessor keep-fittest-p
    :documentation "If true, then the fittest individual is always
    copied without mutation to the next generation. Of course, it may
    also have other offsprings."))
  (:documentation "The GENETIC-PROGRAMMING class defines the search
  space, how mutation and recombination occur, and hold various
  parameters of the evolutionary process and the individuals
  themselves."))

(defun random-gp-expression (gp terminate-fn &key (type (toplevel-type gp)))
  "Creating the initial population by hand is tedious. This
  convenience function calls RANDOM-EXPRESSION to create a random
  individual that produces GP's TOPLEVEL-TYPE. By passing in another
  TYPE one can create expressions that fit somewhere else in a larger
  expression which is useful in a RANDOMIZER function."
  (random-expression (operators gp) (literals gp) type terminate-fn))

(defmethod advance ((gp genetic-programming))
  (calculate-fitnesses gp)
  (incf (slot-value gp 'generation-counter))
  (breed gp)
  (rotatef (population gp) (nursery gp)))

(defsection @gpr-gp-search-space (:title "Search Space")
  "The search space of the GP is defined by the available operators,
  literals and the type of the final result produced. The evaluator
  function acts as the guiding light."
  (operators (reader genetic-programming))
  (literals (reader genetic-programming))
  (toplevel-type (reader genetic-programming))
  (count-nodes function))

(defsection @gpr-gp-reproduction (:title "Reproduction")
  "The RANDOMIZER and SELECTOR functions define how mutation and
  recombination occur."
  (randomizer (reader genetic-programming))
  (selector (reader genetic-programming))
  (hold-tournament function))

(defsection @gpr-gp-environment (:title "Environment")
  "The new generation is created by applying a reproduction operator
  until POPULATION-SIZE is reached in the new generation. At each
  step, a reproduction operator is randomly chosen."
  (copy-chance (accessor genetic-programming))
  (mutation-chance (accessor genetic-programming))
  "If neither copying nor mutation were chosen, then a crossover will
  take place."
  (keep-fittest-p (accessor genetic-programming)))

;;; Calculate the fitnesses of the current generation. Place them into
;;; FITNESSES. Update FITTEST if necessary.
(defun calculate-fitnesses (gp)
  (let ((fitnesses (fitnesses gp))
        (evaluator (evaluator gp))
        (mass-evaluator (mass-evaluator gp))
        (population (population gp))
        (fittest (fittest gp))
        (changedp nil)
        (fitness-key (fitness-key gp)))
    (unless (= (length fitnesses) (length population))
      (setq fitnesses (make-array (length population)))
      (setf (slot-value gp 'fitnesses) fitnesses))
    (fill fitnesses nil)
    (if mass-evaluator
        (funcall mass-evaluator gp population fitnesses)
        (map-into fitnesses (lambda (individual)
                              (funcall evaluator gp individual))
                  population))
    (loop for individual across population
          for fitness across fitnesses
          do (let ((real-fitness (funcall fitness-key fitness)))
               (when (or (null fittest)
                         (< (funcall fitness-key (cdr fittest))
                            real-fitness))
                 (setq fittest (cons individual fitness))
                 (setf (slot-value gp 'fittest) fittest)
                 (setq changedp t))))
    (when (and changedp (fittest-changed-fn gp))
      (funcall (fittest-changed-fn gp) gp
               (car (fittest gp)) (cdr (fittest gp))))))

(defun breed (gp)
  (when (null (nursery gp))
    (setf (slot-value gp 'nursery)
          (make-array 0 :adjustable 0 :fill-pointer t)))
  (let* ((population (population gp))
         (n (population-size gp))
         (copy-chance (copy-chance gp))
         (mutation-chance (mutation-chance gp))
         (nursery (nursery gp)))
    (setf (fill-pointer nursery) 0)
    (when (keep-fittest-p gp)
      (let ((i (nth-value 1 (max-position/vector (fitnesses gp)
                                                 :key (fitness-key gp)))))
        (vector-push-extend (aref population i) nursery)))
    (loop while (< (length nursery) n) do
      (let* ((what (random 1.0)))
        (cond ((< what copy-chance)
               (copy-some gp population nursery))
              ((< what (+ copy-chance mutation-chance))
               (mutate-some gp population nursery))
              (t
               (crossover-some gp population nursery)))))))

(defun copy-some (gp population nursery)
  (let ((selector (selector gp))
        (fitnesses (fitnesses gp)))
    (let* ((xi (funcall selector gp fitnesses))
           (x (aref population xi)))
      (vector-push-extend x nursery))))

(defun mutate-some (gp population nursery)
  (let ((selector (selector gp))
        (fitnesses (fitnesses gp)))
    (let* ((xi (funcall selector gp fitnesses))
           (x (aref population xi)))
      (vector-push-extend (mutate-expression gp x) nursery))))

(defun crossover-some (gp population nursery)
  (let ((n (population-size gp))
        (selector (selector gp))
        (fitnesses (fitnesses gp)))
    (let* ((xi (funcall selector gp fitnesses))
           (x (aref population xi)))
      (let ((y (aref population (funcall selector gp fitnesses))))
        (multiple-value-bind (a b) (crossover-expressions gp x y)
          (vector-push-extend a nursery)
          (when (< (length nursery) n)
            (vector-push-extend b nursery)))))))

(defun mutate-expression (gp x)
  (let* ((xs (count-nodes x))
         (xi (random xs))
         (type (node-expected-type gp x xi)))
    (change-node-at x xi (funcall (randomizer gp) gp type (node-at x xi)))))

(defun crossover-expressions (gp x y)
  (loop for i upfrom 1 do
    (when (zerop (mod i 1000))
      (warn "Crossover having trouble with ~A ~A ~A~%" gp x y))
    ;; Literals more likely to be selected than operators because
    ;; there are more of them in the tree. To counter this, following
    ;; Koza, choose and internal node 90% of the time.
    (let* ((x-internal (and (not (atom x)) (< (random 1.0) 0.9)))
           (y-internal (and (not (atom y)) (< (random 1.0) 0.9)))
           (xs (count-nodes x :internal x-internal))
           (ys (count-nodes y :internal y-internal))
           (xi (random xs))
           (yi (random ys))
           (xin (node-at x xi :internal x-internal))
           (yin (node-at y yi :internal y-internal))
           (xi-type (node-expected-type gp x xi :internal x-internal))
           (yi-type (node-expected-type gp y yi :internal y-internal))
           (xi-expected-type (node-expected-type gp x xi
                                                 :internal x-internal))
           (yi-expected-type (node-expected-type gp y yi
                                                 :internal y-internal)))
      (when (and (subtypep xi-type yi-expected-type)
                 (subtypep yi-type xi-expected-type))
        (return-from crossover-expressions
          (values (change-node-at x xi yin :internal x-internal)
                  (change-node-at y yi xin :internal y-internal)))))))

;;; Return the type its parent expects of node XI of X.
(defun node-expected-type (gp x xi &key internal)
  (multiple-value-bind (parent index)
      (parent-of-node-at x xi :internal internal)
    (if parent
        (elt (argument-types
              (find-operator gp (first (node-at x parent :internal internal))))
             index)
        (toplevel-type gp))))

(defun find-operator (gp name)
  (or (find name (operators gp) :key #'name)
      (error "Operator ~S is undefined." name)))

(defun hold-tournament (fitnesses &key select-contestant-fn n-contestants key)
  "Select N-CONTESTANTS (all different) for the tournament randomly,
  represented by indices into FITNESSES and return the one with the
  highest fitness. If SELECT-CONTESTANT-FN is NIL then contestants are
  selected randomly with uniform probability. If SELECT-CONTESTANT-FN
  is a function, then it's called with FITNESSES to return an
  index (that may or may not be already selected for the tournament).
  Specifying SELECT-CONTESTANT-FN allows one to conduct 'local'
  tournaments biased towards a particular region of the index range.
  KEY is NIL or a function that select the real fitness value from
  elements of FITNESSES."
  (let* ((n (length fitnesses))
         (size (min n-contestants n))
         (v (make-array size :adjustable t :fill-pointer 0))
         (key (or key #'identity)))
    (loop while (< (length v) size) do
      (let ((i (if select-contestant-fn
                   (funcall select-contestant-fn fitnesses)
                   (random n))))
        (unless (find i v)
          (insert-into-sorted-vector
           i v
           :test (lambda (x y)
                   (> (funcall key (aref fitnesses x))
                      (funcall key (aref fitnesses y))))))))
    (aref v 0)))
