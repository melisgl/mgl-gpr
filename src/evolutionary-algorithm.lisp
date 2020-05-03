(in-package :mgl-gpr)

(defsection @gpr-manual (:title "GPR Manual")
  (mgl-gpr asdf:system)
  (@gpr-gp-links section)
  (@gpr-background section)
  (@gpr-ea section)
  (@gpr-gp section)
  (@gpr-de section))

(defsection @gpr-gp-links (:title "Links")
  "Here is the [official
  repository](https://github.com/melisgl/mgl-gpr) and the [HTML
  documentation](http://melisgl.github.io/mgl-gpr/gpr-manual.html)
  for the latest version.")

(defsection @gpr-background (:title "Background")
  "Evolutionary algorithms are optimization tools that assume little
  of the task at hand. Often they are population based, that is, there
  is a set of individuals that each represent a candidate solution.
  Individuals are combined and changed with crossover and mutationlike
  operators to produce the next generation. Individuals with lower
  fitness have a lower probability to survive than those with higher
  fitness. In this way, the fitness function defines the optimization
  task.

  Typically, EAs are quick to get up and running, can produce
  reasonable results across a wild variety of domains, but they may
  need a bit of fiddling to perform well and domain specific
  approaches will almost always have better results. All in all, EA
  can be very useful to cut down on the tedium of human trial and
  error. However, they have serious problems scaling to higher number
  of variables.

  This library grew from the Genetic Programming implementation I
  wrote while working for Ravenpack who agreed to release it under an
  MIT licence. Several years later I cleaned it up, and documented it.
  Enjoy.")

(defsection @gpr-ea (:title "Evolutionary Algorithms")
  "Evolutionary algorithm is an umbrella term. In this section we
  first discuss the concepts common to conrete evolutionary algorithms
  @GPR-GP and @GPR-DE."
  (evolutionary-algorithm class)
  (@gpr-ea-population section)
  (@gpr-ea-evaluation section)
  (@gpr-ea-training section))

(defsection @gpr-ea-population (:title "Populations")
  "The currenly implemented EAs are generational. That is, they
  maintain a population of candidate solutions (also known as
  individuals) which they replace with the next generation of
  individuals."
  (population-size (accessor evolutionary-algorithm))
  (population (accessor evolutionary-algorithm))
  (generation-counter (reader evolutionary-algorithm))
  (add-individual function))

(defun add-individual (ea individual)
  "Adds INDIVIDUAL to POPULATION of EA. Usually called when
  initializing the EA."
  (vector-push-extend individual (population ea)))

(defsection @gpr-ea-evaluation (:title "Evaluation")
  (evaluator (reader evolutionary-algorithm))
  (mass-evaluator (reader evolutionary-algorithm))
  (fitness-key (reader evolutionary-algorithm)))

(defsection @gpr-ea-training (:title "Training")
  "Training is easy: one creates an object of a subclass of
  EVOLUTIONARY-ALGORITHM such as GENETIC-PROGRAMMING or
  DIFFERENTIAL-EVOLUTION, creates the initial population by adding
  individuals to it (see ADD-INDIVIDUAL) and calls ADVANCE in a loop
  to move on to the next generation until a certain number of
  generations or until the FITTEST individual is good enough."
  (advance generic-function)
  (fittest (reader evolutionary-algorithm))
  (fittest-changed-fn (accessor evolutionary-algorithm)))

(defgeneric advance (ea)
  (:documentation "Create the next generation and place it in
  POPULATION of EA."))

(defclass evolutionary-algorithm ()
  ((evaluator
    :initarg :evaluator
    :reader evaluator
    :documentation "A function of two arguments: the
    EVOLUTIONARY-ALGORITHM object and an individual. It must return
    the fitness of the individual. For @GPR-GP, the evaluator often
    simply calls EVAL, or COMPILE + FUNCALL, and compares the result
    to some gold standard. It is also typical to slightly penalize
    solutions with too many nodes to control complexity and evaluation
    cost (see COUNT-NODES). For @GPR-DE, individuals are
    conceptually (and often implemented as) vectors of numbers so the
    fitness function may include an L1 or L2 penalty term.

    Alternatively, one can specify MASS-EVALUATOR instead.")
   (mass-evaluator
    :initform nil
    :initarg :mass-evaluator
    :reader mass-evaluator
    :documentation "NIL or a function of three arguments: the
    EVOLUTIONARY-ALGORITHM object, the population vector and the
    fitness vector into which the fitnesses of the individuals in the
    population vector shall be written. By specifying MASS-EVALUATOR
    instead of an EVALUATOR, one can, for example, distribute costly
    evaluations over multiple threads. MASS-EVALUATOR has precedence
    over EVALUATOR.")
   (fitness-key
    :initform #'identity
    :initarg :fitness-key
    :reader fitness-key
    :documentation "A function that returns a real number for an
    object returned by EVALUATOR. It is called when two fitness are to
    be compared. The default value is #'IDENTITY which is sufficient
    when EVALUATOR returns real numbers. However, sometimes the
    evaluator returns more information about the solution (such as
    fitness in various situations) and FITNESS-KEY key be used to
    select the fitness value.")
   (generation-counter
    :initform 0
    :reader generation-counter
    :documentation "A counter that starts from 0 and is incremented by
    ADVANCE. All accessors of EVOLUTIONARY-ALGORITHM are allowed to be
    specialized on a subclass which allows them to be functions of
    GENERATION-COUNTER.")
   (population-size
    :initarg :population-size
    :accessor population-size
    :documentation "The number of individuals in a generation. This is
    a very important parameter. Too low and there won't be enough
    diversity in the population, too high and convergence will be
    slow.")
   (fittest
    :initform nil
    :reader fittest
    :documentation "The fittest individual ever to be seen and its
    fittness as a cons cell.")
   (fittest-changed-fn
    :initform nil
    :initarg :fittest-changed-fn
    :accessor fittest-changed-fn
    :documentation "If non-NIL, a function that's called when FITTEST
    is updated with three arguments: the EVOLUTIONARY-ALGORITHM
    object, the fittest individual and its fitness. Useful for
    tracking progress.")
   (population
    :initform (make-array 0 :adjustable 0 :fill-pointer t)
    :accessor population
    :documentation "An adjustable array with a fill-pointer that holds
    the individuals that make up the population.")
   ;; This is where newborns are temporarily stored, before it is
   ;; swapped with POPULATION.
   (nursery
    :initform nil
    :accessor nursery)
   ;; The fitness values of each individual in POPULATION.
   (fitnesses
    :initform nil
    :accessor fitnesses))
  (:documentation "The EVOLUTIONARY-ALGORITHM is an abstract base
  class for generational, population based optimization algorithms."))
