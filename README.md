<a id="x-28MGL-GPR-3A-40GPR-MANUAL-20MGL-PAX-3ASECTION-29"></a>
# GPR Manual

## Table of Contents

- [1 `MGL-GPR` ASDF System][34da]
- [2 Links][6e4d]
- [3 Background][2e8a]
- [4 Evolutionary Algorithms][057a]
    - [4.1 Populations][9cba]
    - [4.2 Evaluation][b60a]
    - [4.3 Training][e87f]
- [5 Genetic Programming][0d2f]
    - [5.1 Background][c392]
    - [5.2 Tutorial][075b]
    - [5.3 Expressions][f9a4]
    - [5.4 Basics][e084]
    - [5.5 Search Space][9085]
    - [5.6 Reproduction][5d57]
    - [5.7 Environment][084d]
- [6 Differential Evolution][db20]
    - [6.1 SANSDE][40a0]

###### \[in package MGL-GPR\]
<a id="x-28-22mgl-gpr-22-20ASDF-2FSYSTEM-3ASYSTEM-29"></a>
## 1 `MGL-GPR` ASDF System

- Version: 0.0.1
- Description: [`MGL-GPR`][34da] is a library of evolutionary algorithms such
  as Genetic Programming (evolving typed expressions from a set of
  operators and constants) and Differential Evolution.
- Licence: MIT, see COPYING.
- Author: Gábor Melis <mega@retes.hu>
- Mailto: [mega@retes.hu](mailto:mega@retes.hu)
- Homepage: [http://melisgl.github.io/mgl-gpr](http://melisgl.github.io/mgl-gpr)
- Bug tracker: [https://github.com/melisgl/mgl-gpr/issues](https://github.com/melisgl/mgl-gpr/issues)
- Source control: [GIT](https://github.com/melisgl/mgl-gpr.git)

<a id="x-28MGL-GPR-3A-40GPR-GP-LINKS-20MGL-PAX-3ASECTION-29"></a>
## 2 Links

Here is the [official
repository](https://github.com/melisgl/mgl-gpr) and the [HTML
documentation](http://melisgl.github.io/mgl-gpr/gpr-manual.html)
for the latest version.

<a id="x-28MGL-GPR-3A-40GPR-BACKGROUND-20MGL-PAX-3ASECTION-29"></a>
## 3 Background

Evolutionary algorithms are optimization tools that assume little
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
Enjoy.

<a id="x-28MGL-GPR-3A-40GPR-EA-20MGL-PAX-3ASECTION-29"></a>
## 4 Evolutionary Algorithms

Evolutionary algorithm is an umbrella term. In this section we
first discuss the concepts common to conrete evolutionary algorithms
[Genetic Programming][0d2f] and [Differential Evolution][db20].

<a id="x-28MGL-GPR-3AEVOLUTIONARY-ALGORITHM-20CLASS-29"></a>
- [class] **EVOLUTIONARY-ALGORITHM**

    The `EVOLUTIONARY-ALGORITHM` is an abstract base
    class for generational, population based optimization algorithms.

<a id="x-28MGL-GPR-3A-40GPR-EA-POPULATION-20MGL-PAX-3ASECTION-29"></a>
### 4.1 Populations

The currenly implemented EAs are generational. That is, they
maintain a population of candidate solutions (also known as
individuals) which they replace with the next generation of
individuals.

<a id="x-28MGL-GPR-3APOPULATION-SIZE-20-28MGL-PAX-3AACCESSOR-20MGL-GPR-3AEVOLUTIONARY-ALGORITHM-29-29"></a>
- [accessor] **POPULATION-SIZE** *EVOLUTIONARY-ALGORITHM (:POPULATION-SIZE)*

    The number of individuals in a generation. This is
    a very important parameter. Too low and there won't be enough
    diversity in the population, too high and convergence will be
    slow.

<a id="x-28MGL-GPR-3APOPULATION-20-28MGL-PAX-3AACCESSOR-20MGL-GPR-3AEVOLUTIONARY-ALGORITHM-29-29"></a>
- [accessor] **POPULATION** *EVOLUTIONARY-ALGORITHM (= (MAKE-ARRAY 0 :ADJUSTABLE 0 :FILL-POINTER T))*

    An adjustable array with a fill-pointer that holds
    the individuals that make up the population.

<a id="x-28MGL-GPR-3AGENERATION-COUNTER-20-28MGL-PAX-3AREADER-20MGL-GPR-3AEVOLUTIONARY-ALGORITHM-29-29"></a>
- [reader] **GENERATION-COUNTER** *EVOLUTIONARY-ALGORITHM (= 0)*

    A counter that starts from 0 and is incremented by
    [`ADVANCE`][be13]. All accessors of [`EVOLUTIONARY-ALGORITHM`][f5ee] are allowed to be
    specialized on a subclass which allows them to be functions of
    `GENERATION-COUNTER`.

<a id="x-28MGL-GPR-3AADD-INDIVIDUAL-20FUNCTION-29"></a>
- [function] **ADD-INDIVIDUAL** *EA INDIVIDUAL*

    Adds `INDIVIDUAL` to [`POPULATION`][1602] of `EA`. Usually called when
    initializing the `EA`.

<a id="x-28MGL-GPR-3A-40GPR-EA-EVALUATION-20MGL-PAX-3ASECTION-29"></a>
### 4.2 Evaluation

<a id="x-28MGL-GPR-3AEVALUATOR-20-28MGL-PAX-3AREADER-20MGL-GPR-3AEVOLUTIONARY-ALGORITHM-29-29"></a>
- [reader] **EVALUATOR** *EVOLUTIONARY-ALGORITHM (:EVALUATOR)*

    A function of two arguments: the
    [`EVOLUTIONARY-ALGORITHM`][f5ee] object and an individual. It must return
    the fitness of the individual. For [Genetic Programming][0d2f], the evaluator often
    simply calls [`EVAL`][c1eb], or [`COMPILE`][8c75] + [`FUNCALL`][6b4a], and compares the result
    to some gold standard. It is also typical to slightly penalize
    solutions with too many nodes to control complexity and evaluation
    cost (see [`COUNT-NODES`][d237]). For [Differential Evolution][db20], individuals are
    conceptually (and often implemented as) vectors of numbers so the
    fitness function may include an L1 or L2 penalty term.
    
    Alternatively, one can specify [`MASS-EVALUATOR`][ef7a] instead.

<a id="x-28MGL-GPR-3AMASS-EVALUATOR-20-28MGL-PAX-3AREADER-20MGL-GPR-3AEVOLUTIONARY-ALGORITHM-29-29"></a>
- [reader] **MASS-EVALUATOR** *EVOLUTIONARY-ALGORITHM (:MASS-EVALUATOR = NIL)*

    `NIL` or a function of three arguments: the
    [`EVOLUTIONARY-ALGORITHM`][f5ee] object, the population vector and the
    fitness vector into which the fitnesses of the individuals in the
    population vector shall be written. By specifying `MASS-EVALUATOR`
    instead of an [`EVALUATOR`][9a7e], one can, for example, distribute costly
    evaluations over multiple threads. `MASS-EVALUATOR` has precedence
    over `EVALUATOR`.

<a id="x-28MGL-GPR-3AFITNESS-KEY-20-28MGL-PAX-3AREADER-20MGL-GPR-3AEVOLUTIONARY-ALGORITHM-29-29"></a>
- [reader] **FITNESS-KEY** *EVOLUTIONARY-ALGORITHM (:FITNESS-KEY = #'IDENTITY)*

    A function that returns a real number for an
    object returned by [`EVALUATOR`][9a7e]. It is called when two fitness are to
    be compared. The default value is #'[`IDENTITY`][8804] which is sufficient
    when `EVALUATOR` returns real numbers. However, sometimes the
    evaluator returns more information about the solution (such as
    fitness in various situations) and `FITNESS-KEY` key be used to
    select the fitness value.

<a id="x-28MGL-GPR-3A-40GPR-EA-TRAINING-20MGL-PAX-3ASECTION-29"></a>
### 4.3 Training

Training is easy: one creates an object of a subclass of
[`EVOLUTIONARY-ALGORITHM`][f5ee] such as [`GENETIC-PROGRAMMING`][4643] or
[`DIFFERENTIAL-EVOLUTION`][a4e6], creates the initial population by adding
individuals to it (see [`ADD-INDIVIDUAL`][4a1e]) and calls [`ADVANCE`][be13] in a loop
to move on to the next generation until a certain number of
generations or until the [`FITTEST`][9631] individual is good enough.

<a id="x-28MGL-GPR-3AADVANCE-20GENERIC-FUNCTION-29"></a>
- [generic-function] **ADVANCE** *EA*

    Create the next generation and place it in
    [`POPULATION`][1602] of `EA`.

<a id="x-28MGL-GPR-3AFITTEST-20-28MGL-PAX-3AREADER-20MGL-GPR-3AEVOLUTIONARY-ALGORITHM-29-29"></a>
- [reader] **FITTEST** *EVOLUTIONARY-ALGORITHM (= NIL)*

    The fittest individual ever to be seen and its
    fittness as a cons cell.

<a id="x-28MGL-GPR-3AFITTEST-CHANGED-FN-20-28MGL-PAX-3AACCESSOR-20MGL-GPR-3AEVOLUTIONARY-ALGORITHM-29-29"></a>
- [accessor] **FITTEST-CHANGED-FN** *EVOLUTIONARY-ALGORITHM (:FITTEST-CHANGED-FN = NIL)*

    If non-NIL, a function that's called when [`FITTEST`][9631]
    is updated with three arguments: the [`EVOLUTIONARY-ALGORITHM`][f5ee]
    object, the fittest individual and its fitness. Useful for
    tracking progress.

<a id="x-28MGL-GPR-3A-40GPR-GP-20MGL-PAX-3ASECTION-29"></a>
## 5 Genetic Programming

<a id="x-28MGL-GPR-3A-40GPR-GP-BACKGROUND-20MGL-PAX-3ASECTION-29"></a>
### 5.1 Background

What is Genetic Programming? This is what Wikipedia has to say:

    In artificial intelligence, genetic programming (GP) is an
    evolutionary algorithm-based methodology inspired by biological
    evolution to find computer programs that perform a user-defined
    task. Essentially GP is a set of instructions and a fitness
    function to measure how well a computer has performed a task. It
    is a specialization of genetic algorithms (GA) where each
    individual is a computer program. It is a machine learning
    technique used to optimize a population of computer programs
    according to a fitness landscape determined by a program's ability
    to perform a given computational task.

Lisp has a long history of Genetic Programming because GP involves
manipulation of expressions which is of course particularly easy
with sexps.

<a id="x-28MGL-GPR-3A-40GPR-GP-TUTORIAL-20MGL-PAX-3ASECTION-29"></a>
### 5.2 Tutorial

GPR works with typed expressions. Mutation and crossover never
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
random expressions with [`RANDOM-EXPRESSION`][7df2], but we also need to
define how good a certain expression is which is called *fitness*.

In this example, we are going to perform symbolic regression, that
is, try to find an expression that approximates some target
expression well:

    (defparameter *target-expr* '(+ 7 (sin (expt (* *x* 2 pi) 2))))

Think of `*TARGET-EXPR*` as a function of `*X*`. The evaluator
function will bind the special `*X*` to the input and simply [`EVAL`][c1eb]
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
set up the initial population and just call [`ADVANCE`][be13] a couple of
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
                   (format t "Best fitness until generation ~S: ~S for~%  ~S~%"
                           (generation-counter gp) fitness fittest)))))
        (loop repeat (population-size gp) do
          (add-individual gp (random-gp-expression gp (lambda (level)
                                                        (<= 5 level)))))
        (loop repeat 1000 do
          (when (zerop (mod (generation-counter gp) 20))
            (format t "Generation ~S~%" (generation-counter gp)))
          (advance gp))
        (destructuring-bind (fittest . fitness) (fittest gp)
          (format t "Best fitness: ~S for~%  ~S~%" fitness fittest))))

Note that this example can be found in
example/symbolic-regression.lisp.

<a id="x-28MGL-GPR-3A-40GPR-GP-EXPRESSIONS-20MGL-PAX-3ASECTION-29"></a>
### 5.3 Expressions

Genetic programming works with a population of individuals. The
individuals are sexps that may be evaluated directly by [`EVAL`][c1eb] or by
other means. The internal nodes and the leafs of the sexp as a tree
represent the application of operators and literal objects,
respectively. Note that currently there is no way to represent
literal lists.

<a id="x-28MGL-GPR-3AEXPRESSION-CLASS-20CLASS-29"></a>
- [class] **EXPRESSION-CLASS**

    An object of `EXPRESSION-CLASS` defines two things:
    how to build a random expression that belongs to that expression
    class and what lisp type those expressions evaluate to.

<a id="x-28MGL-GPR-3ARESULT-TYPE-20-28MGL-PAX-3AREADER-20MGL-GPR-3AEXPRESSION-CLASS-29-29"></a>
- [reader] **RESULT-TYPE** *EXPRESSION-CLASS (:RESULT-TYPE)*

    Expressions belonging to this expression class
    must evaluate to a value of this lisp type.

<a id="x-28MGL-GPR-3AWEIGHT-20-28MGL-PAX-3AREADER-20MGL-GPR-3AEXPRESSION-CLASS-29-29"></a>
- [reader] **WEIGHT** *EXPRESSION-CLASS (:WEIGHT = 1)*

    The probability of an expression class to be
    selected from a set of candidates is proportional to its
    weight.

<a id="x-28MGL-GPR-3AOPERATOR-20CLASS-29"></a>
- [class] **OPERATOR** *[EXPRESSION-CLASS][95f7]*

    Defines how the symbol [`NAME`][12f5] in the function
    position of a list can be combined arguments: how many and of what
    types. The following defines `+` as an operator that
    adds two `FLOAT`([`0`][bbfb] [`1`][1fc4])s:
    
        (make-instance 'operator 
                       :name '+
                       :result-type float
                       :argument-types '(float float))
    
    See the macro [`OPERATOR`][5b15] for a shorthand for the above.
    
    Currently no lambda list keywords are supported and there is no way
    to define how an expression with a particular operator is to be
    built. See [`RANDOM-EXPRESSION`][7df2].

<a id="x-28MGL-GPR-3ANAME-20-28MGL-PAX-3AREADER-20MGL-GPR-3AOPERATOR-29-29"></a>
- [reader] **NAME** *OPERATOR (:NAME)*

    A symbol that's the name of the operator.

<a id="x-28MGL-GPR-3AARGUMENT-TYPES-20-28MGL-PAX-3AREADER-20MGL-GPR-3AOPERATOR-29-29"></a>
- [reader] **ARGUMENT-TYPES** *OPERATOR (:ARGUMENT-TYPES)*

    A list of lisp types. One for each argument of
    this operator.

<a id="x-28MGL-GPR-3AOPERATOR-20MGL-PAX-3AMACRO-29"></a>
- [macro] **OPERATOR** *(NAME &REST ARG-TYPES) RESULT-TYPE &KEY (WEIGHT 1)*

    Syntactic sugar for instantiating operators. The example given for
    [`OPERATOR`][b75b] could be written as:
    
        (operator (+ float float) float)
    
    See [`WEIGHT`][be2e] for what `WEIGHT` means.

<a id="x-28MGL-GPR-3ALITERAL-20CLASS-29"></a>
- [class] **LITERAL** *[EXPRESSION-CLASS][95f7]*

    This is slightly misnamed. An object belonging to
    the [`LITERAL`][32b7] class is not a literal itself, it's a factory for
    literals via its [`BUILDER`][0cb2] function. For example, the following
    literal builds bytes:
    
        (make-instance 'literal
                       :result-type '(unsigned-byte 8)
                       :builder (lambda () (random 256)))
    
    In practice, one rarely writes it out like that, because the [`LITERAL`][8a46]
    macro provides a more convenient shorthand.

<a id="x-28MGL-GPR-3ABUILDER-20-28MGL-PAX-3AREADER-20MGL-GPR-3ALITERAL-29-29"></a>
- [reader] **BUILDER** *LITERAL (:BUILDER)*

    A function of no arguments that returns a random
    literal that belongs to its literal class.

<a id="x-28MGL-GPR-3ALITERAL-20MGL-PAX-3AMACRO-29"></a>
- [macro] **LITERAL** *(RESULT-TYPE &KEY (WEIGHT 1)) &BODY BODY*

    Syntactic sugar for defining literal classes. The example given for
    [`LITERAL`][32b7] could be written as:
    
        (literal ((unsigned-byte 8))
          (random 256))
    
    See [`WEIGHT`][be2e] for what `WEIGHT` means.

<a id="x-28MGL-GPR-3ARANDOM-EXPRESSION-20FUNCTION-29"></a>
- [function] **RANDOM-EXPRESSION** *OPERATORS LITERALS TYPE TERMINATE-FN*

    Return an expression built from `OPERATORS` and `LITERALS` that
    evaluates to values of `TYPE`. `TERMINATE-FN` is a function of one
    argument: the level of the root of the subexpression to be generated
    in the context of the entire expression. If it returns `T` then a
    [`LITERAL`][32b7] will be inserted (by calling its [`BUILDER`][0cb2] function),
    else an [`OPERATOR`][b75b] with all its necessary arguments.
    
    The algorithm recursively generates the expression starting from
    level 0 where only operators and literals with a [`RESULT-TYPE`][4e03] that's
    a subtype of `TYPE` are considered and one is selected with the
    unnormalized probability given by its [`WEIGHT`][be2e]. On lower levels, the
    [`ARGUMENT-TYPES`][a92c] specification of operators is similarly satisfied and
    the resulting expression should evaluate without without a type
    error.
    
    The building of expressions cannot backtrack. If it finds itself in
    a situation where no literals or operators of the right type are
    available then it will fail with an error.

<a id="x-28MGL-GPR-3A-40GPR-GP-BASICS-20MGL-PAX-3ASECTION-29"></a>
### 5.4 Basics

To start the evolutionary process one creates a GP object,
adds to it the individuals (see [`ADD-INDIVIDUAL`][4a1e]) that make up the
initial population and calls [`ADVANCE`][be13] in a loop to move on to the
next generation.

<a id="x-28MGL-GPR-3AGENETIC-PROGRAMMING-20CLASS-29"></a>
- [class] **GENETIC-PROGRAMMING** *[EVOLUTIONARY-ALGORITHM][f5ee]*

    The [`GENETIC-PROGRAMMING`][4643] class defines the search
    space, how mutation and recombination occur, and hold various
    parameters of the evolutionary process and the individuals
    themselves.

<a id="x-28MGL-GPR-3ARANDOM-GP-EXPRESSION-20FUNCTION-29"></a>
- [function] **RANDOM-GP-EXPRESSION** *GP TERMINATE-FN &KEY (TYPE (TOPLEVEL-TYPE GP))*

    Creating the initial population by hand is tedious. This
    convenience function calls [`RANDOM-EXPRESSION`][7df2] to create a random
    individual that produces `GP`'s [`TOPLEVEL-TYPE`][5460]. By passing in another
    `TYPE` one can create expressions that fit somewhere else in a larger
    expression which is useful in a [`RANDOMIZER`][612d] function.

<a id="x-28MGL-GPR-3A-40GPR-GP-SEARCH-SPACE-20MGL-PAX-3ASECTION-29"></a>
### 5.5 Search Space

The search space of the GP is defined by the available operators,
literals and the type of the final result produced. The evaluator
function acts as the guiding light.

<a id="x-28MGL-GPR-3AOPERATORS-20-28MGL-PAX-3AREADER-20MGL-GPR-3AGENETIC-PROGRAMMING-29-29"></a>
- [reader] **OPERATORS** *GENETIC-PROGRAMMING (:OPERATORS)*

    The set of [`OPERATOR`][b75b]s from which (together
    with [`LITERAL`][32b7]s) individuals are built.

<a id="x-28MGL-GPR-3ALITERALS-20-28MGL-PAX-3AREADER-20MGL-GPR-3AGENETIC-PROGRAMMING-29-29"></a>
- [reader] **LITERALS** *GENETIC-PROGRAMMING (:LITERALS)*

    The set of [`LITERAL`][32b7]s from which (together
    with [`OPERATOR`][b75b]s) individuals are built.

<a id="x-28MGL-GPR-3ATOPLEVEL-TYPE-20-28MGL-PAX-3AREADER-20MGL-GPR-3AGENETIC-PROGRAMMING-29-29"></a>
- [reader] **TOPLEVEL-TYPE** *GENETIC-PROGRAMMING (:TOPLEVEL-TYPE = T)*

    The type of the results produced by individuals.
    If the problem is to find the minimum a 1d real function then this
    may be the symbol [`REAL`][a2f2]. If the problem is to find the shortest
    route, then this may be a vector. It all depends on the
    representation of the problem, the operators and the literals.

<a id="x-28MGL-GPR-3ACOUNT-NODES-20FUNCTION-29"></a>
- [function] **COUNT-NODES** *TREE &KEY INTERNAL*

    Count the nodes in the sexp `TREE`. If `INTERNAL` then don't count the
    leaves.

<a id="x-28MGL-GPR-3A-40GPR-GP-REPRODUCTION-20MGL-PAX-3ASECTION-29"></a>
### 5.6 Reproduction

The [`RANDOMIZER`][612d] and [`SELECTOR`][049c] functions define how mutation and
recombination occur.

<a id="x-28MGL-GPR-3ARANDOMIZER-20-28MGL-PAX-3AREADER-20MGL-GPR-3AGENETIC-PROGRAMMING-29-29"></a>
- [reader] **RANDOMIZER** *GENETIC-PROGRAMMING (:RANDOMIZER)*

    Used for mutations, this is a function of three
    arguments: the GP object, the type the expression must produce and
    current expression to be replaced with the returned value. It is
    called with subexpressions of individuals.

<a id="x-28MGL-GPR-3ASELECTOR-20-28MGL-PAX-3AREADER-20MGL-GPR-3AGENETIC-PROGRAMMING-29-29"></a>
- [reader] **SELECTOR** *GENETIC-PROGRAMMING (:SELECTOR)*

    A function of two arguments: the GP object and a
    vector of fitnesses. It must return the and index into the fitness
    vector. The individual whose fitness was thus selected will be
    selected for reproduction be it copying, mutation or crossover.
    Typically, this defers to [`HOLD-TOURNAMENT`][d40e].

<a id="x-28MGL-GPR-3AHOLD-TOURNAMENT-20FUNCTION-29"></a>
- [function] **HOLD-TOURNAMENT** *FITNESSES &KEY SELECT-CONTESTANT-FN N-CONTESTANTS KEY*

    Select `N-CONTESTANTS` (all different) for the tournament randomly,
    represented by indices into `FITNESSES` and return the one with the
    highest fitness. If `SELECT-CONTESTANT-FN` is `NIL` then contestants are
    selected randomly with uniform probability. If `SELECT-CONTESTANT-FN`
    is a function, then it's called with `FITNESSES` to return an
    index (that may or may not be already selected for the tournament).
    Specifying `SELECT-CONTESTANT-FN` allows one to conduct 'local'
    tournaments biased towards a particular region of the index range.
    `KEY` is `NIL` or a function that select the real fitness value from
    elements of `FITNESSES`.

<a id="x-28MGL-GPR-3A-40GPR-GP-ENVIRONMENT-20MGL-PAX-3ASECTION-29"></a>
### 5.7 Environment

The new generation is created by applying a reproduction operator
until [`POPULATION-SIZE`][ce25] is reached in the new generation. At each
step, a reproduction operator is randomly chosen.

<a id="x-28MGL-GPR-3ACOPY-CHANCE-20-28MGL-PAX-3AACCESSOR-20MGL-GPR-3AGENETIC-PROGRAMMING-29-29"></a>
- [accessor] **COPY-CHANCE** *GENETIC-PROGRAMMING (:COPY-CHANCE = 0)*

    The probability of the copying reproduction
    operator being chosen. Copying simply creates an exact copy of a
    single individual.

<a id="x-28MGL-GPR-3AMUTATION-CHANCE-20-28MGL-PAX-3AACCESSOR-20MGL-GPR-3AGENETIC-PROGRAMMING-29-29"></a>
- [accessor] **MUTATION-CHANCE** *GENETIC-PROGRAMMING (:MUTATION-CHANCE = 0)*

    The probability of the mutation reproduction
    operator being chosen. Mutation creates a randomly altered copy of
    an individual. See [`RANDOMIZER`][612d].

If neither copying nor mutation were chosen, then a crossover will
take place.

<a id="x-28MGL-GPR-3AKEEP-FITTEST-P-20-28MGL-PAX-3AACCESSOR-20MGL-GPR-3AGENETIC-PROGRAMMING-29-29"></a>
- [accessor] **KEEP-FITTEST-P** *GENETIC-PROGRAMMING (:KEEP-FITTEST-P = T)*

    If true, then the fittest individual is always
    copied without mutation to the next generation. Of course, it may
    also have other offsprings.

<a id="x-28MGL-GPR-3A-40GPR-DE-20MGL-PAX-3ASECTION-29"></a>
## 6 Differential Evolution

The concepts in this section are covered by [Differential
Evolution: A Survey of the State-of-the-Art][2f1d].

[1]: http://107.167.189.191/~piak/teaching/ec/ec2012/das-de-sota-2011.pdf 


<a id="x-28MGL-GPR-3ADIFFERENTIAL-EVOLUTION-20CLASS-29"></a>
- [class] **DIFFERENTIAL-EVOLUTION** *[EVOLUTIONARY-ALGORITHM][f5ee]*

    Differential evolution (DE) is an evolutionary
    algorithm in which individuals are represented by vectors of
    numbers. New individuals are created by taking linear combinations
    or by randomly swapping some of these numbers between two
    individuals.

<a id="x-28MGL-GPR-3AMAP-WEIGHTS-INTO-FN-20-28MGL-PAX-3AREADER-20MGL-GPR-3ADIFFERENTIAL-EVOLUTION-29-29"></a>
- [reader] **MAP-WEIGHTS-INTO-FN** *DIFFERENTIAL-EVOLUTION (:MAP-WEIGHTS-INTO-FN = #'MAP-INTO)*

    The vector of numbers (the 'weights') are most
    often stored in some kind of array. All individuals must have the
    same number of weights, but the actual representation can be
    anything as long as the function in this slot mimics the semantics
    of [`MAP-INTO`][ce52] that's the default.

<a id="x-28MGL-GPR-3ACREATE-INDIVIDUAL-FN-20-28MGL-PAX-3AREADER-20MGL-GPR-3ADIFFERENTIAL-EVOLUTION-29-29"></a>
- [reader] **CREATE-INDIVIDUAL-FN** *DIFFERENTIAL-EVOLUTION (:CREATE-INDIVIDUAL-FN)*

    Holds a function of one argument, the DE, that
    returns a new individual that needs not be initialized in any way.
    Typically this just calls [`MAKE-ARRAY`][0b2b].

<a id="x-28MGL-GPR-3AMUTATE-FN-20-28MGL-PAX-3AREADER-20MGL-GPR-3ADIFFERENTIAL-EVOLUTION-29-29"></a>
- [reader] **MUTATE-FN** *DIFFERENTIAL-EVOLUTION (:MUTATE-FN)*

    One of the supplied mutation functions:
    [`MUTATE/RAND/1`][720f] [`MUTATE/BEST/1`][2fdd] [`MUTATE/CURRENT-TO-BEST/2`][f19b].

<a id="x-28MGL-GPR-3ACROSSOVER-FN-20-28MGL-PAX-3AREADER-20MGL-GPR-3ADIFFERENTIAL-EVOLUTION-29-29"></a>
- [reader] **CROSSOVER-FN** *DIFFERENTIAL-EVOLUTION (:CROSSOVER-FN = #'CROSSOVER/BINARY)*

    A function of three arguments, the DE and two
    individuals, that destructively modifies the second individual by
    using some parts of the first one. Currently, the implemented
    crossover function is [`CROSSOVER/BINARY`][e67a].

<a id="x-28MGL-GPR-3AMUTATE-2FRAND-2F1-20FUNCTION-29"></a>
- [function] **MUTATE/RAND/1** *DE CURRENT BEST POPULATION NURSERY &KEY (F 0.5)*

<a id="x-28MGL-GPR-3AMUTATE-2FBEST-2F1-20FUNCTION-29"></a>
- [function] **MUTATE/BEST/1** *DE CURRENT BEST POPULATION NURSERY &KEY (F 0.5)*

<a id="x-28MGL-GPR-3AMUTATE-2FCURRENT-TO-BEST-2F2-20FUNCTION-29"></a>
- [function] **MUTATE/CURRENT-TO-BEST/2** *DE CURRENT BEST POPULATION NURSERY &KEY (F 0.5)*

<a id="x-28MGL-GPR-3ACROSSOVER-2FBINARY-20FUNCTION-29"></a>
- [function] **CROSSOVER/BINARY** *DE INDIVIDUAL-1 INDIVIDUAL-2 &KEY (CROSSOVER-RATE 0.5)*

    Destructively modify `INDIVIDUAL-2` by replacement each element with
    a probability of 1 - `CROSSOVER-RATE` with the corresponding element
    in `INDIVIDUAL-1`. At least one, element is changed. Return
    `INDIVIDUAL-2`.

<a id="x-28MGL-GPR-3ASELECT-DISTINCT-RANDOM-NUMBERS-20FUNCTION-29"></a>
- [function] **SELECT-DISTINCT-RANDOM-NUMBERS** *TABOOS N LIMIT*

<a id="x-28MGL-GPR-3A-40GPR-DE-SANSDE-20MGL-PAX-3ASECTION-29"></a>
### 6.1 SANSDE

<a id="x-28MGL-GPR-3ASANSDE-20CLASS-29"></a>
- [class] **SANSDE** *[DIFFERENTIAL-EVOLUTION][a4e6]*

    SaNSDE is a special DE that dynamically adjust the
    crossover and mutation are performed. The only parameters are the
    generic EA ones: [`POPULATION-SIZE`][ce25], [`EVALUATOR`][9a7e], etc. One also has to
    specify [`MAP-WEIGHTS-INTO-FN`][7ca1] and [`CREATE-INDIVIDUAL-FN`][1c33].

  [049c]: #x-28MGL-GPR-3ASELECTOR-20-28MGL-PAX-3AREADER-20MGL-GPR-3AGENETIC-PROGRAMMING-29-29 "MGL-GPR:SELECTOR (MGL-PAX:READER MGL-GPR:GENETIC-PROGRAMMING)"
  [057a]: #x-28MGL-GPR-3A-40GPR-EA-20MGL-PAX-3ASECTION-29 "Evolutionary Algorithms"
  [075b]: #x-28MGL-GPR-3A-40GPR-GP-TUTORIAL-20MGL-PAX-3ASECTION-29 "Tutorial"
  [084d]: #x-28MGL-GPR-3A-40GPR-GP-ENVIRONMENT-20MGL-PAX-3ASECTION-29 "Environment"
  [0b2b]: http://www.lispworks.com/documentation/HyperSpec/Body/f_mk_ar.htm "MAKE-ARRAY FUNCTION"
  [0cb2]: #x-28MGL-GPR-3ABUILDER-20-28MGL-PAX-3AREADER-20MGL-GPR-3ALITERAL-29-29 "MGL-GPR:BUILDER (MGL-PAX:READER MGL-GPR:LITERAL)"
  [0d2f]: #x-28MGL-GPR-3A-40GPR-GP-20MGL-PAX-3ASECTION-29 "Genetic Programming"
  [12f5]: #x-28MGL-GPR-3ANAME-20-28MGL-PAX-3AREADER-20MGL-GPR-3AOPERATOR-29-29 "MGL-GPR:NAME (MGL-PAX:READER MGL-GPR:OPERATOR)"
  [1602]: #x-28MGL-GPR-3APOPULATION-20-28MGL-PAX-3AACCESSOR-20MGL-GPR-3AEVOLUTIONARY-ALGORITHM-29-29 "MGL-GPR:POPULATION (MGL-PAX:ACCESSOR MGL-GPR:EVOLUTIONARY-ALGORITHM)"
  [1c33]: #x-28MGL-GPR-3ACREATE-INDIVIDUAL-FN-20-28MGL-PAX-3AREADER-20MGL-GPR-3ADIFFERENTIAL-EVOLUTION-29-29 "MGL-GPR:CREATE-INDIVIDUAL-FN (MGL-PAX:READER MGL-GPR:DIFFERENTIAL-EVOLUTION)"
  [1fc4]: http://www.lispworks.com/documentation/HyperSpec/Body/t_float.htm "FLOAT TYPE"
  [2e8a]: #x-28MGL-GPR-3A-40GPR-BACKGROUND-20MGL-PAX-3ASECTION-29 "Background"
  [2f1d]: http://www.lispworks.com/documentation/HyperSpec/Body/01_.htm '"1" MGL-PAX:CLHS'
  [2fdd]: #x-28MGL-GPR-3AMUTATE-2FBEST-2F1-20FUNCTION-29 "MGL-GPR:MUTATE/BEST/1 FUNCTION"
  [32b7]: #x-28MGL-GPR-3ALITERAL-20CLASS-29 "MGL-GPR:LITERAL CLASS"
  [34da]: #x-28-22mgl-gpr-22-20ASDF-2FSYSTEM-3ASYSTEM-29 '"mgl-gpr" ASDF/SYSTEM:SYSTEM'
  [40a0]: #x-28MGL-GPR-3A-40GPR-DE-SANSDE-20MGL-PAX-3ASECTION-29 "SANSDE"
  [4643]: #x-28MGL-GPR-3AGENETIC-PROGRAMMING-20CLASS-29 "MGL-GPR:GENETIC-PROGRAMMING CLASS"
  [4a1e]: #x-28MGL-GPR-3AADD-INDIVIDUAL-20FUNCTION-29 "MGL-GPR:ADD-INDIVIDUAL FUNCTION"
  [4e03]: #x-28MGL-GPR-3ARESULT-TYPE-20-28MGL-PAX-3AREADER-20MGL-GPR-3AEXPRESSION-CLASS-29-29 "MGL-GPR:RESULT-TYPE (MGL-PAX:READER MGL-GPR:EXPRESSION-CLASS)"
  [5460]: #x-28MGL-GPR-3ATOPLEVEL-TYPE-20-28MGL-PAX-3AREADER-20MGL-GPR-3AGENETIC-PROGRAMMING-29-29 "MGL-GPR:TOPLEVEL-TYPE (MGL-PAX:READER MGL-GPR:GENETIC-PROGRAMMING)"
  [5b15]: #x-28MGL-GPR-3AOPERATOR-20MGL-PAX-3AMACRO-29 "MGL-GPR:OPERATOR MGL-PAX:MACRO"
  [5d57]: #x-28MGL-GPR-3A-40GPR-GP-REPRODUCTION-20MGL-PAX-3ASECTION-29 "Reproduction"
  [612d]: #x-28MGL-GPR-3ARANDOMIZER-20-28MGL-PAX-3AREADER-20MGL-GPR-3AGENETIC-PROGRAMMING-29-29 "MGL-GPR:RANDOMIZER (MGL-PAX:READER MGL-GPR:GENETIC-PROGRAMMING)"
  [6b4a]: http://www.lispworks.com/documentation/HyperSpec/Body/f_funcal.htm "FUNCALL FUNCTION"
  [6e4d]: #x-28MGL-GPR-3A-40GPR-GP-LINKS-20MGL-PAX-3ASECTION-29 "Links"
  [720f]: #x-28MGL-GPR-3AMUTATE-2FRAND-2F1-20FUNCTION-29 "MGL-GPR:MUTATE/RAND/1 FUNCTION"
  [7ca1]: #x-28MGL-GPR-3AMAP-WEIGHTS-INTO-FN-20-28MGL-PAX-3AREADER-20MGL-GPR-3ADIFFERENTIAL-EVOLUTION-29-29 "MGL-GPR:MAP-WEIGHTS-INTO-FN (MGL-PAX:READER MGL-GPR:DIFFERENTIAL-EVOLUTION)"
  [7df2]: #x-28MGL-GPR-3ARANDOM-EXPRESSION-20FUNCTION-29 "MGL-GPR:RANDOM-EXPRESSION FUNCTION"
  [8804]: http://www.lispworks.com/documentation/HyperSpec/Body/f_identi.htm "IDENTITY FUNCTION"
  [8a46]: #x-28MGL-GPR-3ALITERAL-20MGL-PAX-3AMACRO-29 "MGL-GPR:LITERAL MGL-PAX:MACRO"
  [8c75]: http://www.lispworks.com/documentation/HyperSpec/Body/f_cmp.htm "COMPILE FUNCTION"
  [9085]: #x-28MGL-GPR-3A-40GPR-GP-SEARCH-SPACE-20MGL-PAX-3ASECTION-29 "Search Space"
  [95f7]: #x-28MGL-GPR-3AEXPRESSION-CLASS-20CLASS-29 "MGL-GPR:EXPRESSION-CLASS CLASS"
  [9631]: #x-28MGL-GPR-3AFITTEST-20-28MGL-PAX-3AREADER-20MGL-GPR-3AEVOLUTIONARY-ALGORITHM-29-29 "MGL-GPR:FITTEST (MGL-PAX:READER MGL-GPR:EVOLUTIONARY-ALGORITHM)"
  [9a7e]: #x-28MGL-GPR-3AEVALUATOR-20-28MGL-PAX-3AREADER-20MGL-GPR-3AEVOLUTIONARY-ALGORITHM-29-29 "MGL-GPR:EVALUATOR (MGL-PAX:READER MGL-GPR:EVOLUTIONARY-ALGORITHM)"
  [9cba]: #x-28MGL-GPR-3A-40GPR-EA-POPULATION-20MGL-PAX-3ASECTION-29 "Populations"
  [a2f2]: http://www.lispworks.com/documentation/HyperSpec/Body/t_real.htm "REAL TYPE"
  [a4e6]: #x-28MGL-GPR-3ADIFFERENTIAL-EVOLUTION-20CLASS-29 "MGL-GPR:DIFFERENTIAL-EVOLUTION CLASS"
  [a92c]: #x-28MGL-GPR-3AARGUMENT-TYPES-20-28MGL-PAX-3AREADER-20MGL-GPR-3AOPERATOR-29-29 "MGL-GPR:ARGUMENT-TYPES (MGL-PAX:READER MGL-GPR:OPERATOR)"
  [b60a]: #x-28MGL-GPR-3A-40GPR-EA-EVALUATION-20MGL-PAX-3ASECTION-29 "Evaluation"
  [b75b]: #x-28MGL-GPR-3AOPERATOR-20CLASS-29 "MGL-GPR:OPERATOR CLASS"
  [bbfb]: http://www.lispworks.com/documentation/HyperSpec/Body/f_float.htm "FLOAT FUNCTION"
  [be13]: #x-28MGL-GPR-3AADVANCE-20GENERIC-FUNCTION-29 "MGL-GPR:ADVANCE GENERIC-FUNCTION"
  [be2e]: #x-28MGL-GPR-3AWEIGHT-20-28MGL-PAX-3AREADER-20MGL-GPR-3AEXPRESSION-CLASS-29-29 "MGL-GPR:WEIGHT (MGL-PAX:READER MGL-GPR:EXPRESSION-CLASS)"
  [c1eb]: http://www.lispworks.com/documentation/HyperSpec/Body/f_eval.htm "EVAL FUNCTION"
  [c392]: #x-28MGL-GPR-3A-40GPR-GP-BACKGROUND-20MGL-PAX-3ASECTION-29 "Background"
  [ce25]: #x-28MGL-GPR-3APOPULATION-SIZE-20-28MGL-PAX-3AACCESSOR-20MGL-GPR-3AEVOLUTIONARY-ALGORITHM-29-29 "MGL-GPR:POPULATION-SIZE (MGL-PAX:ACCESSOR MGL-GPR:EVOLUTIONARY-ALGORITHM)"
  [ce52]: http://www.lispworks.com/documentation/HyperSpec/Body/f_map_in.htm "MAP-INTO FUNCTION"
  [d237]: #x-28MGL-GPR-3ACOUNT-NODES-20FUNCTION-29 "MGL-GPR:COUNT-NODES FUNCTION"
  [d40e]: #x-28MGL-GPR-3AHOLD-TOURNAMENT-20FUNCTION-29 "MGL-GPR:HOLD-TOURNAMENT FUNCTION"
  [db20]: #x-28MGL-GPR-3A-40GPR-DE-20MGL-PAX-3ASECTION-29 "Differential Evolution"
  [e084]: #x-28MGL-GPR-3A-40GPR-GP-BASICS-20MGL-PAX-3ASECTION-29 "Basics"
  [e67a]: #x-28MGL-GPR-3ACROSSOVER-2FBINARY-20FUNCTION-29 "MGL-GPR:CROSSOVER/BINARY FUNCTION"
  [e87f]: #x-28MGL-GPR-3A-40GPR-EA-TRAINING-20MGL-PAX-3ASECTION-29 "Training"
  [ef7a]: #x-28MGL-GPR-3AMASS-EVALUATOR-20-28MGL-PAX-3AREADER-20MGL-GPR-3AEVOLUTIONARY-ALGORITHM-29-29 "MGL-GPR:MASS-EVALUATOR (MGL-PAX:READER MGL-GPR:EVOLUTIONARY-ALGORITHM)"
  [f19b]: #x-28MGL-GPR-3AMUTATE-2FCURRENT-TO-BEST-2F2-20FUNCTION-29 "MGL-GPR:MUTATE/CURRENT-TO-BEST/2 FUNCTION"
  [f5ee]: #x-28MGL-GPR-3AEVOLUTIONARY-ALGORITHM-20CLASS-29 "MGL-GPR:EVOLUTIONARY-ALGORITHM CLASS"
  [f9a4]: #x-28MGL-GPR-3A-40GPR-GP-EXPRESSIONS-20MGL-PAX-3ASECTION-29 "Expressions"

* * *
###### \[generated by [MGL-PAX](https://github.com/melisgl/mgl-pax)\]
