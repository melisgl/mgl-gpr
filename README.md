<a name='x-28-40GPR-MANUAL-20MGL-PAX-3ASECTION-29'></a>

# GPR Manual

## Table of Contents

- [1 mgl-gpr ASDF System Details][7710]
- [2 Background][f9c7]
- [3 Tutorial][7d46]
- [4 Expressions][ffaa]
- [5 Basics][8eff]
- [6 Search Space][92a7]
- [7 Reproduction][2ef4]
- [8 Environment][6e8c]
- [9 Individuals][afbd]

###### \[in package MGL-GPR\]
<a name='x-28-22mgl-gpr-22-20ASDF-2FSYSTEM-3ASYSTEM-29'></a>

## 1 mgl-gpr ASDF System Details

- Version: 0.0.1
- Description: MGL-GPR is a library for genetic programming: evolving
  typed expressions for a particular purpose from a set of operators
  and constants.
- Licence: MIT, see COPYING.
- Author: Gábor Melis
- Mailto: [mega@retes.hu](mailto:mega@retes.hu)
- Homepage: [http://quotenil.com](http://quotenil.com)

<a name='x-28-40GPR-BACKGROUND-20MGL-PAX-3ASECTION-29'></a>

## 2 Background

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

GP is quick to get up and running, can produce good results across
a wild variety of domains, but it needs quite a bit of fiddling to
perform well and domain specific approaches will almost always have
better results. All in all, GP can be very useful to cut down on
the tedium of human trial and error.

I originally wrote this library while working for Ravenpack who
agreed to release it under an MIT licence. Several years later I
cleaned it up, and documented it. Enjoy.

<a name='x-28-40GPR-TUTORIAL-20MGL-PAX-3ASECTION-29'></a>

## 3 Tutorial

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
random expressions with [`RANDOM-EXPRESSION`][a611], but we also need to
define how good a certain expression is which is called *fitness*.

In this example, we are going to perform symbolic regression, that
is, try to find an expression that approximates some target
expression well:

    (defparameter *target-expr* '(+ 7 (sin (expt (* *x* 2 pi) 2))))

Think of `*TARGET-EXPR*` as a function of `*X*`. The evaluator
function will bind the special `*X*` to the input and simply `EVAL`
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

That's about it. Now we create a [`GP`][b39f] instance hooking everything up,
set up the initial population and just call [`ADVANCE`][c844] a couple of
times to create new generations of expressions.

    (defun run ()
      (let ((*print-length* nil)
            (*print-level* nil)
            (gp (make-instance
                 'gp
                 :toplevel-type 'number
                 :operators *operators*
                 :literals *literals*
                 :population-size 10000
                 :copy-chance 0.0
                 :mutation-chance 0.2
                 :evaluator (lambda (gp expr)
                              (evaluate gp expr *target-expr*))
                 :randomizer #'randomize
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
        (loop repeat 100 do
          (when (zerop (mod (generation-counter gp) 20))
            (format t "Generation ~S~%" (generation-counter gp)))
          (advance gp))
        (destructuring-bind (fittest . fitness) (fittest gp)
          (format t "Best fitness: ~S for~%  ~S~%" fitness fittest))))


<a name='x-28-40GPR-EXPRESSIONS-20MGL-PAX-3ASECTION-29'></a>

## 4 Expressions

Genetic programming works with a population of individuals. The
individuals are sexps that may be evaluated directly by `EVAL` or by
other means. The internal nodes and the leafs of the sexp as a tree
represent the application of operators and literal objects,
respectively. Note that currently there is no way to represent
literal lists.

<a name='x-28EXPRESSION-CLASS-20CLASS-29'></a>

- [class] **EXPRESSION-CLASS**

    An object of [`EXPRESSION-CLASS`][c737] defines two things:
    how to build a random expression that belongs to that expression
    class and what lisp type those expressions evaluate to.

<a name='x-28RESULT-TYPE-20-28MGL-PAX-3AREADER-20EXPRESSION-CLASS-29-29'></a>

- [reader] **RESULT-TYPE** *EXPRESSION-CLASS*

    Expressions belonging to this expression class
    must evaluate to a value of this lisp type.

<a name='x-28OPERATOR-20CLASS-29'></a>

- [class] **OPERATOR** *EXPRESSION-CLASS*

    Defines how the symbol [`NAME`][00c8] in the function
    position of a list can be combined arguments: how many and of what
    types. The following defines `+` as an operator that adds two
    `FLOAT`s:
    
        (make-instance 'operator 
                       :name '+
                       :result-type float
                       :argument-types '(float float))
    
    See the macro [`OPERATOR`][b65a] for a shorthand for the above.
    
    Currently no lambda list keywords are supported and there is no way
    to define how an expression with a particular operator is to be
    built. See [`RANDOM-EXPRESSION`][a611].

<a name='x-28NAME-20-28MGL-PAX-3AREADER-20OPERATOR-29-29'></a>

- [reader] **NAME** *OPERATOR*

    A symbol that's the name of the operator.

<a name='x-28ARGUMENT-TYPES-20-28MGL-PAX-3AREADER-20OPERATOR-29-29'></a>

- [reader] **ARGUMENT-TYPES** *OPERATOR*

    A list of lisp types. One for each argument of
    this operator.

<a name='x-28OPERATOR-20MGL-PAX-3AMACRO-29'></a>

- [macro] **OPERATOR** *(NAME &REST ARG-TYPES) RESULT-TYPE*

    Syntactic sugar for instantiating operators. The example given for
    [`OPERATOR`][bc9a] could be written as:
    
        (operator (+ float float) float)


<a name='x-28LITERAL-20CLASS-29'></a>

- [class] **LITERAL** *EXPRESSION-CLASS*

    This is slightly misnamed. An object belonging to
    the [`LITERAL`][514a] class is not a literal itself, it's a factory for
    literals via its [`BUILDER`][c8e5] function. For example, the following
    literal builds bytes:
    
        (make-instance 'literal
                       :result-type '(unsigned-byte 8)
                       :builder (lambda () (random 256)))
    
    In practice, one rarely writes it out like that, because the [`LITERAL`][7ec4]
    macro provides a more convenient shorthand.

<a name='x-28BUILDER-20-28MGL-PAX-3AREADER-20LITERAL-29-29'></a>

- [reader] **BUILDER** *LITERAL*

    A function of no arguments that returns a random
    literal that belongs to its literal class.

<a name='x-28LITERAL-20MGL-PAX-3AMACRO-29'></a>

- [macro] **LITERAL** *(RESULT-TYPE) &BODY BODY*

    Syntactic sugar for defining literal classes. The example given for
    [`LITERAL`][514a] could be written as:
    
        (literal ((unsigned-byte 8))
          (random 256))


<a name='x-28RANDOM-EXPRESSION-20FUNCTION-29'></a>

- [function] **RANDOM-EXPRESSION** *OPERATORS LITERALS TYPE TERMINATE-FN*

    Return an expression built from `OPERATORS` and `LITERALS` that
    evaluates to values of `TYPE`. `TERMINATE-FN` is a function of one
    argument: the level of the root of the subexpression to be generated
    in the context of the entire expression. If it returns `T` then a
    [`LITERAL`][514a] will be inserted (by calling its [`BUILDER`][c8e5] function),
    else an [`OPERATOR`][bc9a] with all its necessary arguments.
    
    The algorithm recursively generates the expression starting from
    level 0 where only operators and literals with a [`RESULT-TYPE`][e4b0] that's
    a subtype of `TYPE` are considered. On lower levels, the
    [`ARGUMENT-TYPES`][4fee] specification of operators is similarly satisfied and
    the resulting expression should evaluate without without a type
    error.
    
    The building of expressions cannot backtrack. If it finds itself in
    a situation where no literals or operators of the right type are
    available then it will fail with an error.

<a name='x-28-40GPR-BASICS-20MGL-PAX-3ASECTION-29'></a>

## 5 Basics

To start the evolutionary process one creates a [`GP`][b39f] object,
adds to it the individuals that make up the initial population and
calls [`ADVANCE`][c844] in a loop to move on to the next generation.

<a name='x-28GP-20CLASS-29'></a>

- [class] **GP**

    The [`GP`][b39f] class defines the search space, how mutation
    and recombination occur, and hold various parameters of the
    evolutionary process and the individuals themselves.

<a name='x-28ADD-INDIVIDUAL-20FUNCTION-29'></a>

- [function] **ADD-INDIVIDUAL** *GP INDIVIDUAL*

    Adds `INDIVIDUAL` to [`POPULATION`][8ed7] of `GP`. Usually called to initialize
    the `GP`, but it is also allowed to add individuals (or change
    [`POPULATION`][8ed7] in any way) in between calls to [`ADVANCE`][c844].

<a name='x-28RANDOM-GP-EXPRESSION-20FUNCTION-29'></a>

- [function] **RANDOM-GP-EXPRESSION** *GP TERMINATE-FN &KEY (TYPE (TOPLEVEL-TYPE GP))*

    Creating the initial population by hand is tedious. This
    convenience function calls [`RANDOM-EXPRESSION`][a611] to create a random
    individual that produces `GP`'s [`TOPLEVEL-TYPE`][5782]. By passing in another
    `TYPE` one can create expressions that fit somewhere else in a larger
    expression which is useful in a [`RANDOMIZER`][9a98] function.

<a name='x-28ADVANCE-20FUNCTION-29'></a>

- [function] **ADVANCE** *GP*

    Create the next generation and place it in [`POPULATION`][8ed7].

<a name='x-28-40GPR-SEARCH-SPACE-20MGL-PAX-3ASECTION-29'></a>

## 6 Search Space

The search space of the [`GP`][b39f] is defined by the available operators,
literals and the type of the final result produced. The evaluator
function acts as the guiding light.

<a name='x-28OPERATORS-20-28MGL-PAX-3AREADER-20GP-29-29'></a>

- [reader] **OPERATORS** *GP*

    The set of [`OPERATOR`][bc9a]s from which (together
    with [`LITERAL`][514a]s) individuals are built.

<a name='x-28LITERALS-20-28MGL-PAX-3AREADER-20GP-29-29'></a>

- [reader] **LITERALS** *GP*

    The set of [`LITERAL`][514a]s from which (together
    with [`OPERATOR`][bc9a]s) individuals are built.

<a name='x-28TOPLEVEL-TYPE-20-28MGL-PAX-3AREADER-20GP-29-29'></a>

- [reader] **TOPLEVEL-TYPE** *GP*

    The type of the results produced by individuals.
    If the problem is to find the minimum a 1d real function then this
    may be the symbol `REAL`. If the problem is to find the shortest
    route, then this may be a vector. It all depends on the
    representation of the problem, the operators and the literals.

<a name='x-28EVALUATOR-20-28MGL-PAX-3AREADER-20GP-29-29'></a>

- [reader] **EVALUATOR** *GP*

    A function of two arguments: the [`GP`][b39f] object and the
    individual. It must return the fitness of the individual. Often,
    the evaluator just calls `EVAL`, or `COMPILE` + `FUNCALL`, and compares
    the result to some gold standard. It is also typical to slightly
    penalize solution with too many nodes to control complexity and
    evaluation cost (see [`COUNT-NODES`][7598]).

<a name='x-28COUNT-NODES-20FUNCTION-29'></a>

- [function] **COUNT-NODES** *TREE &KEY INTERNAL*

    Count the nodes in the sexp `TREE`. If `INTERNAL` then don't count the
    leaves.

<a name='x-28-40GPR-REPRODUCTION-20MGL-PAX-3ASECTION-29'></a>

## 7 Reproduction

The [`RANDOMIZER`][9a98] and [`SELECTOR`][bb5c] functions define how mutation and
recombination occur.

<a name='x-28RANDOMIZER-20-28MGL-PAX-3AREADER-20GP-29-29'></a>

- [reader] **RANDOMIZER** *GP*

    Used for mutations, this is a function of three
    arguments: the [`GP`][b39f] object, the type the expression must produce and
    current expression to be replaced with the returned value. It is
    called with subexpressions of individuals.

<a name='x-28SELECTOR-20-28MGL-PAX-3AREADER-20GP-29-29'></a>

- [reader] **SELECTOR** *GP*

    A function of two arguments: the [`GP`][b39f] object and a
    vector of fitnesses. It must return the and index into the fitness
    vector. The individual whose fitness was thus selected will be
    selected for reproduction be it mutation or crossover. Typically,
    this defers to [`HOLD-TOURNAMENT`][3ef2].

<a name='x-28HOLD-TOURNAMENT-20FUNCTION-29'></a>

- [function] **HOLD-TOURNAMENT** *FITNESSES &KEY SELECT-CONTESTANT-FN N-CONTESTANTS*

    Select `N-CONTESTANTS` (all different) for the tournament randomly,
    represented by indices into `FITNESSES` and return the one with the
    highest fitness. If `SELECT-CONTESTANT-FN` is `NIL` then contestants are
    selected randomly with uniform probability. If `SELECT-CONTESTANT-FN`
    is a function, then it's called with `FITNESSES` to return an
    index (that may or may not be already selected for the tournament).
    Specifying `SELECT-CONTESTANT-FN` allows one to conduct 'local'
    tournaments biased towards a particular region of the index range.

<a name='x-28-40GPR-ENVIRONMENT-20MGL-PAX-3ASECTION-29'></a>

## 8 Environment

The following are just various knobs to control the environment in
which individuals live.

<a name='x-28GENERATION-COUNTER-20-28MGL-PAX-3AREADER-20GP-29-29'></a>

- [reader] **GENERATION-COUNTER** *GP*

    A counter that starts from 0 and is incremented by
    [`ADVANCE`][c844]. All accessors of [`GP`][b39f] are allowed to be specialized on a
    subclass of [`GP`][b39f] which allows them to be functions of
    [`GENERATION-COUNTER`][d31e].

<a name='x-28POPULATION-SIZE-20-28MGL-PAX-3AACCESSOR-20GP-29-29'></a>

- [accessor] **POPULATION-SIZE** *GP*

    The number of individuals in a generation.

<a name='x-28COPY-CHANCE-20-28MGL-PAX-3AACCESSOR-20GP-29-29'></a>

- [accessor] **COPY-CHANCE** *GP*

    The probability of an individual selected (by
    [`SELECTOR`][bb5c]) for reproduction to produce an offspring by
    copying (subject to mutation). If it is not copied then it is
    going to be crossed over with another selected individual.

<a name='x-28MUTATION-CHANCE-20-28MGL-PAX-3AACCESSOR-20GP-29-29'></a>

- [accessor] **MUTATION-CHANCE** *GP*

    All new individuals regardless of whether they
    were created by copying or by crossover experience random mutation
    with this chance.

<a name='x-28KEEP-FITTEST-P-20-28MGL-PAX-3AACCESSOR-20GP-29-29'></a>

- [accessor] **KEEP-FITTEST-P** *GP*

    If true, then the fittest individual is always
    copied without mutation to the next generation. Of course, it may
    also have other offsprings.

<a name='x-28-40GPR-INDIVIDUALS-20MGL-PAX-3ASECTION-29'></a>

## 9 Individuals

<a name='x-28POPULATION-20-28MGL-PAX-3AACCESSOR-20GP-29-29'></a>

- [accessor] **POPULATION** *GP*

    An adjustable array with a fill-pointer that holds
    the individuals that make up the population.

<a name='x-28FITTEST-20-28MGL-PAX-3AREADER-20GP-29-29'></a>

- [reader] **FITTEST** *GP*

    The fittest individual ever to be seen by this [`GP`][b39f]
    and its fittness as a cons cell.

<a name='x-28FITTEST-CHANGED-FN-20-28MGL-PAX-3AACCESSOR-20GP-29-29'></a>

- [accessor] **FITTEST-CHANGED-FN** *GP*

    If non-NIL, a function that's called when [`FITTEST`][ab0a]
    is updated with three arguments: the [`GP`][b39f] object, the fittest
    individual and its fitness. Useful for tracking progress.

  [00c8]: #x-28NAME-20-28MGL-PAX-3AREADER-20OPERATOR-29-29 "(NAME (MGL-PAX:READER OPERATOR))"
  [2ef4]: #x-28-40GPR-REPRODUCTION-20MGL-PAX-3ASECTION-29 "(@GPR-REPRODUCTION MGL-PAX:SECTION)"
  [3ef2]: #x-28HOLD-TOURNAMENT-20FUNCTION-29 "(HOLD-TOURNAMENT FUNCTION)"
  [4fee]: #x-28ARGUMENT-TYPES-20-28MGL-PAX-3AREADER-20OPERATOR-29-29 "(ARGUMENT-TYPES (MGL-PAX:READER OPERATOR))"
  [514a]: #x-28LITERAL-20CLASS-29 "(LITERAL CLASS)"
  [5782]: #x-28TOPLEVEL-TYPE-20-28MGL-PAX-3AREADER-20GP-29-29 "(TOPLEVEL-TYPE (MGL-PAX:READER GP))"
  [6e8c]: #x-28-40GPR-ENVIRONMENT-20MGL-PAX-3ASECTION-29 "(@GPR-ENVIRONMENT MGL-PAX:SECTION)"
  [7598]: #x-28COUNT-NODES-20FUNCTION-29 "(COUNT-NODES FUNCTION)"
  [7710]: #x-28-22mgl-gpr-22-20ASDF-2FSYSTEM-3ASYSTEM-29 "(\"mgl-gpr\" ASDF/SYSTEM:SYSTEM)"
  [7d46]: #x-28-40GPR-TUTORIAL-20MGL-PAX-3ASECTION-29 "(@GPR-TUTORIAL MGL-PAX:SECTION)"
  [7ec4]: #x-28LITERAL-20MGL-PAX-3AMACRO-29 "(LITERAL MGL-PAX:MACRO)"
  [8ed7]: #x-28POPULATION-20-28MGL-PAX-3AACCESSOR-20GP-29-29 "(POPULATION (MGL-PAX:ACCESSOR GP))"
  [8eff]: #x-28-40GPR-BASICS-20MGL-PAX-3ASECTION-29 "(@GPR-BASICS MGL-PAX:SECTION)"
  [92a7]: #x-28-40GPR-SEARCH-SPACE-20MGL-PAX-3ASECTION-29 "(@GPR-SEARCH-SPACE MGL-PAX:SECTION)"
  [9a98]: #x-28RANDOMIZER-20-28MGL-PAX-3AREADER-20GP-29-29 "(RANDOMIZER (MGL-PAX:READER GP))"
  [a611]: #x-28RANDOM-EXPRESSION-20FUNCTION-29 "(RANDOM-EXPRESSION FUNCTION)"
  [ab0a]: #x-28FITTEST-20-28MGL-PAX-3AREADER-20GP-29-29 "(FITTEST (MGL-PAX:READER GP))"
  [afbd]: #x-28-40GPR-INDIVIDUALS-20MGL-PAX-3ASECTION-29 "(@GPR-INDIVIDUALS MGL-PAX:SECTION)"
  [b39f]: #x-28GP-20CLASS-29 "(GP CLASS)"
  [b65a]: #x-28OPERATOR-20MGL-PAX-3AMACRO-29 "(OPERATOR MGL-PAX:MACRO)"
  [bb5c]: #x-28SELECTOR-20-28MGL-PAX-3AREADER-20GP-29-29 "(SELECTOR (MGL-PAX:READER GP))"
  [bc9a]: #x-28OPERATOR-20CLASS-29 "(OPERATOR CLASS)"
  [c737]: #x-28EXPRESSION-CLASS-20CLASS-29 "(EXPRESSION-CLASS CLASS)"
  [c844]: #x-28ADVANCE-20FUNCTION-29 "(ADVANCE FUNCTION)"
  [c8e5]: #x-28BUILDER-20-28MGL-PAX-3AREADER-20LITERAL-29-29 "(BUILDER (MGL-PAX:READER LITERAL))"
  [d31e]: #x-28GENERATION-COUNTER-20-28MGL-PAX-3AREADER-20GP-29-29 "(GENERATION-COUNTER (MGL-PAX:READER GP))"
  [e4b0]: #x-28RESULT-TYPE-20-28MGL-PAX-3AREADER-20EXPRESSION-CLASS-29-29 "(RESULT-TYPE (MGL-PAX:READER EXPRESSION-CLASS))"
  [f9c7]: #x-28-40GPR-BACKGROUND-20MGL-PAX-3ASECTION-29 "(@GPR-BACKGROUND MGL-PAX:SECTION)"
  [ffaa]: #x-28-40GPR-EXPRESSIONS-20MGL-PAX-3ASECTION-29 "(@GPR-EXPRESSIONS MGL-PAX:SECTION)"
