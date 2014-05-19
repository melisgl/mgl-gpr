<a name='x-28MGL-GPR-3A-40GPR-MANUAL-20MGL-PAX-3ASECTION-29'></a>

# GPR Manual

## Table of Contents

- [1 mgl-gpr ASDF System Details][7710]
- [2 Background][eb32]
- [3 Tutorial][7c52]
- [4 Expressions][e0ef]
- [5 Basics][61ed]
- [6 Search Space][8d6c]
- [7 Reproduction][6028]
- [8 Environment][0089]
- [9 Individuals][b6e1]

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

<a name='x-28MGL-GPR-3A-40GPR-BACKGROUND-20MGL-PAX-3ASECTION-29'></a>

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

<a name='x-28MGL-GPR-3A-40GPR-TUTORIAL-20MGL-PAX-3ASECTION-29'></a>

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
random expressions with [`RANDOM-EXPRESSION`][ea57], but we also need to
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

That's about it. Now we create a [`GP`][8f13] instance hooking everything up,
set up the initial population and just call [`ADVANCE`][ef36] a couple of
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


<a name='x-28MGL-GPR-3A-40GPR-EXPRESSIONS-20MGL-PAX-3ASECTION-29'></a>

## 4 Expressions

Genetic programming works with a population of individuals. The
individuals are sexps that may be evaluated directly by `EVAL` or by
other means. The internal nodes and the leafs of the sexp as a tree
represent the application of operators and literal objects,
respectively. Note that currently there is no way to represent
literal lists.

<a name='x-28MGL-GPR-3AEXPRESSION-CLASS-20CLASS-29'></a>

- [class] **EXPRESSION-CLASS**

    An object of [`EXPRESSION-CLASS`][2af3] defines two things:
    how to build a random expression that belongs to that expression
    class and what lisp type those expressions evaluate to.

<a name='x-28MGL-GPR-3ARESULT-TYPE-20-28MGL-PAX-3AREADER-20MGL-GPR-3AEXPRESSION-CLASS-29-29'></a>

- [reader] **RESULT-TYPE** *EXPRESSION-CLASS*

    Expressions belonging to this expression class
    must evaluate to a value of this lisp type.

<a name='x-28MGL-GPR-3AOPERATOR-20CLASS-29'></a>

- [class] **OPERATOR** *EXPRESSION-CLASS*

    Defines how the symbol [`NAME`][3071] in the function
    position of a list can be combined arguments: how many and of what
    types. The following defines `+` as an operator that adds two
    `FLOAT`s:
    
        (make-instance 'operator 
                       :name '+
                       :result-type float
                       :argument-types '(float float))
    
    See the macro [`OPERATOR`][0982] for a shorthand for the above.
    
    Currently no lambda list keywords are supported and there is no way
    to define how an expression with a particular operator is to be
    built. See [`RANDOM-EXPRESSION`][ea57].

<a name='x-28MGL-GPR-3ANAME-20-28MGL-PAX-3AREADER-20MGL-GPR-3AOPERATOR-29-29'></a>

- [reader] **NAME** *OPERATOR*

    A symbol that's the name of the operator.

<a name='x-28MGL-GPR-3AARGUMENT-TYPES-20-28MGL-PAX-3AREADER-20MGL-GPR-3AOPERATOR-29-29'></a>

- [reader] **ARGUMENT-TYPES** *OPERATOR*

    A list of lisp types. One for each argument of
    this operator.

<a name='x-28MGL-GPR-3AOPERATOR-20MGL-PAX-3AMACRO-29'></a>

- [macro] **OPERATOR** *(NAME &REST ARG-TYPES) RESULT-TYPE*

    Syntactic sugar for instantiating operators. The example given for
    [`OPERATOR`][ea46] could be written as:
    
        (operator (+ float float) float)


<a name='x-28MGL-GPR-3ALITERAL-20CLASS-29'></a>

- [class] **LITERAL** *EXPRESSION-CLASS*

    This is slightly misnamed. An object belonging to
    the [`LITERAL`][5af0] class is not a literal itself, it's a factory for
    literals via its [`BUILDER`][8bb2] function. For example, the following
    literal builds bytes:
    
        (make-instance 'literal
                       :result-type '(unsigned-byte 8)
                       :builder (lambda () (random 256)))
    
    In practice, one rarely writes it out like that, because the [`LITERAL`][d1ea]
    macro provides a more convenient shorthand.

<a name='x-28MGL-GPR-3ABUILDER-20-28MGL-PAX-3AREADER-20MGL-GPR-3ALITERAL-29-29'></a>

- [reader] **BUILDER** *LITERAL*

    A function of no arguments that returns a random
    literal that belongs to its literal class.

<a name='x-28MGL-GPR-3ALITERAL-20MGL-PAX-3AMACRO-29'></a>

- [macro] **LITERAL** *(RESULT-TYPE) &BODY BODY*

    Syntactic sugar for defining literal classes. The example given for
    [`LITERAL`][5af0] could be written as:
    
        (literal ((unsigned-byte 8))
          (random 256))


<a name='x-28MGL-GPR-3ARANDOM-EXPRESSION-20FUNCTION-29'></a>

- [function] **RANDOM-EXPRESSION** *OPERATORS LITERALS TYPE TERMINATE-FN*

    Return an expression built from `OPERATORS` and `LITERALS` that
    evaluates to values of `TYPE`. `TERMINATE-FN` is a function of one
    argument: the level of the root of the subexpression to be generated
    in the context of the entire expression. If it returns `T` then a
    [`LITERAL`][5af0] will be inserted (by calling its [`BUILDER`][8bb2] function),
    else an [`OPERATOR`][ea46] with all its necessary arguments.
    
    The algorithm recursively generates the expression starting from
    level 0 where only operators and literals with a [`RESULT-TYPE`][27ef] that's
    a subtype of `TYPE` are considered. On lower levels, the
    [`ARGUMENT-TYPES`][cb67] specification of operators is similarly satisfied and
    the resulting expression should evaluate without without a type
    error.
    
    The building of expressions cannot backtrack. If it finds itself in
    a situation where no literals or operators of the right type are
    available then it will fail with an error.

<a name='x-28MGL-GPR-3A-40GPR-BASICS-20MGL-PAX-3ASECTION-29'></a>

## 5 Basics

To start the evolutionary process one creates a [`GP`][8f13] object,
adds to it the individuals that make up the initial population and
calls [`ADVANCE`][ef36] in a loop to move on to the next generation.

<a name='x-28MGL-GPR-3AGP-20CLASS-29'></a>

- [class] **GP**

    The [`GP`][8f13] class defines the search space, how mutation
    and recombination occur, and hold various parameters of the
    evolutionary process and the individuals themselves.

<a name='x-28MGL-GPR-3AADD-INDIVIDUAL-20FUNCTION-29'></a>

- [function] **ADD-INDIVIDUAL** *GP INDIVIDUAL*

    Adds `INDIVIDUAL` to [`POPULATION`][debc] of `GP`. Usually called to initialize
    the `GP`, but it is also allowed to add individuals (or change
    [`POPULATION`][debc] in any way) in between calls to [`ADVANCE`][ef36].

<a name='x-28MGL-GPR-3ARANDOM-GP-EXPRESSION-20FUNCTION-29'></a>

- [function] **RANDOM-GP-EXPRESSION** *GP TERMINATE-FN &KEY (TYPE (TOPLEVEL-TYPE GP))*

    Creating the initial population by hand is tedious. This
    convenience function calls [`RANDOM-EXPRESSION`][ea57] to create a random
    individual that produces `GP`'s [`TOPLEVEL-TYPE`][e7da]. By passing in another
    `TYPE` one can create expressions that fit somewhere else in a larger
    expression which is useful in a [`RANDOMIZER`][d34a] function.

<a name='x-28MGL-GPR-3AADVANCE-20FUNCTION-29'></a>

- [function] **ADVANCE** *GP*

    Create the next generation and place it in [`POPULATION`][debc].

<a name='x-28MGL-GPR-3A-40GPR-SEARCH-SPACE-20MGL-PAX-3ASECTION-29'></a>

## 6 Search Space

The search space of the [`GP`][8f13] is defined by the available operators,
literals and the type of the final result produced. The evaluator
function acts as the guiding light.

<a name='x-28MGL-GPR-3AOPERATORS-20-28MGL-PAX-3AREADER-20MGL-GPR-3AGP-29-29'></a>

- [reader] **OPERATORS** *GP*

    The set of [`OPERATOR`][ea46]s from which (together
    with [`LITERAL`][5af0]s) individuals are built.

<a name='x-28MGL-GPR-3ALITERALS-20-28MGL-PAX-3AREADER-20MGL-GPR-3AGP-29-29'></a>

- [reader] **LITERALS** *GP*

    The set of [`LITERAL`][5af0]s from which (together
    with [`OPERATOR`][ea46]s) individuals are built.

<a name='x-28MGL-GPR-3ATOPLEVEL-TYPE-20-28MGL-PAX-3AREADER-20MGL-GPR-3AGP-29-29'></a>

- [reader] **TOPLEVEL-TYPE** *GP*

    The type of the results produced by individuals.
    If the problem is to find the minimum a 1d real function then this
    may be the symbol `REAL`. If the problem is to find the shortest
    route, then this may be a vector. It all depends on the
    representation of the problem, the operators and the literals.

<a name='x-28MGL-GPR-3AEVALUATOR-20-28MGL-PAX-3AREADER-20MGL-GPR-3AGP-29-29'></a>

- [reader] **EVALUATOR** *GP*

    A function of two arguments: the [`GP`][8f13] object and the
    individual. It must return the fitness of the individual. Often,
    the evaluator just calls `EVAL`, or `COMPILE` + `FUNCALL`, and compares
    the result to some gold standard. It is also typical to slightly
    penalize solution with too many nodes to control complexity and
    evaluation cost (see [`COUNT-NODES`][63d7]).

<a name='x-28MGL-GPR-3ACOUNT-NODES-20FUNCTION-29'></a>

- [function] **COUNT-NODES** *TREE &KEY INTERNAL*

    Count the nodes in the sexp `TREE`. If `INTERNAL` then don't count the
    leaves.

<a name='x-28MGL-GPR-3A-40GPR-REPRODUCTION-20MGL-PAX-3ASECTION-29'></a>

## 7 Reproduction

The [`RANDOMIZER`][d34a] and [`SELECTOR`][17d6] functions define how mutation and
recombination occur.

<a name='x-28MGL-GPR-3ARANDOMIZER-20-28MGL-PAX-3AREADER-20MGL-GPR-3AGP-29-29'></a>

- [reader] **RANDOMIZER** *GP*

    Used for mutations, this is a function of three
    arguments: the [`GP`][8f13] object, the type the expression must produce and
    current expression to be replaced with the returned value. It is
    called with subexpressions of individuals.

<a name='x-28MGL-GPR-3ASELECTOR-20-28MGL-PAX-3AREADER-20MGL-GPR-3AGP-29-29'></a>

- [reader] **SELECTOR** *GP*

    A function of two arguments: the [`GP`][8f13] object and a
    vector of fitnesses. It must return the and index into the fitness
    vector. The individual whose fitness was thus selected will be
    selected for reproduction be it mutation or crossover. Typically,
    this defers to [`HOLD-TOURNAMENT`][119c].

<a name='x-28MGL-GPR-3AHOLD-TOURNAMENT-20FUNCTION-29'></a>

- [function] **HOLD-TOURNAMENT** *FITNESSES &KEY SELECT-CONTESTANT-FN N-CONTESTANTS*

    Select `N-CONTESTANTS` (all different) for the tournament randomly,
    represented by indices into `FITNESSES` and return the one with the
    highest fitness. If `SELECT-CONTESTANT-FN` is `NIL` then contestants are
    selected randomly with uniform probability. If `SELECT-CONTESTANT-FN`
    is a function, then it's called with `FITNESSES` to return an
    index (that may or may not be already selected for the tournament).
    Specifying `SELECT-CONTESTANT-FN` allows one to conduct 'local'
    tournaments biased towards a particular region of the index range.

<a name='x-28MGL-GPR-3A-40GPR-ENVIRONMENT-20MGL-PAX-3ASECTION-29'></a>

## 8 Environment

The following are just various knobs to control the environment in
which individuals live.

<a name='x-28MGL-GPR-3AGENERATION-COUNTER-20-28MGL-PAX-3AREADER-20MGL-GPR-3AGP-29-29'></a>

- [reader] **GENERATION-COUNTER** *GP*

    A counter that starts from 0 and is incremented by
    [`ADVANCE`][ef36]. All accessors of [`GP`][8f13] are allowed to be specialized on a
    subclass of [`GP`][8f13] which allows them to be functions of
    [`GENERATION-COUNTER`][f631].

<a name='x-28MGL-GPR-3APOPULATION-SIZE-20-28MGL-PAX-3AACCESSOR-20MGL-GPR-3AGP-29-29'></a>

- [accessor] **POPULATION-SIZE** *GP*

    The number of individuals in a generation.

<a name='x-28MGL-GPR-3ACOPY-CHANCE-20-28MGL-PAX-3AACCESSOR-20MGL-GPR-3AGP-29-29'></a>

- [accessor] **COPY-CHANCE** *GP*

    The probability of an individual selected (by
    [`SELECTOR`][17d6]) for reproduction to produce an offspring by
    copying (subject to mutation). If it is not copied then it is
    going to be crossed over with another selected individual.

<a name='x-28MGL-GPR-3AMUTATION-CHANCE-20-28MGL-PAX-3AACCESSOR-20MGL-GPR-3AGP-29-29'></a>

- [accessor] **MUTATION-CHANCE** *GP*

    All new individuals regardless of whether they
    were created by copying or by crossover experience random mutation
    with this chance.

<a name='x-28MGL-GPR-3AKEEP-FITTEST-P-20-28MGL-PAX-3AACCESSOR-20MGL-GPR-3AGP-29-29'></a>

- [accessor] **KEEP-FITTEST-P** *GP*

    If true, then the fittest individual is always
    copied without mutation to the next generation. Of course, it may
    also have other offsprings.

<a name='x-28MGL-GPR-3A-40GPR-INDIVIDUALS-20MGL-PAX-3ASECTION-29'></a>

## 9 Individuals

<a name='x-28MGL-GPR-3APOPULATION-20-28MGL-PAX-3AACCESSOR-20MGL-GPR-3AGP-29-29'></a>

- [accessor] **POPULATION** *GP*

    An adjustable array with a fill-pointer that holds
    the individuals that make up the population.

<a name='x-28MGL-GPR-3AFITTEST-20-28MGL-PAX-3AREADER-20MGL-GPR-3AGP-29-29'></a>

- [reader] **FITTEST** *GP*

    The fittest individual ever to be seen by this [`GP`][8f13]
    and its fittness as a cons cell.

<a name='x-28MGL-GPR-3AFITTEST-CHANGED-FN-20-28MGL-PAX-3AACCESSOR-20MGL-GPR-3AGP-29-29'></a>

- [accessor] **FITTEST-CHANGED-FN** *GP*

    If non-NIL, a function that's called when [`FITTEST`][14f9]
    is updated with three arguments: the [`GP`][8f13] object, the fittest
    individual and its fitness. Useful for tracking progress.

  [0089]: #x-28MGL-GPR-3A-40GPR-ENVIRONMENT-20MGL-PAX-3ASECTION-29 "(MGL-GPR:@GPR-ENVIRONMENT MGL-PAX:SECTION)"
  [0982]: #x-28MGL-GPR-3AOPERATOR-20MGL-PAX-3AMACRO-29 "(MGL-GPR:OPERATOR MGL-PAX:MACRO)"
  [119c]: #x-28MGL-GPR-3AHOLD-TOURNAMENT-20FUNCTION-29 "(MGL-GPR:HOLD-TOURNAMENT FUNCTION)"
  [14f9]: #x-28MGL-GPR-3AFITTEST-20-28MGL-PAX-3AREADER-20MGL-GPR-3AGP-29-29 "(MGL-GPR:FITTEST (MGL-PAX:READER MGL-GPR:GP))"
  [17d6]: #x-28MGL-GPR-3ASELECTOR-20-28MGL-PAX-3AREADER-20MGL-GPR-3AGP-29-29 "(MGL-GPR:SELECTOR (MGL-PAX:READER MGL-GPR:GP))"
  [27ef]: #x-28MGL-GPR-3ARESULT-TYPE-20-28MGL-PAX-3AREADER-20MGL-GPR-3AEXPRESSION-CLASS-29-29 "(MGL-GPR:RESULT-TYPE (MGL-PAX:READER MGL-GPR:EXPRESSION-CLASS))"
  [2af3]: #x-28MGL-GPR-3AEXPRESSION-CLASS-20CLASS-29 "(MGL-GPR:EXPRESSION-CLASS CLASS)"
  [3071]: #x-28MGL-GPR-3ANAME-20-28MGL-PAX-3AREADER-20MGL-GPR-3AOPERATOR-29-29 "(MGL-GPR:NAME (MGL-PAX:READER MGL-GPR:OPERATOR))"
  [5af0]: #x-28MGL-GPR-3ALITERAL-20CLASS-29 "(MGL-GPR:LITERAL CLASS)"
  [6028]: #x-28MGL-GPR-3A-40GPR-REPRODUCTION-20MGL-PAX-3ASECTION-29 "(MGL-GPR:@GPR-REPRODUCTION MGL-PAX:SECTION)"
  [61ed]: #x-28MGL-GPR-3A-40GPR-BASICS-20MGL-PAX-3ASECTION-29 "(MGL-GPR:@GPR-BASICS MGL-PAX:SECTION)"
  [63d7]: #x-28MGL-GPR-3ACOUNT-NODES-20FUNCTION-29 "(MGL-GPR:COUNT-NODES FUNCTION)"
  [7710]: #x-28-22mgl-gpr-22-20ASDF-2FSYSTEM-3ASYSTEM-29 "(\"mgl-gpr\" ASDF/SYSTEM:SYSTEM)"
  [7c52]: #x-28MGL-GPR-3A-40GPR-TUTORIAL-20MGL-PAX-3ASECTION-29 "(MGL-GPR:@GPR-TUTORIAL MGL-PAX:SECTION)"
  [8bb2]: #x-28MGL-GPR-3ABUILDER-20-28MGL-PAX-3AREADER-20MGL-GPR-3ALITERAL-29-29 "(MGL-GPR:BUILDER (MGL-PAX:READER MGL-GPR:LITERAL))"
  [8d6c]: #x-28MGL-GPR-3A-40GPR-SEARCH-SPACE-20MGL-PAX-3ASECTION-29 "(MGL-GPR:@GPR-SEARCH-SPACE MGL-PAX:SECTION)"
  [8f13]: #x-28MGL-GPR-3AGP-20CLASS-29 "(MGL-GPR:GP CLASS)"
  [b6e1]: #x-28MGL-GPR-3A-40GPR-INDIVIDUALS-20MGL-PAX-3ASECTION-29 "(MGL-GPR:@GPR-INDIVIDUALS MGL-PAX:SECTION)"
  [cb67]: #x-28MGL-GPR-3AARGUMENT-TYPES-20-28MGL-PAX-3AREADER-20MGL-GPR-3AOPERATOR-29-29 "(MGL-GPR:ARGUMENT-TYPES (MGL-PAX:READER MGL-GPR:OPERATOR))"
  [d1ea]: #x-28MGL-GPR-3ALITERAL-20MGL-PAX-3AMACRO-29 "(MGL-GPR:LITERAL MGL-PAX:MACRO)"
  [d34a]: #x-28MGL-GPR-3ARANDOMIZER-20-28MGL-PAX-3AREADER-20MGL-GPR-3AGP-29-29 "(MGL-GPR:RANDOMIZER (MGL-PAX:READER MGL-GPR:GP))"
  [debc]: #x-28MGL-GPR-3APOPULATION-20-28MGL-PAX-3AACCESSOR-20MGL-GPR-3AGP-29-29 "(MGL-GPR:POPULATION (MGL-PAX:ACCESSOR MGL-GPR:GP))"
  [e0ef]: #x-28MGL-GPR-3A-40GPR-EXPRESSIONS-20MGL-PAX-3ASECTION-29 "(MGL-GPR:@GPR-EXPRESSIONS MGL-PAX:SECTION)"
  [e7da]: #x-28MGL-GPR-3ATOPLEVEL-TYPE-20-28MGL-PAX-3AREADER-20MGL-GPR-3AGP-29-29 "(MGL-GPR:TOPLEVEL-TYPE (MGL-PAX:READER MGL-GPR:GP))"
  [ea46]: #x-28MGL-GPR-3AOPERATOR-20CLASS-29 "(MGL-GPR:OPERATOR CLASS)"
  [ea57]: #x-28MGL-GPR-3ARANDOM-EXPRESSION-20FUNCTION-29 "(MGL-GPR:RANDOM-EXPRESSION FUNCTION)"
  [eb32]: #x-28MGL-GPR-3A-40GPR-BACKGROUND-20MGL-PAX-3ASECTION-29 "(MGL-GPR:@GPR-BACKGROUND MGL-PAX:SECTION)"
  [ef36]: #x-28MGL-GPR-3AADVANCE-20FUNCTION-29 "(MGL-GPR:ADVANCE FUNCTION)"
  [f631]: #x-28MGL-GPR-3AGENERATION-COUNTER-20-28MGL-PAX-3AREADER-20MGL-GPR-3AGP-29-29 "(MGL-GPR:GENERATION-COUNTER (MGL-PAX:READER MGL-GPR:GP))"

* * *
###### \[generated by [MGL-PAX](https://github.com/melisgl/mgl-pax)\]
