# 4.1 The Metacircular Evaluator

An evaluator that is written in the same language that it evaluates is said to be _metacircular_.

The metacircular evaluator is essentially a Scheme formulation of the environment model of evaluation described in section 3.2. The model has two basic parts:

1. To evaluate a combination (a compound expression other than a special form), evaluate the subexpressions and then apply the value of the operator subexpression to the values of the operand subexpressions.

2. To apply a compound procedure to a set of arguments, evaluate the body of the procedure in a new environment. To construct this environment, extend the environment part of the procedure object by a frame in which the formal parameters of the procedure are bound to the arguments to which the procedure is applied.

These two rules describe the essence of the evaluation process, a basic cycle in which expressions to be evaluated in environments are reduced to procedures to be applied to arguments, which in turn are reduced to new expressions to be evaluated in new environments, and so on, until we get down to symbols, whose values are looked up in the environment, and to primitive procedures, which are applied directly.

This evaluation cycle will be embodied by the interplay between the two critical procedures in the evaluator, `eval` and `apply`.

The implementation of the evaluator will depend upon procedures that define the _syntax_ of the expressions to be evaluated. Data abstraction is used to make the evaluator independent of the representation of the language.

## The Core of the Evaluator

The evaluation process can be described as the interplay between two procedures: `eval` and `apply`.

### Eval

`eval` takes as arguments an expression and an environment. It classifies the expression and directs its evaluation. `eval` is structured as a case analysis of the syntactic type of the expression to be evaluated.

Each type of expression has a predicate that tests for it and an abstract means for selecting its parts. This _abstract syntax_ makes it easy to see how we can change the syntax of the language by using the same evaluator, but with a different collection of syntax procedures.

- Primitive expressions
  - for self-evaluating expressions, such as numbers, `eval` returns the expression itself
  - `eval` must look up variables in the environment to find their values
- Special forms
  - for quoted expressions, eval returns the expression that was quoted
  - an assignment to (or a definition of) a variable must recursively call `eval` to compute the new value to be associated with the variable. The environment must be modified to change (or create) the binding of the variable
  - an `if` expression requires special processing of its parts, so as to evaluate the consequent if the predicate is true, and otherwise to evaluate the alternative
  - a `lambda` expression must be transformed into an applicable procedure by packaging together the parameters and body specified by the `lambda` expression with the environment of the evaluation
  - a `begin` expression requires evaluating its sequence of expressions in the order in which they appear
  - a case analysis (`cond`) is transformed into a nest of `if` expressions and then evaluated
  - etc.
- Combinations
  - for a procedure application, `eval` must recursively evaluate the operator part and the operands of the combination. The resulting procedure and arguments are passed to `apply`, which handles the actual procedure application

### Apply

`apply` takes two arguments, a procedure and a list of arguments to which the procedure should be applied.

`apply` classifies procedures into two kinds:
- it calls `apply-primitive-procedure` to apply primitives
- it applies compound procedures by sequentially evaluating the expressions that make up the body of the procedure

The environment for the evaluation of the body of a compound procedure is constructed by extending the base environment carried by the procedure to include a frame that binds the parameters of the procedure to the arguments to which the procedure is to be applied.

## Representing expressions

The syntax of the language being evaluated is determined solely by the procedures that classify and extract pieces of expressions.

### Derived expressions

Some special forms in our language can be defined in terms of expressions involving other special forms, rather than being implemented directly. One example is `cond`, which can be implemented as a nest of `if` expressions.

Practical Lisp systems provide a mechanism that allows a user to add new derived expressions and specify their implementation as syntactic transformations without modifying the evaluator. Such a user-defined transformation is called a _macro_.

Although it is easy to add an elementary mechanism for defining macros, the resulting language has subtle name-conflict problems.

## Evaluator Data Structures

In addition to defining the external syntax of expressions, the evaluator implementation must also define the data structures that the evaluator manipulates internally, as part of the execution of a program, such as the representation of:
- procedures
- environments
- true and false

### Operations on Environments

As explained in section `3.2`, an environment is a sequence of frames, where each frame is a table of bindings that associate variables with their corresponding values.

The following operations for manipulating environments are used:

- `(lookup-variable-value <var> <env>)`

  returns the value that is bound to the symbol `<var>` in the environment `<env>`, or signals an error if the variable is unbound

- `(extend-environment <variables> <values> <base-env>)`

  returns a new environment, consisting of a new frame in which the symbols in the list `<variables>` are bound to the corresponding elements in the list `<values>`, where the enclosing environment is the environment `<base-env>`

- `(define-variable! <var> <value> <env>)`

  adds to the first frame in the environment `<env>` a new binding that associates the variable `<var>` with the value `<value>`

- `(set-variable-value! <var> <value> <env>)`
  
  changes the binding of the variable `<var>` in the environment `<env>` so that the variable is now bound to the value `<value>`, or signals an error if the variable is unbound

## Running the Evaluator as a Program

The evaluator program reduces expressions ultimately to the application of primitive procedures. Therefore, all that we need to run the evaluator is to create a mechanism that calls on the underlying Lisp system to model the application of primitive procedures.

There must be a binding for each primitive procedure name, so that when `eval` evaluates the operator of an application of a primitive, it will find an object to pass to `apply`. We thus set up a global environment that associates unique objects with the names of the primitive procedures that can appear in the expressions we will be evaluating.

It does not matter how the primitive procedure objects are represented, so long as `apply` can identify and apply them.

Any procedure defined in the underlying Lisp can be used as a primitive for the metacircular evaluator. The name of a primitive installed in the evaluator need not be the same as the name of its implementation in the underlying Lisp.

## Data as Programs

The notion of "what can in principle be computed" (ignoring practicalities of time and memory required) is independent of the language or the computer, and instead reflects an underlying notion of _computability_.

The user's programs are the evaluator's data.

## Internal Definitions

Our environment model of evaluation and our metacircular evaluator execute definitions in sequence, extending the environment frame one definition at a time. However, name-by-name extension of the environment may not be the best way to define local variables.

Since the definitions of the internal procedures come first, no calls to these procedures will be evaluated until all of them have been defined. In fact, our sequential evaluation mechanism will give the same result as a mechanism that directly implements simultaneous definition for any procedure in which the internal definitions come first in a body and evaluation of the value expressions for the defined variables doesn't actually use any of the defined variables.

A procedure that doesn't obey these restrictions reveals the inequality between sequential definition and simultaneous definition.

## Separating Syntactic Analysis from Execution

If the syntactic analysis of expressions is interleaved with their execution, an evaluator becomes very inefficient. Thus if a program is executed many times, its syntax is analyzed many times.

The evaluator can be transformed to be significantly more efficient by arranging things so that syntactic analysis is performed only once.

We split `eval`, which takes an expression and an environment, into two parts:
1. The procedure `analyze` takes only the expression. It performs the syntactic analysis and returns a new procedure, the `execution procedure`, that encapsulates the work to be done in executing the analyzed expression.
2. The execution procedure takes an environment as its argument and completes the evaluation.

This saves work because `analyze` will be called only **once** on an expression, while the execution procedure may be called many times.

## Resources

- [SICP - The Metacircular Evaluator](https://mitpress.mit.edu/sites/default/files/sicp/full-text/book/book-Z-H-26.html)
