# 1.1 The Elements of Programming

Every powerful PL has three mechanisms for combining simple ideas to form more complex ideas:
- __primitive expressions__, which represent the simplest entities the language is concerned with
- __means of combination__, by which compound elements are built from simpler ones
- __means of abstraction__, by which compound elements can be named and manipulated as units

## Expressions

Expressions, formed by delimiting a list of expressions within parentheses in order to denote procedure application, are called _combinations_. The leftmost element in the list is called the _operator_, and the other elements are called _operands_. The value of a combination is obtained by applying the procedure specified by the operator to the _arguments_ that are the values of the operands.

The convention of placing the operator to the left of the operands is known as _prefix notation_, and it may be somewhat confusing at first because it departs significantly from the customary mathematical convention.

Complex programs are constructed by building, step by step, computational objects of increasing complexity. The interpreter makes this step-by-step program construction particularly convenient because name-object associations can be created incrementally in successive interactions. This feature encourages the incremental development and testing of programs and is largely responsible for the fact that a Lisp program usually consists of a large number of relatively simple procedures.

The possibility of associating values with symbols and later retrieving them means that the interpreter must maintain some sort of memory that keeps track of the name-object pairs. This memory is called the _environment_.

## Evaluating Combinations

To evaluate a combination, the interpreter follows the procedure:
1) Evaluate the subexpressions of the combination
2) Apply the procedure that is the value of the leftmost subexpression (the operator) to the arguments that are the values of the other subexpressions (the operands)

The first step dictates that in order to accomplish the evaluation process for a combination, we must first perform the evaluation process on each element of the combination. Thus, the evaluation rule is _recursive_ in nature; that is, it includes, as one of its steps, the need to invoke the rule itself.

Exceptions to the general evaluation rule are called _special forms_ (for ex., _define_). Each special form has its own evaluation rule. The various kinds of expressions (each with its associated evaluation rule) constitute the syntax of the PL.

The _substitution model_ for procedure application:
To apply a compound procedure to arguments, evaluate the body of the procedure with each formal parameter replaced by the corresponding argument.

Typical interpreters do not evaluate procedure applications by manipulating the text of a procedure to substitute values for the formal parameters. In practice, the _substitution_ is accomplished by using a local environment for the formal parameters. The substitution model is only the first of the sequence of increasingly elaborate models (presented in the book) - a way to get started thinking formally about the evaluation process.

Evaluation types:
- _normal-order evaluation_, fully expand and then reduce
- _applicative-order evaluation_, evaluate the arguments and then apply

Lisp uses applicative-order evaluation, partly because of the additional efficiency obtained from avoiding multiple evaluation of the same expressions, and, more significantly, because normal-order evaluation becomes much more complicated to deal with when we leave the realm of procedures that can be modelled by substitution.

## Procedures and Functions

Procedures are much like ordinary mathematical functions. They specify a value that is determined by one or more parameters. But there is an important difference between mathematical functions and computer procedures. Procedures must be effective.

The contrast between function and procedure is a reflection of the general distinction between describing properties of things and describing how to do things, or, as it is sometimes referred to, the distinction between declarative knowledge and imperative knowledge.

In mathematics we are usually concerned with declarative (what is) descriptions, whereas in computer science we are usually concerned with imperative (how to) descriptions).

Declarative and imperative descriptions are intimately related, as indeed are mathematics and computer science. For instance, to say that the answer produced by a program is _correct_ is to make a declarative statement about the program. There is a large amount of research aimed at establishing techniques for proving that programs are correct, and much of the technical difficulty of this subject has to do with negotiating the transition between imperative statements (from which programs are constructed) and declarative statements (which can be used to deduce things).

In a related vein, an important current area in PL design is the exploration of so-called very high-level languages, in which one actually programs in terms of declarative statements. The idea is to make interpreters sophisticated enough so that, given _what is_ knowledge specified by the programmer, they can generate _how to_ knowledge automatically. This cannot be done in general, but there are important areas where progress has been made.

## Procedures as Black-Box Abstractions

The importance of a decomposition strategy is not simply that one is dividing the program into parts. After all, we could take any large program and divide it into parts - the first ten lines, the next ten lines, the next ten lines, and so on. Rather, it is crucial that each procedure accomplishes an identifiable task that can be used as a module in defining other procedures.

For example, when we define the _good-enough?_ procedure in terms of _square_, we are able to regard the _square_ procedure as a _black box_. We are not at that moment concerned with _how_ the procedure computes its result, only with the fact that it computes the square. The details of how the _square_ is computed can be suppressed, to be considered at a later time. Indeed, as far as the _good-enough?_ procedure is concerned, _square_ is not quite a procedure but rather an abstraction of a procedure, a so-called _procedural abstraction_. At this level of abstraction, any procedure that computes the square is equally good.

A procedure definition should be able to suppress detail. The users of the procedure may not have written the procedure themselves, but may have obtained it from another programmer as a black box. A user should not need to know how the procedure is implemented in order to use it.

## Local names

One detail of a procedure's implementation that should not matter to the user of the procedure is the implementer's choice of names for the procedure's formal parameters. Thus, the following procedures should not be distinguishable:

```
(define (square x) (* x x))
(define (square y) (* y y))
```

A formal parameter of a procedure has a very special role in the procedure definition, in that it doesn't matter what name the formal parameter has. Such a name is called a _bound variable_, and we say that the procedure definition _binds_ its formal parameters. The meaning of a procedure definition is unchanged if a bound variable is consistently renamed throughout the definition. If a variable is not bound, we say that it is _free_.

The set of expressions for which a binding defines a name is called the _scope_ of that name. In a procedure definition, the bound variables declared as the formal parameters of the procedure have the body of the procedure as their scope.

## Resources

- [1.1 The Elements of Programming](https://mitpress.mit.edu/sites/default/files/sicp/full-text/book/book-Z-H-10.html)
