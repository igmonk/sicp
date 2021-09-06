# 2.1 Introduction to Data Abstraction

_Data abstraction_ is a methodology that enables us to isolate how a compound data object is used from the details of how it is constructed from more primitive data objects.

The basic idea of data abstraction is to structure the programs that are to use compound data objects so that they operate on 'abstract data'. That is, our programs should use data in such a way as to make no assumptions about the data that are not strictly necessary for performing the task at hand. At the same time, a 'concrete' data representation is defined independent of the programs that use the data.

The interface between these two parts of our system will be a set of procedures, called _selectors_ and _constructors_, that implement the abstract data in terms of the concrete representation.

## Pairs

To enable us to implement the concrete level of our data abstraction, our language provides a compound structure called a _pair_, which can be constructed with the primitive procedure `cons`. This procedure takes two arguments and returns a compound data object that contains the two arguments as parts.

Given a pair, we can extract the parts using the primitive procedures `car` and `cdr`:

```scheme
(define x (cons 1 2))
(car x) ; 1
(cdr x) ; 2
```

Data objects constructed from pairs are called _list-structured_ data.

## Abstraction Barriers

In general, the underlying idea of data abstraction is to identify for each type of data object a basic set of operations in terms of which all manipulations of data objects of that type will be expressed, and then to use only those operations in manipulating the data.

_Abstraction barriers_ isolate different 'levels' of the system. At each level, the barrier separates the programs that `use` the data abstraction from the programs that `implement` the data abstraction. In effect, procedures at each level are the interfaces that define the abstraction barriers and connect the different levels.

This idea has many advantages. One advantage is that it makes programs much easier to maintain and modify. Any complex data structure can be represented in a variety of ways with the primitive data structures provided by a PL. The choice of representation influences the programs that operate on it; thus, if the representation were to be changed at some later time, all such programs might have to be modified accordingly. This task could be time-consuming and expensive in the case of large programs unless the dependence on the representation were to be confined by design to a very few program modules.

Constraining the dependence on the representation to a few interface procedures helps us design programs as well as modify them, because it allows us to maintain the flexibility to consider alternate implementations. The data-abstraction methodology gives us a way to defer implementation decisions without losing the ability to make progress on the rest of the system.

## What Is Meant by Data?

What exactly is meant by data? It is not enough to say 'whatever is implemented by the given selectors and constructors.' Clearly, not every arbitrary set of N procedures can serve as an appropriate basis for a particular use case implementation.

In general, we can think of data as defined by some collection of selectors and constructors, together with specified conditions that these procedures must fulfill in order to be a valid representation.

This point of view can serve to define not only 'high-level' data objects, such as rational numbers, but lower-level objects as well.

Consider the notion of a pair. It was never said what a pair was, only that the language supplied procedures `cons`, `car`, and `cdr` for operating on pairs. But the only thing we need to know about these three operations is that if we glue two objects together using `cons` we can retrieve the objects using `car` and `cdr`. That is, the operations satisfy the condition that, for any objects `x` and `y`, if `z` is `(cons x y)` then `(car z)` is `x` and `(cdr z)` is `y`.

These three procedures are included as primitives in our language. However, any triple of procedures that satisfies the above condition can be used as the basis for implementing pairs.

Here are the definitions of a procedural implementation of pairs:

```scheme
(define (cons x y)
  (define (dispatch m)
    (cond ((= m 0) x)
          ((= m 1) y)
          (else (error "Argument not 0 or 1 -- CONS" m))))
  dispatch)

(define (car z)
  (z 0))

(define (cdr z)
  (z 1))
```

As long as only `cons`, `car`, and `cdr` are used to access pairs, this procedural implementation is indistinguishable from one that uses `real` data structures.

The procedural representation, although obscure, is a perfectly adequate way to represent pairs, since it fulfills the only conditions that pairs need to fulfill. This example also demonstrates that the ability to manipulate procedures as objects automatically provides the ability to represent compound data. This style of programming is often called _message passing_ (used to address the issues of modeling and simulation in chapter 3).

## Lambda Calculus

Lambda calculus is a formal system in mathematical logic for expressing computation based on function abstraction and application using variable binding and substitution.

Lambda calculus consists of constructing lambda terms and performing reduction operations on them.

In the simplest form of lambda calculus, terms are built using only the following rules:

| Syntax | Name | Description |
| ------ | ---- | ----------- |
| `x` | Variable | A character of string representing a parameter or mathematical/logical value. |
| `(λx.M)` | Abstraction | Function definition (`M` is a lambda term). The variable `x` becomes bound in the expression. |
| `(M N)` | Application | Applying a function to an argument. `M` and `N` are lambda terms. |

The reduction operations include:

| Operation | Name | Description |
| --------- | ---- | ----------- |
| `(λx.M[x]) -> (λy.M[y])` | α-conversion | Renaming the bound variables in the expression. Used to avoid name collisions. |
| `((λx.M) E) -> (M[x := E])` | β-reduction | Replacing the bound variables with the argument expression in the body of the abstraction. |

The lambda calculus provides a simple semantics for computation, enabling properties of computation to be studied formally. It incorporates two simplifications that make this semantics simple:
1. All functions in the lambda calculus are anonymous functions, having no names.
2. Functions only accept one input variable, with currying used to implement functions with several variables.

_Currying is the technique of converting a function that takes multiple arguments into a sequence of functions that each takes a single argument._

For example, the function:

`square_sum(x,y) = x^2 + y^2`

can be rewritten in anonymous form as

`(x,y) -> x^2 + y^2`

which can be reworked into

`x -> (y -> x^2 + y^2)`

Function application of the `square_sum` function to the arguments (5,2), yields at once

```
((x,y) -> x^2 + y^2))(5,2)
= 5^2 + 2^2
= 29
```

whereas evaluation of the curried version requires one more step

```
((x -> (y -> x^2 + y^2))(5))(2)
= (y -> 5^2 + y^2)(2) // β-reduction
= 5^2 + 2^2 // β-reduction
= 29
```

## Church numerals

Church numerals are the representations of natural numbers under Church encoding.

The higher-order function that represents natural number n is a function that maps any function f to its n-fold composition.

In simpler terms, the 'value' of the numeral is equivalent to the number of times the function encapsulates its argument.

Church numerals 0, 1, 2, ..., are defined as follows in the lambda calculus.
Starting with 0 not applying the function at all, proceed with 1 applying the function once, 2 applying the function twice, 3 applying the function three times, etc.:

| Number | Function definition | Lambda expression |
| ------:| ------------------- | ----------------- |
| 0 | 0 f x = `x` | 0 = `λf.λx.x` |
| 1 | 1 f x = `f x` | 1 = `λf.λx.f x` |
| 2 | 2 f x = `f(f x)` | 2 = `λf.λx.f(f x)` |
| 3 | 3 f x = `f(f(f x))` | 3 = `λf.λx.f(f(f x))` |
| ... | ... | ... |
| n | n f x = `f(f x)` | 2 = `λf.λx.(f x)^n` |

The Church numeral __3__ represents the action of applying any given function three times to a value. The supplied function is first applied to a supplied parameter and then successively to its own result. The end result is not the numeral 3 (unless the supplied parameter happens to be 0 and the function is a successor function). The function itself, and not its end result, is the Church numeral __3__.

The Church numeral __3__ means simply to do anything three times. It is an ostensive demonstration of what is meant by 'three times'.

## Resources

- [SICP - Introduction to Data Abstraction](https://mitpress.mit.edu/sites/default/files/sicp/full-text/book/book-Z-H-14.html)
- [Message passing](https://en.wikipedia.org/wiki/Message_passing)
- [Lambda calculus](https://en.wikipedia.org/wiki/Lambda_calculus)
- [Church numerals - Wikipedia](https://en.wikipedia.org/wiki/Church_encoding#Church_numerals)
- [Church numerals - UNC](https://www.cs.unc.edu/~stotts/723/Lambda/church.html)
- [Natural numbers as Church numerals](https://www.cs.unc.edu/~stotts/723/Lambda/church.html)
