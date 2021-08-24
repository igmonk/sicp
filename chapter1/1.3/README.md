# 1.3 Formulating Abstractions with Higher-Order Procedures

One of the things we should demand from a powerful PL is the ability to build abstractions by assigning names to common patterns and then to work in terms of the abstractions directly. Otherwise, it would place us at a serious disadvantage, forcing us to work always at the level of the particular operations that happen to be primitives in the language rather then in terms of higher-level operations.

Often the same programming pattern will be used with a number of different procedures. To express such patterns as concepts, we will need to construct procedures that can accept procedures as arguments or return procedures as values. Procedures that manipulate procedures are called _higher-order procedures_.

## Procedures as Arguments

If some procedures share a common underlying pattern - they are for the most part identical (differing only in the name of the procedure, the function to compute the value based on an argument, etc.) - we could generate each of the procedures by filling in slots in a more general template. These slots are to be transformed into formal parameters.

The presence of such a common pattern is strong evidence that there is a useful abstraction waiting to be brought to the surface.

As program designers, we would like our language to be powerful enough so that we can write a procedure that expresses a general concept itself rather than only procedures that compute its particular applications.

_See the workbook for more details._

## Constructing Procedures Using _lambda_

Sometimes, rather than define procedures, it would be more convenient to have a way to directly specify them (inline). We can do this by introducing the special form _lambda_, which creates procedures.

In general, _lambda_ is used to create procedures in the same way as _define_, except that no name is specified for the procedure:

```scheme
(lambda (<formal-parameters>) <body>)
```

The resulting procedure is just as much a procedure as one that is created using _define_. The only difference is that it has not been associated with any name in the environment. In fact,

```scheme
(define (plus4 x) (+ x 4))
```

is equivalent to

```scheme
(define plus4 (lambda (x) (+ x 4)))
```

Like any expression that has a procedure as its value, a _lambda_ expression can be used as the operator in a combination such as

```scheme
((lambda (x y z) (+ x y (square z))) 1 2 3) ; 12
```

or, more generally, in any context where we would normally use a procedure name.

### Using _let_ to create local variables

Another use of _lambda_ is in creating local variables. We often need local variables in our procedures other than those that have been bound as formal parameters.

There are a few ways to accomplish this:
- using an auxiliary procedure to bind the local variables
- using a _lambda_ expression to specify an anonymous procedure for binding our local variables
- using a special form called _let_ to make it more convenient

The general form of a let expression is

```scheme
(let ((<var1> <exp1>)
      (<var2> <exp2>)
      ...
      (<varn> <expn>))
   <body>)
```

The first part of the _let_ expression is a list of name-expression pairs. When the _let_ is evaluated, each name is associated with the value of the corresponding expression. The body of the _let_ is evaluated with these names bound as local variables. The way this happens is that the _let_ expression is interpreted as an alternate syntax for

```scheme
((lambda (<var1> ...<varn>)
    <body>)
 <exp1>
 ...
 <expn>)
```

No new mechanism is required in the interpreter in order to provide local variables. A _let_ expression is simply syntactic sugar for the underlying _lambda_ application.

## Procedures as General Methods (Обобщенные методы)

In section `1.1.4`, compound procedures were introduced as a mechanism for abstracting patterns of numerical operations so as to make them independent of the particular numbers involved.

With higher-order procedures, such as the _integral_ procedure of section `1.3.1`, a more powerful kind of abstraction was introduced: procedures used to express general methods of computation, independent of the particular functions involved.

In this section, two more elaborate examples are discussed - general methods for finding zeros and fixed points of functions - and it is showed how these methods can be expressed directly as procedures.

_See the workbook for more details._

## 1.3.4 Procedures as Returned Values

The ability to pass procedures as arguments significantly enhances the expressive power of a PL. Even more expressive power can be achieved by creating procedures whose 
returned values are themselves procedures.

In general, there are many ways to formulate a process as a procedure. Experienced programmers know how to choose procedural formulations that are particularly perspicuous, and where useful elements of the process are exposed as separate entities that can be reused in other applications.

### The idea of derivative

Derivative, like average damping, is something that transforms a function into another function.

For instance, the derivative of the function `x -> x^3` is the function `x -> 3x^2`.

In general, if `g` is a function and `dx` is a small number, then the derivative `Dg` of `g` is the function whose values at any number `x` is given (in the limit of small `dx`) by:

```
Dg(x) = (g(x+dx) - g(x)) / dx
```

Thus, the idea of derivative can be expressed as the procedure (taking `dx` to be 0.00001):

```scheme
(define (deriv g)
  (lambda (x)
    (/ (- (g (+ x dx)) (g x))
       dx)))

(define dx 0.00001)
```

Here `deriv` is a procedure that takes a procedure as argument and returns a procedure as value. For example, to approximate the derivative of `x -> x^3` at 5 (whose exact value is 75) we can evaluate:

```scheme
(define (cube x) (* x x x))

((deriv cube) 5) ; 75.00014999664018
```

### Abstractions and first-class procedures

Compound procedures are a crucial abstraction mechanism, because they permit us to express general methods of computing as explicit elements in our programming language.

Higher-order procedures permit us to manipulate these general methods to create further abstractions.

As programmers, we should be alert to opportunities to identify the underlying abstractions in our programs and to build upon them and generalize them to create more powerful abstractions.

The significance of higher-order procedures is that they enable us to represent these abstractions explicitly as elements in our programming language, so that they can be handled just like other computational elements.

In general, PLs impose restrictions on the ways in which computational elements can be manipulated. Elements with the fewest restrictions are said to have _first-class_ status. Some of the 'rights and privileges' of first-class elements are:
- they may be named by variables
- they may be passed as arguments to procedures
- they may be returned as the results of procedures
- they may be included in data structures

Lisp, unlike other common PLs, awards procedures full first-class status. This poses challenges for efficient implementation, but the resulting gain in expressive power is enormous.

## Resources

- [SICP - Procedures and the Processes They Generate](https://mitpress.mit.edu/sites/default/files/sicp/full-text/book/book-Z-H-12.html)
