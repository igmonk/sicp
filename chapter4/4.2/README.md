# 4.2 Variations on a Scheme - Lazy Evaluation

New languages are often invented by first writing an evaluator that embeds the new language within an existing high-level language.

Not only does the high-level implementation base make it easier to test and debug the evaluator; in addition, the embedding enables the designer to snarf [слизывать] features from the underlying language, just as our embedded Lisp evaluator uses primitives and control structure from the underlying Lisp. Only later (if ever) need the designer go to the trouble of building a complete implementation in a low-level language or in hardware.

## Normal Order and Applicative Order

Evaluation orders:
- _applicative order_ - all the arguments to procedures are evaluated when the procedure is applied
- _normal order_ - evaluation of procedure arguments is delayed until the actual argument values are needed

Scheme is an applicative-order language.

Delaying evaluation of procedure arguments untils the last possible moment (e.g., until they are required by a primitive operation) is called _lazy evaluation_.

Consider the procedure:

```scheme
(define (unless condition usual-value exceptional-value)
  (if condition exceptional-value usual-value))
```

that can be used in expressions such as

```scheme
(let ((a 0) (b 0))
  (unless (= b 0)
    (/ a b)
    (begin (display "exception: returning 0")
           0)))
```

This won't work in an applicative-order language because both the usual value and the exceptional value will be evaluated before unless is called.

An advantage of lazy evaluation is that some procedures, such as `unless`, can do useful computation even if evaluation of some of their arguments would produce errors or would not terminate.

## An Interpreter with Lazy Evaluation

With lazy evaluation, when applying a procedure, the interpreter must determine which arguments are to be evaluated and which are to be delayed. The delayed arguments are not evaluated; instead, they are transformed into objects called _thunks_.

The thunk must contain the information required to produce the value of the argument when it is needed, as if it had been evaluated at the time of the application. Thus, the thunk must contain the argument expression and the environment in which the procedure application is being evaluated.

The process of evaluating the expression in a thunk is called _forcing_. In general, a thunk will be forced only when its value is needed:
- when it is passed to a primitive procedure that will use the value of the thunk
- when it is the value of a predicate of a conditional
- when it is the value of an operator that is about to be applied as a procedure

A thunk can be _memoized_ - the first time a thunk is forced, it stores the value that is computed; subsequent forcings simply return the stored value without repeating the computation.

## Streams as Lazy Lists

A special form is not a first-class object like a procedure, so we cannot use it together with higher-order procedures.

With lazy evaluation, streams and lists can be identical, so there is no need for special forms or for separate list and stream operations. All we need to do is to arrange matters so that `cons` is _non-strict_. One way to accomplish this is to extend the lazy evaluator to allow for non-strict primitives, and to implement `cons` as one of these.

Alternatively, if we had originally included `cons`, `car`, and `cdr` as primitives in the global environment, they can be redefined with any other procedural representation, for ex.:

```scheme
(define (cons x y) (lambda (m) (m x y)))
(define (car z) (z (lambda (p q) p)))
(define (cdr z) (z (lambda (p q) q)))
```

Or, in case of an _upward-compatible extension_ (ex. 4.31) apply the following transformation to the parameters: `x -> (x lazy-memo)`. For example:

```scheme
(define (cons (x lazy-memo) (y lazy-memo))
  (lambda ((m lazy-memo)) (m x y)))
```

In terms of these basic operations, the standard definitions of the list operations will work with infinite lists (streams) as well as finite ones, and the stream operations can be implemented as list operations.

Note that these lazy lists are even lazier than the streams of chapter 3: the `car` of the list, as well as the `cdr`, is delayed. This permits us to create delayed versions of more general kinds of list structures, not just sequences. Hughes 1990 discusses some applications of "lazy trees".

In fact, even accessing the `car` or `cdr` of a lazy pair need not force the value of a list element. The value will be forced only when it is really needed - e.g., for use as the argument of a primitive, or to be printed as an answer.

## Resources

- [SICP - Variations on a Scheme - Lazy Evaluation](https://mitpress.mit.edu/sites/default/files/sicp/full-text/book/book-Z-H-27.html)
