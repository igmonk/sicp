# Metalinguistic Abstraction

Establishing new languages is a powerful strategy for controlling complexity in engineering design; we can often enhance our ability to deal with a complex problem by adopting a new language that enables us to describe (and hence to think about) the problem in a different way, using primitives, means of combination, and means of abstraction that are particularly well suited to the problem at hand.

_Metalinguistic abstraction_ - establishing new languages - plays an important role in all branches of engineering design. It is particularly important to computer programming, because in programming not only can we formulate new languages but we can also implement these languages by constructing evaluators.

An _evaluator_ (or _interpreter_) for a programming language is a procedure that, when applied to an expression of the language, performs the actions required to evaluate that expression.

It is no exaggeration to regard this as the most fundamental idea in programming:

```
The evaluator, which determines the meaning of expressions in a programming language, is just another program.
```

In fact, we can regard almost any program as the evaluator for some language - one with its own primitives, means of combination, and means of abstraction. Seen from this perspective, the technology for coping with large-scale computer systems merges with the technology for building new computer languages, and computer science itself becomes no more (and no less) than the discipline of constructing appropriate descriptive languages.

## Contents

- A simplified Lisp Scheme evaluator
    ```
    The language implemented by the evaluator will be a subset of the Scheme dialect of Lisp that is used in this book
    ```
- Normal-order evaluation
    ```
    An evaluation model that does not evaluate the operands until their values are needed. Instead it first substitutes operand expressions for parameters until it obtaines an expression involving only primitive operators, and  then performs the evaluation
    ```
- Nondeterministic computing
    ```
    where it is natural to express processes that generate all possible values for expressions and then search for those values that satisfy certain constraints
    ```
- Logic-programming language
    ```
    in which knowledge is expressed in terms of relations, rather than in terms of computations with inputs and outputs
    ```

## Resources

- [SICP - Metalinguistic Abstraction](https://mitpress.mit.edu/sites/default/files/sicp/full-text/book/book-Z-H-25.html)
