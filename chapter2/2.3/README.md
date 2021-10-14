# 2.3 Symbolic Data

All the compound data objects we have used so far were constructed ultimately from numbers. In this section we extend the representational capability of our language by introducing the ability to work with arbitrary symbols as data.

## Quotation

In order to manipulate symbols we need a new element in our language: the ability to _quote_ a data object. Suppose we want to construct the list `(a b)`. We can't accomplish this with `(list a b)`, because this expression constructs a list of the `values` of `a` and `b` rather than the symbols themselves.

The common practice in natural languages is to use quotation marks to indicate that a word or a sentence is to be treated literally as a string of characters. For instance, the first letter of `John` is clearly `J`. If we tell somebody `say your name aloud`, we expect to hear that person's name. However, if we tell somebody `say 'your name' aloud`, we expect to hear the words `your name`.

This same practice is used to identify lists and symbols that are to be treated as data objects rather than as expressions to be evaluated. The meaning of the single quote character is to quote the next object.

```scheme
(define a 1)
(define b 2)
(list a b)   ; (1 2)
(list 'a 'b) ; (a b)
(list 'a b)  ; (a 2)
```

Quotation also allows us to type in compound objects, using the conventional printed representation for lists:

```scheme
(car '(a b c)) ; a
(cdr '(a b c)) ; (b c)
```

The empty list is obtained by evaluating '(), the variable `nil` can be dispensed with.

`eq?` takes two symbols as arguments and tests whether they are the same.

## Resources

- [SICP - Symbolic Data](https://mitpress.mit.edu/sites/default/files/sicp/full-text/book/book-Z-H-16.html)
