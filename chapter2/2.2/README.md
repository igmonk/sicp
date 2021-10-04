# 2.2 Hierarchical Data and the Closure Property

As shown in Chapter 2.1, `cons` can be used to combined not only numbers but pairs as well. As a consequence, pairs provide a universal building block from which we can construct all sorts of data structures.

The ability to create pairs whose elements are pairs is the essence of list structure's importance as a representational tool. This property is referred to as the _closure property_ of `cons`.

In general, an operation for combining data objects satisfies the closure property if the results of combining things with that operation can themselves be combined using the same operation.

```
The use of the word 'closure' here comes from abstract algebra, where a set of elements is said to be closed under an operation if applying the operation to elements in the set produces an element that is again an element of the set.

The Lisp community also (unfortunately) uses the word 'closure' to describe a totally unrelated concept: A closure is an implementation technique for representing procedures with free variables.

The word 'closure' is not used in this second sense here.
```

Closure is the key to power in any means of combination because it permits us to create _hierarchical_ structures - structures made up of parts, which themselves are made up of parts, and so on.

## Representing Sequences

One of the useful structures we can build with pairs is a _sequence_ - an ordered collection of data objects.

Representation of a sequence as a chain of pairs constructed by nested `cons` operations:

```scheme
(cons 1
      (cons 2
            (cons 3
                  (cons 4 nil))))
```

- the `car` of each pair is the corresponding item in the chain
- the `cdr` of the pair is the next pair in the chain
- the `cdr` of the final pair signals the end of the sequence

_Note: `nil` is no longer of Scheme anymore, use `'()` instead._

Such a sequence of pairs, formed by nested `cons`-es, is called a _list_, and Scheme provides a primitive called `list` to help in constructing lists.

The aboove sequence could be produced by:

```scheme
(list 1 2 3 4)
```

Be careful not to confuse the expression `(list 1 2 3 4)` with the list `(1 2 3 4)`, which is the result obtained when the expression is evaluated. Attempting to evaluate the expression `(1 2 3 4)` will signal an error when the interpreter tries to apply the procedure `1` to arguments `2`, `3`, and `4`.

### List operations

The use of pairs to represent sequences of elements as lists is accompanied by conventional programming techniques for manipulating lists by successively 'cdr-ing down' the lists, such as:
- the n-th item
- length
- append, etc.

To aid in this, Scheme includes a primitive predicate `null?`, which tests whether its argument is the empty list.

### Mapping over lists

One extremely useful operation is to apply some transformation to each element in a list and generate the list of results. This general idea can be abstracted and captured as a common pattern expressed as a higher-order procedure - `map`.

`map` is an important construct, not only because it captures a common pattern, but because it establishes a higher level of abstraction in dealing with lists. `map` helps establish an abstraction barrier that isolates the implementation of procedures that transform lists from the details of how the elements of the list are extracted and combined.

This abstraction gives us the flexibility to change the low-level details of how sequences are implemented, while preserving the conceptual framework of operations that transform sequences to sequences.

## Hierarchical Structures

The representation of sequences in terms of lists generalizes naturally to represent sequences whose elements may themselves be sequences. Another way to think of sequences whose elements are sequences is as _trees_. The elements of the sequence are the branches of the tree, and elements that are themselves sequences are subtrees.

Recursion is a natural tool for dealing with tree structures, since we can often reduce operations on trees to operations on their branches, which reduce in turn to operations on the branches of the branches, and so on, until we reach the leaves of the tree.

### Mapping over trees

Just as `map` is a powerful abstraction for dealing with sequences, `map` together with recursion is a powerful abstraction for dealing with trees.

Many tree operations can be implemented by similar combinations of sequence operations and recursion.

## Sequences as Conventional Interfaces

In working with compound data, data abstraction permits us to design programs without becoming enmeshed in the details of data representations, and how abstraction preserves for us the flexibility to experiment with alternative representations. This section introduces another powerful design principle for working with data structures - the use of _conventional interfaces_.

Program abstractions, implemented as higher-order procedures, can capture common patterns in programs that deal with numerical data. Our ability to formulate analogous operations for working with compound data depends crucially on the style in which we manipulate our data structures.

A signal-processing engineer would find it natural to conceptualize  processes in terms of _signals_ flowing through a cascade of stages, each of which implements part of the program plan, consisting of _enumerator_, _filter_, _map_, _accumulator_, etc.

Unfortunately, many procedure definitions fail to exhibit this signal-flow structure: there are no distinct parts of any procedure that correspond to the elements in the signal-flow description. Those procedures decompose the computations in a different way, spreading the enumeration over the program and mingling it with the map, the filter, and the accumulation. If we could organize our programs to make the signal-flow structure manifest in the procedures we write, this would increase the conceptual clarity of the resulting code.

### Sequence Operations

The key to organizing programs so as to more clearly reflect the signal-flow structure is to concentrate on the 'signals' that flow from one stage in the process to the next. If we represent these signals as lists, then we can use list operations to implement the processing at each of the stages.

The value of expressing programs as sequence operations is that this helps us make program designs that are modular, that is, designs that are constructed by combining relatively independent pieces. We can encourage modular design by providing a library of standard components together with a conventional interface for connecting the components in flexible ways.

Modular construction is a powerful strategy for controlling complexity in engineering design. In real signal-processing applications, for example, designers regularly build systems by cascading elements selected from standardized families of filters and transducers. Similarly, sequence operations provide a library of standard program elements that we can mix and match.

Sequences serve as a conventional interface that permits us to combine processing modules. Additionally, when we uniformly represent structures as sequences, we have localized the data-structure dependencies in our programs to a small number of sequence operations. By changing these, we can experiment with alternative representations of sequences, while leaving the overall design of our programs intact.

### Nested Mappings

The sequence paradigm can be extended to include many computations that are commonly expressed using nested loops.pr

The combination of mapping and accumulating with append is so common that it is often defined as a separate procedure:

```scheme
(define (flatmap proc seq)
  (accumulate append '() (map proc seq)))
```

## Levels of language for robust design

We have also obtained a glimpse of another crucial idea about languages and program design. This is the approach of _stratified design_, the notion that a complex system should be structured as a sequence of levels that are described using a sequence of languages.

Each level is constructed by combining parts that are regarded as primitive at that level, and the parts constructed at each level are used as primitives at the next level. The language used at each level of a stratified design has primitives, means of combination, and means of abstraction appropriate to that level of detail.

Stratified design helps make programs _robust_, that is, it makes it likely that small changes in a specification will require correspondingly small changes in the program. In general, each level of a stratified design provides a different vocabulary for expressing the characteristics of the system, and a different kind of ability to change it.

## Resources

- [SICP - Hierarchical Data and the Closure Property](https://mitpress.mit.edu/sites/default/files/sicp/full-text/book/book-Z-H-15.html)
- [SICP - a footnote on nil](https://mitpress.mit.edu/sites/default/files/sicp/full-text/book/book-Z-H-15.html#footnote_Temp_158)
