# 2.5 Systems with Generic Operations

In the previous section, we saw how to design systems in which data objects can be represented in more than one way. The key idea is to link the code that specifies the data operations to the several representations by means of generic interface procedures.

This section reveals how to use this same idea not only to define operations that are generic over different representations but also to define operations that are generic over different kinds of arguments.

## Generic Arithmetic Operations

The task of designing generic arithmetic operations is analogous to that of designing the generic complex-number operations. We would like, for instance, to have a generic addition procedure `add` that acts like ordinary primitive addition `+` on ordinary numbers, like `add-rat` on rational numbers, and like `add-complex` on complex numbers.

## Combining Data of Different Types

It is meaningful to define operations that cross the type boundaries, such as the addition of a complex number to an ordinary number.

The cross-type operations must be introduced in some carefully controlled way, so that they can be supported without serviously violating the module boundaries.

One way to handle cross-type operations is to design a different procedure for each possible combination of types for which the operation is valid. This technique works, but it is cumbersome. With such a system, the cost of introducing a new type is not just the construction of the package of procedures for that type but also the construction and installation of the procedures that implement the cross-type operations.

### Coercion

In the general situation of completely unrelated operations acting on completely unrelated types, implementing explicit cross-type operations, cumbersome though it may be, is the best that one can hope for.

Often the different data types are not completely independent, and there may be ways by which objects of one type may be viewed as being of another type. This process is called _coercion_.

For example, if we are asked to arithmetically combine an ordinary number with a complex number, we can view the ordinary number as a complex number whose imaginary part is zero. This transforms the problem to that of combining two complex numbers, which can be handled in the ordinary way by the complex-arithmetic package.

In general, we can implement this idea by designing coercion procedures that transform an object of one type into an equivalent object of another type. The appropriate transformation between types depends only on the types themselves, not on the operation to be applied.

### Hierarchies of types

The coercion scheme for an arithmetic system relies on the existence of natural relations between pairs of types. In such a system, it is quite natural to regard an integer as a special kind of rational number, which is in turn a special kind of real number, which is in turn a special kind of complex number.

What we actually have is a so-called _hierarchy of types_, in which, for example, integers are a _subtype_ of rational numbers (i.e., any operation that can be applied to a rational number can automatically be applied to an integer). Conversely, we say that rational numbers form a _supertype_ of integers. The particular hierarchy we have here is of a very simple kind, in which each type has at most one supertype and at most one subtype. Such a structure, called a _tower_.

Advantages of a `tower`:
- the problem of adding a new type to the hierarchy boils down to specifying how the new type is embedded in the next supertype above it and how it is the supertype of the type below it
- the ease of implementing the notion that every type 'inherits' all operations defined on a supertype. If the required operation is not directly defined for the type of the object given, we raise the object to its supertype and try again
- it gives us a simple way to 'lower' a data object to the simplest representation

### Inadequacies of hierarchies

In a more complex arrangement of mixed types, a type may have more than one subtype, as well as more than one supertype. This multiple-supertypes issue is particularly thorny, since it means that there is no unique way to 'raise' a type in the hierarchy.

Finding the 'correct' supertype in which to apply an operation to an object may involve considerable searching through the entire type network on the part of a procedure such as apply-generic. Since there generally are multiple subtypes for a type, there is a similar problem in coercing a value 'down' the type hierarchy.

Dealing with large numbers of interrelated types while still preserving modularity in the design of large systems is very difficult, and is an area of much current research. Developing a useful, general framework for expressing the relations among different types of entities (what philosophers call 'ontology') seems intractably difficult.

## Resources

- [SICP - Systems with Generic Operations](https://mitpress.mit.edu/sites/default/files/sicp/full-text/book/book-Z-H-18.html)
