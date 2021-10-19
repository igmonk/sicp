# 2.4 Multiple Representations for Abstract Data

Data-abstraction barriers are powerful tools for controlling complexity. By isolating the underlying representations of data objects, we can divide the task of designing a large program into smaller tasks that can be performed separately. But this kind of data abstraction is not yet powerful enough, because it may not always make sense to speak of 'the underlying representation' for a data object.

For one thing, there might be more than one useful representation for a data object, and we might like to design systems that can deal with multiple representations.

More importantly, programming systems are often designed by many people working over extended periods of time, subject to requirements that change over time. In such an environment, it is simply not possible for everyone to agree in advance on choices of data representation.

So in addition to the data-abstraction barriers that isolate representation from use, we need abstraction barriers that isolate different design choices from each other and permit different choices to coexist in a single program.

Furthermore, since large programs are often created by combining pre-existing modules that were designed in isolation, we need conventions that permit programmers to incorporate modules into larger systems _additively_, that is, without having to redesign or reimplement these modules.

That leads to systems containing two different kinds of abstraction barriers:
1. The 'horizontal' abstraction barriers isolate 'higher-level' operations from 'lower-level' representations
2. The 'vertical' barriers give us the ability to separately design and install alternative representations

This section shows how to cope with data that may be represented in different ways by different parts of a program. This requires:
- constructing _generic procedures_ - procedures that can operate on data that may be represented in more than one way
- working in terms of data objects that have _type tags_, that is, data objects that include explicit information about how they are to be processed (our main technique for building generic procedures)
- _data-directed_ programming, a powerful and convenient implementation strategy for additively assembling systems with generic operations

## Representations for Complex Numbers

We begin by discussing two plausible representations for complex numbers as ordered pairs:
1. rectangular form (real part and imaginary part)
2. polar form (magnitude and angle)

Thus, there are two different representations for complex numbers, which are appropriate for different operations:
1. Addition
   ```
   Re(z1 + z2) = Re(z1) + Re(z2)
   Im(z1 + z2) = Im(z1) + Im(z2)
   ```
2. Multiplication
   ```
   Mag(z1 * z2) = Mag(z1) * Mag(z2)
   Ang(z1 * z2) = Ang(z1) + Ang(z2)
   ```

From the viewpoint of someone writing a program that uses complex numbers, the principle of data abstraction suggests that all the operations for manipulating complex numbers should be available regardless of which representation is used by the computer.

## Tagged data

One way to view data abstraction is as an application of the 'principle of least commitment'. In implementing the complex-number system above, we can use either rectangular representation or polar representation.

The abstraction barrier formed by the selectors and constructors permits us to defer to the last possible moment the choice of a concrete representation for our data objects and thus retain maximum flexibility in our system design.

The principle of least commitment can be carried to even further extremes. If we desire, we can maintain the ambiguity of representation even after we have designed the selectors and constructors, and elect to use both representations. If both representations are included in a single system, however, we will need some way to distinguish data in polar form from data in rectangular form.

A straightforward way to accomplish this distinction is to include a _type tag_ - the symbol `rectangular` or `polar` - as part of each complex number. Then when we need to manipulate a complex number we can use the tag to decide which selector to apply.

The resulting complex-number system has been decomposed into three relatively independent parts:
1. the complex-number-arithmetic operations
2. the polar implementation
3. the rectangular implementation.

The polar and rectangular implementations could have been written by two different programmers working separately, and both of these can be used as underlying representations by a third programmer implementing the complex-arithmetic procedures in terms of the abstract constructor/selector interface.

Since each data object is tagged with its type, the selectors operate on the data in a generic manner. That is, each selector is defined to have a behavior that depends upon the particular type of data it is applied to.

Notice the general mechanism for interfacing the separate representations: within a given representation implementation (say, the polar package) a complex number is an untyped pair (magnitude, angle). When a generic selector operates on a number of polar type, it strips off the tag and passes the contents on to the polar package code. Conversely, when the package developer constructs a number for general use, they tag it with a type so that it can be appropriately recognized by the higher-level procedures.

## Data-Directed Programming and Additivity

The general strategy of checking the type of a datum and calling an appropriate procedure is called _dispatching on type_. This is a powerful strategy for obtaining modularity in system design.

Implementing the dispatch as in the previous section (see the workbook) has two significant weaknesses:
1. the generic interface procedures (`real-part`, `imag-part`, `magnitude`, and `angle`) must know about all the different representations
2. even though the individual representations can be designed separately, we must guarantee that no two procedures in the entire system have the same name

The issue underlying both of these weaknesses is that the technique for implementing generic interfaces is not _additive_. The person implementing the generic selector procedures must modify those procedures each time a new representation is installed, and the people interfacing the individual representations must modify their code to avoid name conflicts.

What we need is a means for modularizing the system design even further. This is provided by the programming technique known as _data-directed programming_. 

Observation: whenever we deal with a set of generic operations that are common to a set of different types we are, in effect, dealing with a two-dimensional table that contains the possible operations on one axis and the possible types on the other axis. The entries in the table are the procedures that implement each operation for each type of argument presented.

| Operations | Types |             |
|------------|------:|-------------|
|            | Polar | Rectangular |
| real-part | real-part-polar | real-part-rectangular |
| imag-part | imag-part-polar | imag-part-rectangular |
| magnitude | magnitude-polar | magnitude-rectangular |
| angle     | angle-polar     | angle-rectangular     |

Data-directed programming is the technique of designing programs to work with such a table directly.

Previously, the mechanism that interfaces the complex-arithmetic code with the two representation packages was implemented as a set of procedures that each perform an explicit dispatch on type.

Here the interface is implemented as a single procedure that looks up the combination of the operation name and argument type in the table to find the correct procedure to apply, and then applies it to the contents of the argument.

To add a new representation package to the system we need not change any existing procedures; we need only add new entries to the table.

### Message passing

An alternative implementation strategy is to decompose the operation-and-type table into columns and, instead of using 'intelligent operations' that dispatch on data types, to work with 'intelligent data objects' that dispatch on operation names.

This can be achieved by arranging things so that a data object, such as a rectangular number, is represented as a procedure that takes as input the required operation name and performs the operation indicated.

This style of programming is called _message passing_. The name comes from the image that a data object is an entity that receives the requested operation name as a 'message'.

Message passing is a useful technique for organizing systems with generic operations. One limitation of this organization is it permits only generic procedures of one argument.

## Resources

- [SICP - Multiple Representations for Abstract Data](https://mitpress.mit.edu/sites/default/files/sicp/full-text/book/book-Z-H-17.html)
