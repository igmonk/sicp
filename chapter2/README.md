# Building Abstractions with Data

Programs are typically designed to model complex phenomena, and more often than not one must construct computational objects that have several parts in order to model real-world phenomena that have several aspects. Thus, whereas our focus in chapter 1 was on building abstractions by combining procedures to form compound procedures, we turn in this chapter to another key aspect of any PL: the means it provides for building abstractions by combining data objects to form _compound data_.

Just as the ability to define procedures enables us to deal with processes at a higher conceptual level than that of the primitive operations of the language, the ability to construct compound data objects enables us to deal with data at a higher conceptual level than that of the primitive data objects of the language.

The use of compound data also enables us to increase the modularity of our programs. The general technique of isolating the parts of a program that deal with how data objects are represented from the parts of a program that deal with how data objects are used is a powerful design methodology called _data abstraction_.

The chapter will form the background for our discussion of compound data and data abstraction. As with compound procedures, the main issue to be addressed is that of abstraction as a technique for coping with complexity, and we will see how data abstraction enables us to erect suitable _abstraction barriers_ between different parts of a program.

It will be shown that the key to forming compound data is that a PL should provide some kind of 'glue' so that data objects can be combined to form more complex data objects.

The following topics are covered:
- how to form compound data using no special 'data' operations at all, only procedures
- some conventional techniques for representing sequences and trees
- the notion of _closure_ - a technique that allows us to combine not only primitive data object, but compound ones as well
- the idea that compound data objects can serve as conventional interfaces for combining program modules in mix-and-match ways
- _symbolic expressions_ - data whose elementary parts can be arbitrary symbols rather than only numbers
- various alternatives for representing sets of objects
- the ability to represent a given data structure in terms of simpler objects in many ways, where the choice of representation can have significant impact on the time and space requirements of processes that manipulate the data
- the problem of working with data that may be represented differently by different parts of a program, which leads to the need to implement _generic operations_, which must handle many different types of data
- _data-directed programming_ as a technique that allows individual data representations to be designed in isolation and then combined _additively_ (i.e., without modification)

## Resources

- [SICP - Building Abstractions with Data](https://mitpress.mit.edu/sites/default/files/sicp/full-text/book/book-Z-H-13.html)
