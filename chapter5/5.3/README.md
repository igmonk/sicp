# 5.3 Storage Allocation and Garbage Collection

For the sake of simplicity, we will assume that our register machines can be equipped with a _list-structured_ memory, in which the basic operations for manipulating list-structured data are primitive. This does not, however, reflect a realistic view of the actual primitive data operations of contemporary computers.

There are two considerations in implementing list structure:
- an issue of representation: how to represent the "box-and-pointer" structure of Lisp pairs, using only the storage and addressing capabilities of typical computer memories
- the management of memory as a computation proceeds

Lisp systems provide an _automatic storage allocation_ facility to support the illusion of an infinite memory. When a data object is no longer needed, the memory allocated to it is automatically recycled and used to construct new data objects.

There are various techniques for providing such automatic storage allocation. The method discussed in this section is called _garbage collection_.

## Memory as Vectors

A conventional computer memory can be thought of as an array of cubbyholes, each of which can contain a piece of information. Each cubbyhole has a unique name, called its _address_ or _location_.

Typical memory systems provide two primitive operations:
- fetch the data stored in a specified location
- assigns new data to a specified location

Memory addresses can be incremented to support sequential access to some set of the cubbyholes. More generally, many important data operations require that memory addresses be treated as data, which can be stored in memory locations and manipulated in machine registers.

The representation of list structure is one application of such _address arithmetic_.

To model computer memory, we use a new kind of data structure called a _vector_. Abstractly, a vector is a compound data object whose individual elements can be accessed by means of an integer index in an amount of time that is independent of the index.

In order to describe memory operations, two primitive Scheme procedures for manipulating vectors are used:
- `(vector-ref <vector> <n>)` returns the n-th element of the vector
- `(vector-set! <vector> <n> <value>)` sets the n-th element of the vector to the designated value

For computer memory, this access can be implemented through the use of address arithmetic to combine a _base address_ that specifies the beginning location of a vector in memory with an _index_ that specifies the offset of a particular element of the vector.

#### Representing Lisp data

We can use vectors to implement the basic pair structures required for a list-structured memory. Let us imagine that computer memory is divided into two vectors: `the-cars` and `the-cdrs`.

List structure will be represented as follows:
- a pointer to a pair is an index into the two vectors
- the `car` of the pair is the entry in `the-cars` with the designated index
- the `cdr` of the pair is the entry in `the-cdrs` with the designated index

There are many methods of accomplishing a way to distinguish one kind of data from another. But they all reduce to using _typed pointers_, that is, to extending the notion of "pointer" to include information on data type.

The data type enables the system to distinguish a pointer to a pair from pointers to other kinds of data. Two data objects are considered to be the same `(eq?)` if their pointers are identical.

#### Implementing the primitive list operations

Each "primitive" list operation of a register machine can be replaced with one or more primitive vector operations.

Two registers, `the-cars` and `the-cdrs` will be used to identify the memory vectors. It is assumed that `vector-ref` and `vector-set!` are available as primitive operationos. In addition, it is assumed that numeric operations on pointers (incrementing a pointer, adding two pointers, etc.) use only the index portion of the typed pointer.

To support the instructions

```scheme
(assign <reg1> (op car) (reg <reg2>))
(assign <reg1> (op cdr) (reg <reg2>))
```

they can be implemeneted, respectively, as

```scheme
(assign <reg1> (op vector-ref) (reg the-cars) (reg <reg2>))
(assign <reg1> (op vector-ref) (reg the-cdrs) (reg <reg2>))
```

The instructions

```scheme
(perform (op set-car!) (reg <reg1>) (reg <reg2>))
(perform (op set-cdr!) (reg <reg1>) (reg <reg2>))
```

are implemented as

```scheme
(perform
  (op vector-set!) (reg the-cars) (reg <reg1>) (reg <reg2>))
(perform 
  (op vector-set!) (reg the-cdrs) (reg <reg1>) (reg <reg2>))
```

`cons` is performed by allocating an unused index and storing the arguments to `cons` in `the-cars` and `the-cdrs` at that indexed vector position. We presume that there is a special register, `free`, that always holds a pair pointer containing the next available index, and that we can increment the index part of that pointer to find the next free location.

For example, the instruction

```scheme
(assign <reg1> (op cons) (reg <reg2>) (reg <reg3>))
```

is implemented as the following sequence of vector operations:

```scheme
(perform
  (op vector-set!) (reg the-cars) (reg free) (reg <reg2>))
(perform
  (op vector-set!) (reg the-cdrs) (reg free) (reg <reg3>))
(assign <reg1> (reg free))
(assign free (op +) (reg free) (const 1))
```

The `eq?` operation

```scheme
(op eq?) (reg <reg1>) (reg <reg2>)
```

simply tests the equality of all fields in the registers, and predicates such as `pair?`, `null?`, `symbol?`, and `number?` need only check the type field.

#### Implementing stacks

The stack can be a list of the saved values, pointed to by a special register the-stack. Thus, 

```scheme
(save <reg>)
```

can be implemented as

```scheme
(assign the-stack (op cons) (reg <reg>) (reg the-stack))
```

Similarly, 

```scheme
(restore <reg>)
```

can be implemented as

```scheme
(assign <reg> (op car) (reg the-stack))
(assign the-stack (op cdr) (reg the-stack))
```

and 

```scheme
(perform (op initialize-stack))
```

can be implemented as

```scheme
(assign the-stack (const ()))
```

In conventional computer architectures, however, it is usually advantageous to allocate the stack as a separate vector. Then pushing and popping the stack can be accomplished by incrementing or decrementing an index into that vector.

## Maintaining the Illusion of Infinite Memory

With a real computer we will eventually run out of free space in which to construct new pairs. However, most of the pairs generated in a typical computation are used only to hold intermediate results. After these results are accessed, the pairs are no longer needed - they are _garbage_.

If we can arrange to collect all the garbage periodically, and if this turns out to recycle memory at about the same rate at which we construct new pairs, we will have preserved the illusion that there is an infinite amount of memory.

In order to recycle pairs, we must have a way to determine which allocated pairs are not needed (in the sense that their contents can no longer influence the future of the computation).

_Garbage collection_ is based on the observation that, at any moment in a Lisp interpretation, the only objects that can affect the future of the computation are those that can be reached by some succession of `car` and `cdr` operations starting from the pointers that are currently in the machine registers. Any memory cell that is not so accessible may be recycled.

An alternative commonly used garbage-collection technique is the _mark-sweep_ method. This consists of tracing all the structure accessible from the machine registers and marking each pair we reach. We then scan all of memory, and any location that is unmarked is "swept up" as garbage and made available for reuse.

#### Implementation of a stop-and-copy garbage collector



## Resources

- [SICP - Storage Allocation and Garbage Collection](https://mitpress.mit.edu/sites/default/files/sicp/full-text/book/book-Z-H-33.html)
- [MIT Scheme - Vectors](https://www.gnu.org/software/mit-scheme/documentation/stable/mit-scheme-ref.html#Vectors)
