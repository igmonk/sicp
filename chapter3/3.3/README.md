# 3.3 Modeling with Mutable Data

The desire to model systems composed of objects that have changing state leads us to the need to modify compound data objects, as well as to construct and select from them. In order to model compound objects with changing state, we will design data abstractions to include, in addition to selectors and constructors, operations called _mutators_, which modify data objects.

Data objects for which mutators are defined are known as _mutable data objects_.

### Mutable List Structure

The basic operations on pairs - `cons`, `car`, and `cdr` - can be used to construct list structure and to select parts from list structure, but they are incapable of modifying list structure. The same is true of the list operations we have used so far, such as `append` and `list`, since these can be defined in terms of `cons`, `car`, and `cdr`. To modify list structures we need new operations.

The primitive mutators for pairs are `set-car!` and `set-cdr!`:
- `set-car!` takes two arguments, the first of which must be a pair. It modifies this pair, replacing the `car` pointer by a pointer to the second argument of `set-car!`;
- `set-cdr!` is similar to `set-car!`. The only difference is that the `cdr` pointer of the pair, rather than the `car` pointer, is replaced.

Mutation operations on lists can create 'garbage' that is not part of any accessible structure. Lisp memory-management systems include a garbage collector, which identifies and recycles the memory space used by unneeded pairs.

### Sharing and Identity

The theoretical issues of 'sameness' and 'change' raised by the introduction of assignment were mentioned in section 3.1.3. These issues arise in practice when individual pairs are _shared_ among different data objects.

In general, using `cons` to construct lists will result in an interlinked structure of pairs in which many individual pairs are shared by many different structures.

Sharing can be exploited to greatly extend the repertoire of data structures that can be represented by pairs. On the other hand, sharing can also be dangerous, since modifications made to structures will also affect other structures that happen to share the modified parts. The mutation operations `set-car!` and `set-cdr!` should be used with care; unless we have a good understanding of how our data objects are shared, mutation can have unanticipated results.

The subtleties of dealing with sharing of mutable data objects reflect the underlying issues of 'sameness' and 'change' that were raised in section 3.1.3. We mentioned there that admitting change to our language requires that a compound object must have an 'identity' that is something different from the pieces from which it is composed.

In Lisp, we consider this 'identity' to be the quality that is tested by `eq?`, i.e., by equality of pointers. Since in most Lisp implementations a pointer is essentially a memory address, we are 'solving the problem' of defining the identity of objects by stipulating that a data object 'itself' is the information stored in some particular set of memory locations in the computer. This suffices for simple Lisp programs, but is hardly a general way to resolve the issue of 'sameness' in computational models.

### Mutation is just assignment

We can implement mutable data objects as procedures (as in section `2.3.1`) using assignment and local state. Assignment is all that is needed, theoretically, to account for the behavior of mutable data. As soon as we admit `set!` to our language, we raise all the issues, not only of assignment, but of mutable data in general.

On the other hand, from the viewpoint of implementation, assignment requires us to modify the environment, which is itself a mutable data structure. Thus, assignment and mutation are equipotent: Each can be implemented in terms of the other.

### Representing queues

The mutators `set-car!` and `set-cdr!` enable us to use pairs to construct data structures that cannot be built with `cons`, `car`, and `cdr` alone.

A _queue_ is a sequence in which items are inserted at one end (called the _rear_ of the queue) and deleted from the other end (the _front_). Because items are always removed in the order in which they are inserted, a queue is sometimes called a _FIFO_ (first in, first out) buffer.

Because a queue is a sequence of items, we could certainly represent it as an ordinary list; the front of the queue would be the `car` of the list, inserting an item in the queue would amount to appending a new element at the end of the list, and deleting an item from the queue would just be taking the `cdr` of the list.

However, this representation is inefficient, because in order to insert an item we must scan the list until we reach the end. Since the only method we have for scanning a list is by successive `cdr` operations, this scanning requires `θ(n)` steps for a list of `n` items. A simple modification to the list representation overcomes this disadvantage by allowing the queue operations to be implemented so that they require `θ(1)` steps; that is, so that the number of steps needed is independent of the length of the queue.

The difficulty with the list representation arises from the need to scan to find the end of the list. The reason we need to scan is that, although the standard way of representing a list as a chain of pairs readily provides us with a pointer to the beginning of the list, it gives us no easily accessible pointer to the end. The modification that avoids the drawback is to represent the queue as a list, together with an additional pointer that indicates the final pair in the list. That way, when we go to insert an item, we can consult the rear pointer and so avoid scanning the list.

### Representing tables

A table is a collection of records indexed by identifying keys.

Among other things, a table can be useful for:
- representing sets (section `2.3.3`)
- implementing data-directed programming (section `2.4.3`)

#### One-dimensional table

A one-dimensional table is a table in which each value is stored under a single key.

Built as a mutable list structure, the table is implemented as a list of records, each of which is implemented as a pair consisting of a key and the associated value.

The records are glued together to form a list by pairs whose `cars` point to successive records. These gluing pairs are called the _backbone_ of the table.

In order to have a place that can be changed when a new record is added to the table, the table is built as a _headed list_. A headed list has a special backbone pair at the beginning, which holds a dummy "record".

Operations:
- `assoc` [auxiliary] - returns the record that has the given key as its `car`
- `lookup` [extracts information from a table] - takes a key as argument and returns the associated value (or `false` if there is no value stored under that key)
- `insert!` [inserts information in a table] - inserts a value in a table under specified key

#### Two-dimensional tables

In a two-dimensional table, each value is indexed by two keys. Such a table can be constructed as a one-dimensional table in which each key identifies a subtable.

The subtables don't need a special header symbol, since the key that identifies the subtable serves this purpose.

Operations' features (in comparison to one-dimensional):
- `lookup` - when an item is being looked up, the first key is used to identify the correct subtable. Then the second key is used to identify the record within the subtable.
- `insert!` - the operation finds out if there is a subtable stored under the first key. If not, a new subtable containing the single record (key-2, value) is built and inserted into the table under the first key. If a subtable already exists for the first key, the new record is inserted into this subtable, using the insertion method for one-dimensional tables.

#### Creating tables

The `lookup` and `insert!` operations might take the table as an argument. This enables us to use programs that access more than one table.

Another way to deal with multiple tables is to have separate `lookup` and `insert!` procedures for each table. It can be done by representing a table procedurally, as an object that maintains an internal table as part of its local state. When sent an appropriate message, this "table object" supplies the procedure with which to operate on the internal table.

#### Memoization

_Memoization_ (also called _tabulation_) is a technique that enables a procedure to record, in a local table, values that have previously been computed.

This technique can make a vast difference in the performance of a program. A memoized procedure maintains a table in which values of previous calls are stored using as keys the arguments that produced the values. When the memoized procedure is asked to compute a value, it first checks the table to see if the value is already there and, if so, just returns that value. Otherwise, it computes the new value in the ordinary way and stores this in the table.

### A Simulator for Digital Circuits

Digital systems are constructed by interconnecting simple elements. Although the behavior of these individual elements is simple, networks of them can have very complex behavior.

Computer simulation of proposed circuit designs is an important tool used by digital systems engineers. In this section we design a system for performing digital logic simulations. This system typifies a kind of program called an _event-driven simulation_, in which actions ("events") trigger further events that happen at a later time, which in turn trigger more events, and so on.

If we adopt the general perspective on languages with which we approached the study of Lisp in section 1.1, we can say that
- the primitive function boxes form the primitive elements of the language,
- wiring boxes together provides a means of combination,
- specifying wiring patterns as procedures serves as a means of abstraction.

### Propagation of Constraints

Computer programs are traditionally organized as one-directional computations, which perform operations on prespecified arguments to produce desired outputs. On the other hand, we often model systems in terms of relations among quantities.

This section sketches the design of a language that enables us to work in terms of relations themselves.

The primitive elements of the language are:
- primitive constraints
- constraint networks
- connectors

Primitive constraints state that certain relations hold between quantities.

For example,
- `(adder a b c)` specifies that the quantities `a`, `b`, and `c` must be related by the equation `a + b = c`
- `(multiplier x y z)` expresses the constraint `xy=z`
- `(constant 3.14 x)` says that the value of `x` must be `3.14`

The language provides a means of combining primitive constraints in order to express more complex relations. Constraints are combined by constructing constraint networks, in which constraints are joined by connectors. A connector is an object that "holds" a value that may participate in one or more constraints.

For example, we know that the relationship between Fahrenheit and Celsius temperatures is

```
9C = 5(F - 32)
```

Such a constraint can be thought of as a network consisting of primitive adder, multiplier, and constant constraints.

## Resources

- [SICP - Modeling with Mutable Data](https://mitpress.mit.edu/sites/default/files/sicp/full-text/book/book-Z-H-22.html)
