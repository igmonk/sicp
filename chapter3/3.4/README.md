# 3.4 Concurrency: Time Is of the Essence

The power of computational objects with local state as tools for modeling comes at a price: the loss of referential transparency, giving rise to a thicket of questions about sameness and change, and the need to abandon the substitution model of evaluation in favor of the more intricate environment model.

The central issue lurking beneath the complexity of state, sameness, and change is that by introducing assignment we are forced to admit _time_ into our computational models. Before we introduced assignment, all our programs were timeless, in the sense that any expression that has a value always has the same value.

The execution of assignment statements delineates _moments in time_ when values change. The result of evaluating an expression depends not only on the expression itself, but also on whether the evaluation occurs before or after these moments. Building models in terms of computational objects with local state forces us to confront time as an essential concept in programming.

We can go further in structuring computational models to match our perception of the physical world. Objects in the world do not change one at a time in sequence. Rather we perceive them as acting _concurrently_ - all at once. So it is often natural to model systems as collections of computational processes that execute concurrently.

Just as we can make our programs modular by organizing models in terms of objects with separate local state, it is often appropriate to divide computational models into parts that evolve separately and concurrently. Even if the programs are to be executed on a sequential computer, the practice of writing programs as if they were to be executed concurrently forces the programmer to avoid inessential timing constraints and thus makes programs more modular.

In addition to making programs more modular, concurrent computation can provide a speed advantage over sequential computation. If it is possible to decompose a problem into pieces that are relatively independent and need to communicate only rarely, it may be possible to allocate pieces to separate computing processors, producing a speed advantage proportional to the number of processors available.

### The Nature of Time in Concurrent Systems

On the surface, time seems straightforward. It is an ordering imposed on events. For any events _A_ and _B_, either _A_ occurs before _B_, _A_ and _B_ are simultaneous, or _A_ occurs after _B_.

To quote some graffiti seen on a Cambridge building wall:

```
Time is a device that was invented to keep everything from happening at once.
```

#### Correct behaviour of concurrent programs

The root of the complexity lies in the assignments to variables that are shared among the different processes. We must be careful in writing programs that use `set!`, because the results of a computation depend on the order in which the assignments occur.

With concurrent processes we must be especially careful about assignments, because we may not be able to control the order of the assignments made by the different processes. If several such changes might be made concurrently we need some way to ensure that our system behaves correctly. To make concurrent programs behave correctly, we may have to place some restrictions on concurrent execution.

One possible restriction on concurrency would stipulate that no two operations that change any shared state variables can occur at the same time. This is an extremely stringent requirement, both inefficient and overly conservative.

A less stringent restriction on concurrency would ensure that a concurrent system produces the same result as if the processes had run sequentially in some order. There are two important aspects to this requirement:
1. It does not require the processes to actually run sequentially, but only to produce results that are the same _as if_ they had run sequentially.
2. There may be more than one possible "correct" result produced by a concurrent program, because we require only that the result be the same as for _some_ sequential order.

There are still weaker requirements for correct execution of concurrent programs. If an algorithm converges to the right answer independently of the order in which the operations are done, there is no need on any restrictions on concurrent use of the shared values.

### Mechanisms for Controlling Concurrency

The difficulty in dealing with concurrent processes is rooted in the need to consider the interleaving of the order of events in the different processes. As programmers designing a system, we would have to consider the effects of each of the possible orderings and check that each behaviour is acceptable. Such an approach rapidly becomes unwieldy as the numbers of processes and events increase.

A more practical approach to the design of concurrent systems is to devise general mechanisms that allow us to constrain the interleaving of concurrent processes so that we can be sure that the program behavior is correct. Many mechanisms have been developed for this purpose. This section describes one of them, the _serializer_.

#### Serializing access to shared state

Serialization implements the following idea:

```
Processes will execute concurrently, but there will be certain collections of procedures that cannot be executed concurrently.
```

More precisely, serialization creates distinguished sets of procedures such that only one execution of a procedure in each serialized set is permitted to happen at a time. If some procedure in the set is being executed, then a process that attempts to execute any procedure in the set will be forced to wait until the first execution has finished.

#### Complexity of using multiple shared resources

While using serializers is relatively straightforward when there is only a single shared resource, concurrent programming can be treacherously difficult when there are multiple shared resources.

#### Implementing serializers

Serializers are implemented in terms of a more primitive synchronization mechanism called a _mutex_ (an abbreviation for _mutual exclusion_). A mutex is an object that supports two operations:
- the mutex can be _acquired_
- the mutex can be _released_

Once a mutex has been acquired, no other acquire operations on that mutex may proceed until the mutex is released.

Each serializer has an associated mutex. Given a procedure `p`, the serializer returns a procedure that acquires the mutex, runs `p`, and then releases the mutex. This ensures that only one of the procedures produced by the serializer can be running at once, which is precisely the serialization property that we need to guarantee.

#### Deadlock

When each process is stalled forever, waiting for the other, the situation is called a _deadlock_. Deadlock is always a danger in systems that provide concurrent access to multiple shared resources.

One way to avoid the deadlock in some situations is to give each shared resource a unique identification number and rewrite serialized access so that a process will always attempt to enter a procedure protecting the lowest-numbered resource first.

Although this method works well for the exchange problem, there are other situations that require more sophisticated deadlock-avoidance techniques, or where deadlock cannot be avoided at all.

Situations where deadlock cannot be avoided require deadlock-recovery methods, which entail having processes "back out" of the deadlocked state and try again. Deadlock-recovery mechanisms are widely used in database management systems.

#### Concurrency, time, and communication

From a fundamental point of view, it's not always clear what is meant by "shared state".

Mechanisms such as `test-and-set!` require processes to examine a global shared flag at arbitrary times. This is problematic and inefficient to implement in modern high-speed processors, where due to optimization techniques such as pipelining and cached memory, the contents of memory may not be in a consistent state at every instant.

In contemporary multiprocessing systems, therefore, the serializer paradigm is being supplanted by new approaches to concurrency control. One such alternative to serialization is called barrier synchronization. The programmer permits concurrent processes to execute as they please, but establishes certain synchronization points ("barriers") through which no process can proceed until all the processes have reached the barrier. Modern processors provide machine instructions that permit programmers to establish synchronization points at places where consistency is required.

The basic phenomenon here is that synchronizing different processes, establishing shared state, or imposing an order on events requires communication among the processes. In essence, any notion of time in concurrency control must be intimately tied to communication. The complexities we encounter in dealing with time and state in our computational models may in fact mirror a fundamental complexity of the physical universe.

## Resources

- [SICP - Concurrency: Time Is of the Essence](https://mitpress.mit.edu/sites/default/files/sicp/full-text/book/book-Z-H-23.html)
