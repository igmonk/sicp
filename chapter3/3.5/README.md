# 3.5 Streams

In this section, we explore an alternative approach to modeling state, based on data structures called _streams_. Streams can mitigate some of the complexity of modeling state.

If time is measured in discrete steps, then we can model a time function as a (possibly infinite) sequence. This section shows how to model change in terms of sequences that represent the time histories of the systems being modeled. To accomplish this, we introduce new data structures called _streams_.

From an abstract point of view, a stream is simply a sequence. However, the straightforward implementation of streams as lists (as in section 2.2.1) doesn't fully reveal the power of stream processing. As an alternative, we introduce the technique of _delayed evaluation_, which enables us to represent very large (even infinite) sequences as streams.

Stream processing lets us model systems that have state without ever using assignment or mutable data. This has important implications, both theoretical and practical, because we can build models that avoid the drawbacks inherent in introducing assignment. On the other hand, the stream framework raises difficulties of its own, and the question of which modeling technique leads to more modular and more easily maintained systems remains open.

## Streams Are Delayed Lists

As we saw in section `2.2.3`, sequences can serve as standard interfaces for combining program modules. We formulated powerful abstractions for manipulating sequences, such as `map`, `filter`, and `accumulate`, that capture a wide variety of operations in a manner that is both succinct and elegant.

Unfortunately, if we represent sequences as lists, this elegance is bought at the price of severe inefficiency with respect to both the time and space required by our computations. When we represent manipulations on sequences as transformations of lists, our programs must construct and copy data structures (which may be huge) at every step of a process.

Streams are a clever idea that allows one to use sequence manipulations without incurring the costs of manipulating sequences as lists. With streams we can achieve the best of both worlds:

```
We can formulate programs elegantly as sequence manipulations, while attaining the efficiency of incremental computation.
```

The basic idea is to arrange to construct a stream only partially, and to pass the partial construction to the program that consumes the stream. If the consumer attempts to access a part of the stream that has not yet been constructed, the stream will automatically construct just enough more of itself to produce the required part, thus preserving the illusion that the entire stream exists.

Although the programs are written as if complete sequences are processed, the stream implementation is designed to automatically and transparently interleave the __construction__ of the stream with its __use__.

In general, we can think of delayed evaluation as "demand-driven" programming, whereby each stage in the stream process is activated only enough to satisfy the next stage. The actual order of events in the computation is decoupled from the apparent structure of our procedures. Procedures are written as if the streams existed "all at once" when, in reality, the computation is performed incrementally, as in traditional programming styles.

Delayed evaluation, which is the key to making streams practical, was inherent in Algol 60's _call-by-name_ parameter-passing method.

The memoizing optimization is also known as _call-by-need_.

## Infinite Streams

We can use streams to represent sequences that are infinitely long. In any given time we can examine only a finite portion of such a stream. Thus, our programs will never know that the entire infinite stream is not there.

## Exploiting the Stream Paradigm

Streams with delayed evaluation can be a powerful modeling tool, providing many of the benefits of local state and assignment. 

The stream approach allows us to build systems with different module boundaries than systems organized around assignment to state variables. For example, we can think of an entire time series (or signal) as a focus of interest, rather than the values of the state variables at individual moments. This makes it convenient to combine and compare components of state from different moments.

Streams give us an opportunity to do some interesting tricks. For example, we can transform a stream with a _sequence accelerator_ that converts a sequence of approximations to a new sequence that converges to the same value as the original, only faster. See [Series Acceleration](https://en.wikipedia.org/wiki/Series_acceleration).

Even better, we can accelerate the accelerated sequence, and recursively accelerate that, and so on. Namely, we create a stream of streams (a structure we'll call a _tableau_) in which each stream is the transform of the preceding one:

```scheme
(define (make-tableau transform s)
  (cons-stream s
               (make-tableau transform
                             (transform s))))
```

Finally, the accelerated sequence is formed by taking the first term in each row of the tableau:

```scheme
(define (accelerated-sequence transform s)
  (stream-map stream-car
              (make-tableau transform s)))
```

### Streams as signals

Streams can be used to model signal-processing systems in a very direct way, representing the values of a signal at successive time intervals as consecutive elements of a stream.

## Streams and Delayed Evaluation

We can use streams to model signal-processing systems that contain feedback loops. The feedback loop for the adder shown in figure 3.32 is modeled by the fact that `integral`'s internal stream `int` is defined in terms of itself:

```scheme
(define int
  (cons-stream initial-value
               (add-streams (scale-stream integrand dt)
                            int)))
```

The interpreter's ability to deal with such an implicit definition depends on the `delay` that is incorporated into `cons-stream`. Without this delay, the interpreter could not construct `int` before evaluating both arguments to `cons-stream`, which would require that `int` already be defined.

In general, `delay` is crucial for using streams to model signal-processing systems that contain loops. Without `delay`, our models would have to be formulated so that the inputs to any signal-processing component would be fully evaluated before the output could be produced. This would outlaw loops.

In systems with loops (where components/streams are defined in terms of each other), _delayed arguments_ can be used.

## Modularity of Functional Programs and Modularity of Objects

One of the major benefits of introducing assignment is that we can increase the modularity of our systems by encapsulating, or "hiding", parts of the state of a large system within local variables. Stream models can provide an equivalent modularity without the use of assignment.

### A functional-programming view of time

We can model a changing quantity, such as the local state of some object, using a stream that represents the time history of successive states. In essence, we represent time explicitly, using streams, so that we decouple time in our simulated world from the sequence of events that take place during evaluation.

From the point of view of one part of a complex process, the other parts appear to change with time. They have hidden time-varying local state. If we wish to write programs that model this kind of natural decomposition in our world (as we see it from our viewpoint as a part of that world) with structures in our computer, we make computational objects that are not functional -- they must change with time.

We model state with local state variables, and we model the changes of state with assignments to those variables. By doing this we make the time of execution of a computation model time in the world that we are part of, and thus we get "objects" in our computer.

We can model the world as a collection of separate, time-bound, interacting objects with state, or we can model the world as a single, timeless, stateless unity. Each view has powerful advantages, but neither view alone is completely satisfactory. A grand unification has yet to emerge.

The object model approximates the world by dividing it into separate pieces. The functional model does not modularize along object boundaries. The object model is useful when the unshared state of the "objects" is much larger than the state that they share. An example of a place where the object viewpoint fails is quantum mechanics, where thinking of things as individual particles leads to paradoxes and confusions. Unifying the object view with the functional view may have little to do with programming, but rather with fundamental epistemological issues.

## Resources

- [SICP - Streams](https://mitpress.mit.edu/sites/default/files/sicp/full-text/book/book-Z-H-24.html)
