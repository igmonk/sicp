# Modularity, Objects, and State

Apart from the ability to construct compound entities by combining primitive procedures and primitive data and to build abstractions, effective program synthesis also requires organizational principles that can guide us in formulating the overall design of a program.

To a large extent, the way we organize a large program is dictated by our perception of the system to be modeled. In this chapter we will investigate two prominent organizational strategies arising from two rather different 'world views' of the structure of systems:
1. The strategy that concentrates on _objects_, viewing a large system as a collection of distinct objects whose behaviors may change over time.
2. The strategy that concentrates on the _streams_ of information that flow in the system, much as an electrical engineer views a signal-processing system.

With objects, we must be concerned with how a computational object can change and yet maintain its identity. This will force us to abandon our old substitution model of computation in favor of a more mechanistic but less theoretically tractable _environment model_ of computation. The difficulties of dealing with objects, change, and identity are a fundamental consequence of the need to grapple with time in our computational models. These difficulties become even greater when we allow the possibility of concurrent execution of programs.

The stream approach can be most fully exploited when we decouple simulated time in our model from the order of the events that take place in the computer during evaluation. We will accomplish this using a technique known as _delayed evaluation_.

## Resources

- [SICP - Modularity, Objects, and State](https://mitpress.mit.edu/sites/default/files/sicp/full-text/book/book-Z-H-19.html)
