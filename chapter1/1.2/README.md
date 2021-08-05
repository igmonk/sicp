# 1.2 Procedures and the Processes They Generate

A procedure is a pattern for the _local evolution_ of a computational process. It specifies how each stage of the process is built upon the previous stage. We would like to be able to make statements about the overall, or _global_, behavior of a process whose local evolution has been specified by a procedure.

## Linear Recursion and Iteration

The type of process, characterized by a chain of _deferred operations_, is called a _recursive process_. Carrying out this process requires that the interpreter keep track of the operations to be performed later on.

If the lengh of the chain of deferred operations, and hence the amount of information needed to keep track of it, grows linearly, such a process is called a _linear recursive process_.

An _iterative process_ is one whose state can be summarized by a fixed number of _state variables_, together with a fixed rule that describes how the state variables should be updated as the process moves from state to state and an (optional) end test that specifies conditions under which the process should terminate.

If the number of steps required grows linearly, such a process is called a _linear iterative process_.

In contrasting iteration and recursion, we must be careful not to confuse the notion of a recursive _process_ with the notion of a recursive _procedure_. When we describe a procedure as recursive, we are referring to the syntactic fact that the procedure definition refers (either directly or indirectly) to the procedure itself. But when we describe a process as following a pattern that is, say, linearly recursive, we are speaking about how the process evolves, not about the syntax of how a procedure is written.

One reason that the distinction between process and procedure may be confusing is that most implementations of common languages (including Ada, Pascal, and C) are designed in such a way that the interpretation of any recursive procedure consumes an amount of memory that grows with the number of procedure calls, even when the process described is, in principle, iterative. As a consequence, these languages can describe iterative processes only by resorting to special-purpose _looping constructs_ such as _do_, _repeat_, _until_, _for_, and _while_.

The implementation of Scheme does not share this defect. It will execute an iterative process in constant space, even if the iterative process is described by a recursive procedure. An implementation with this property is called _tail-recursive_. With a tail-recursive implementation, iteration can be expressed using the ordinary procedure call mechanism, so that special iteration constructs are useful only as syntactic sugar.

## Tree Recursion

Another common pattern of computation is called _tree recursion_.

As an example, consider computing the sequence of Fibonacci numbers, in which each number is the sum of the preceding two:

```
0, 1, 1, 2, 3, 5, 8, 13, 21, ...
```

A recursive procedure for computing Fibonacci numbers:

```scheme
(define (fib n)
  (cond ((= n 0) 0)
	    ((= n 1) 1)
	    (else (+ (fib (- n 1))
		         (fib (- n 2))))))
```

The evolved process looks like a tree.
The branches split into two at each level (except at the bottom); this reflects the fact that the 'fib' procedure calls itself twice each time it is invoked.

This procedure is instructive as a prototypical tree recursion, but it is a terrible way to compute Fibonacci numbers because it does so much redundant computation (the entire computation of `(fib 3)` - almost half the work - is duplicated).

The process uses a number of steps that grows exponentially with the input.
The space required grows only linearly with the input, because we need keep track only of which nodes are above us in the tree at any point in the computation.

In general, the `number of steps` required by a tree-recursive process will be proportional to the number of nodes in the tree, while the `space` required will be proportional to the maximum depth of the tree.

An iterative process for computing Fibonacci numbers can be reformulated. The idea is to use a pair of integers _a_ and _b_, initialized to `Fib(1) = 1` and `Fib(0) = 0`, and to repeatedly apply the simultaneous transformations:

```
a <- a + b
b <- a
```

After applying this transformation _n_ times, _a_ and _b_ will be equal, respectively, to `Fib(n+1)` and `Fib(n)`. Thus, Fibonacci numbers can be computed iteratively using the procedure:

```scheme
(define (fib-iter a b n)
  (if (> n 0)
      (fib-iter (+ a b) a (- n 1))
      b))

(define (fib n)
  (fib-iter 1 0 n))
```

This second method for computing `Fib(n)` is a linear iteration - the number of steps required is linear in _n_.

To formulate the iterative algorithm required noticing that the computation could be recast as an iteration with three state variables.

The observation that a tree-recursive process may be highly inefficient but often easy to specify and understand has led people to propose that one could get the best of both worlds by designing a `smart compiler` that could transform tree-recursive procedures into more efficient procedures that compute the same result.

One approach to coping with redundant computations is to arrange matters so that we automatically construct a table of values as they are computed. Each time we are asked to apply the procedure to some argument, we first look to see if the value is already stored in the table, in which case we avoid performing the redundant computation.

This strategy, known as _tabulation_ or _memoization_, can be implemented in a straightforward way. Tabulation can sometimes be used to transform processes that require an exponential number of steps into processes whose space and time requirements grow linearly with the input.

## Orders of Growth

Processes can differ considerably in the rates at which they consume computational resources. One convenient way to describe this difference is to use the notion of _order of growth_ to obtain a gross measure of the resources required by a process as the inputs become larger.

In general, there are a number of properties of the problem with respect to which it will be desirable to analyze a given process (the number of internal storage registers used, the number of elementary machine operations performed, and so on). In computers that do only a fixed number of operations at a time, the time required will be proportional to the number of elementary machine operations performed.

Orders of growth provide only a crude description of the behaviour of a process. For example, a process requiring `n^2` steps and a process requiring `1000n^2` steps and a process requiring `3n^2+10n+17` steps all have _&theta;(n^2)_ order of growth. On the other hand, order of growth provides a useful indication of how we may expect the behavior of the process to change as we change the size of the problem.

## Resources

- [SICP - Procedures and the Processes They Generate](https://mitpress.mit.edu/sites/default/files/sicp/full-text/book/book-Z-H-11.html)
- [Analysis of Algorithms](https://en.wikipedia.org/wiki/Analysis_of_algorithms)
- [Big O notation](https://en.wikipedia.org/wiki/Big_O_notation)
