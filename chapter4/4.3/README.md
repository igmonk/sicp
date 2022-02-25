# 4.3 Variations on a Scheme - Nondeterministic Computing

Nondeterministic algorithm (from [wiki](https://en.wikipedia.org/wiki/Nondeterministic_algorithm)):

> In computer programming, a nondeterministic algorithm is an algorithm that, even for the same input, can exhibit different behaviors on different runs, as opposed to a deterministic algorithm.

To support a programming paradigm called _nondeterministic computing_ the Scheme evaluator is extended by building into it a facility to support automatic search.

Nondeterministic computing, like stream processing, is useful for "generate and test" applications. Consider the task of starting with two lists of positive integers and finding a pair of integers - one from the first list and one from the second list - whose sum is prime.

The approach with finite sequence operations (section `2.2.3`) and with infinite streams (section `3.5.3`) was to generate the sequence of all possible pairs and filter these to select the pairs whose sum is prime.

The nondeterministic approach evokes a different image. Imagine simply that we choose (in some way) a number from the first list and a number from the second list and require (using some mechanism) that their sum be prime. This is expressed by following procedure:

```scheme
(define (prime-sum-pair list1 list2)
  (let ((a (an-element-of list1))
        (b (an-element-of list2)))
    (require (prime? (+ a b)))
    (list a b)))
```

The key idea here is that expressions in a nondeterministic language can have more than one possible value. For instance, `an-element-of` might return any element of the given list. The nondeterministic program evaluator will work by automatically choosing a possible value and keeping track of the choice.

If a subsequent requirement is not met, the evaluator will try a different choice, and it will keep trying new choices until the evaluation succeeds, or until we run out of choices.

Just as the lazy evaluator freed the programmer from the details of how values are delayed and forced, the nondeterministic program evaluator will free the programmer from the details of how choices are made.

The nondeterministic program evaluator is called the `amb` evaluator because it is based on a new special form called `amb`.

The above definition of `prime-sum-pair` can be typed at the `amb` evaluator driver loop (along with definitions of `prime?`, `an-element-of`, and `require`) and the procedure runs as follows:

```scheme
;;; Amb-Eval input:
(prime-sum-pair '(1 3 5 8) '(20 35 110))
;;; Starting a new problem
;;; Amb-Eval value:
(3 20)
```

The value returned was obtained after the evaluator repeatedly chose elements from each of the lists, until a successful choice was made.

### Images of time

#### Streams

Stream processing uses lazy evaluation to decouple the time when the stream of possible answers is assembled from the time when the actual stream elements are produced. The evaluator supports the illusion that all the possible answers are laid out before us in a timeless sequence.

#### Nondeterministic evaluation

With nondeterministic evaluation, an expression represents the exploration of a set of possible worlds, each determined by a set of choices. Some of the possible worlds lead to dead ends, while others have useful values.

The nondeterministic program evaluator supports the illusion that time branches, and that our programs have different possible execution histories. When we reach a dead end, we can revisit a previous choice point and proceed along a different branch.

## Amb and Search

To extend Scheme to support nondeterminism, a new special form is introduced - `amb`. The expression `(amb <e1> <e2> ... <en>)` returns the value of one of the `n` expressions `<ei>` "ambiguously".

For example, the expression

```scheme
(list (amb 1 2 3) (amb 'a 'b))
```

can have six possible values:

```
(1 a) (1 b) (2 a) (2 b) (3 a) (3 b)
```

`amb` with a single choice produces an ordinary (single) value.

`amb` with no choices - the expression `(amb)` - is an expression with no acceptable values.

The requirement that a particular predicate expression `p` must be true can be expressed as follows:

```scheme
(define (require p)
  (if (not p) (amb)))
```

With amb and require, we can implement the `an-element-of` procedure used above:

```scheme
(define (an-element-of items)
  (require (not (null? items)))
  (amb (car items) (an-element-of (cdr items))))
```

Abstractly, we can imagine that evaluating an `amb` expression causes time to split into branches, where the computation continues on each branch with one of the possible values of the expression. We say that `amb` represents a _nondeterministic choice point_.

### Systematic search

We might try running the evaluator over and over, making random choices and hoping to find a non-failing value, but it is better to _systematically search_ all possible execution paths.

Automatic search strategies:
- chronological backtracking (depth-first search) [Robert Floyd (1967)]
- dependency-directed backtracking [Sussman and Stallman (1975)]
- truth maintenance [Doyle (1979) and McAllester (1978, 1980)]

The `amb` evaluator that we will develop and work with in this section implements a systematic search as follows:

When the evaluator encounters an application of `amb`, it initially selects the first alternative. This selection may itself lead to a further choice. The evaluator will always initially choose the first alternative at each choice point. If a choice results in a failure, then the evaluator automagically _backtracks_ to the most recent choice point and tries the next alternative.

If it runs out of alternatives at any choice point, the evaluator will back up to the previous choice point and resume from there. This process leads to a search strategy known as _depth-first search_ or _chronological backtracking_.

## Examples of Nondeterministic Programs

The advantage of nondeterministic programming is that we can suppress the details of how search is carried out, thereby expressing our programs at a higher level of abstraction.

The implementation of the `amb` evaluator can be used to:
- solve logic puzzles
- parse natural language (to match the input against some grammatical structure)

## Implementing the `amb` evaluator

The evaluation of an ordinary Scheme expression may:
- return a value
- never terminate
- signal an error

In nondeterministic Scheme the evaluation of an expression may in addition result in the discovery of a dead end, in which case evaluation must backtrack to a previous choice point.

### Execution procedures and continuations

The execution procedures for the ordinary evaluator take one argument: the environment of execution.

In contrast, the execution procedures in the `amb` evaluator take three arguments:
- the environment
- success continuation
- failure continuation

The evaluation of an expression will finish by calling one of these two continuations:
- if the evaluation results in a value, the _success continuation_ is called with that value;
- if the evaluation results in the discovery of a dead end, the _failure continuation_ is called

Constructing and calling appropriate continuations is the mechanism by which the nondeterministic evaluator implements backtracking.

#### Success continuation

The success continuation receives a value and proceeds with the computation. Along with that value, the success continuation is passed another failure continuation, which is to be called subsequently if the use of that value leads to a dead end.

#### Failure continuation

The failure continuation tries another branch of the nondeterministic process. The evaluator picks one of the alternatives and passes this value to the success continuation. Together with this value, the evaluator constructs and passes along a failure continuation that can be called later to choose a different alternative.

## Resources

- [SICP - Variations on a Scheme - Nondeterministic Computing](https://mitpress.mit.edu/sites/default/files/sicp/full-text/book/book-Z-H-28.html)
