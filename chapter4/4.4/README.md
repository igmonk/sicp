# 4.4 Logic Programming

Computer science deals with imperative (how to) knowledge, whereas mathematics deals with declarative (what is) knowledge.

Programming languages require that the programmer express knowledge in a form that indicates the step-by-step methods for solving particular problems. On the other hand, high-level languages provide, as part of the language implementation, a substantial amount of methodological knowledge that frees the user from concern with numerous details of how a specified computation will progress.

Most programming languages are strongly biased toward unidirectional computations (computations with well-defined inputs and outputs).

There are, however, radically different programming languages that relax this bias:
- a constraint system (section `3.3.5`), where the direction and the order of computation are not so well specified;
- the nondeterministic program evaluator (section `4.3`), where expressions can have more than one value, thereby the computation is dealing with relations rather than with single-valued functions;
- logic programming, where a relational vision of programming is combined with a powerful kind of symbolic pattern matching called _unification_.

Part of the power of logic programming comes from the fact that a single "what is" fact can be used to solve a number of different problems that would have different "how to" components.

Sometimes, however, "what is" information gives no clue "how to" compute an answer. For example, consider the problem of computing the `y` such that `y^2 = x`.

The ideas and technology of implementing interpreters (sections `4.1`, `4.2` and `4.3`) are applied to discuss an interpreter for a logic programming language. The language is called the _query language_, because it is useful for retrieving information from data bases by formulating _queries_, or questions, expressed in the language.

As with any other language, it is convenient to describe the query language in terms of the same general framework:
- primitive elements
- means of combination
- means of abstraction

## Deductive Information Retrieval

Logic programming excels in providing interfaces to data bases for information retrieval.

The language provides pattern-directed access to the information in a data base and can also take advantage of general rules in order to make logical deductions.

### Assertions

Assertions are records in a data base. For ex.:

```scheme
(address (Bitdiddle Ben) (Slumerville (Ridge Road) 10))
(job (Bitdiddle Ben) (computer wizard))
(salary (Bitdiddle Ben) 60000)
(supervisor (Bitdiddle Ben) (Warbucks Oliver))
```

### Simple queries

Simple queries are the primitive elements of the language that specify that one is looking for entries in a data base that match a certain _pattern_. Symbols preceded by a question mark are _pattern variables_. For ex.:

```scheme
(job ?x (computer programmer)) ; find all computer programmers
(address ?x ?y) ; list all the employees' addresses
(supervisor ?x ?x) ; find all people who supervise themselves
```

### Compound queries

Compound queries are the means of combination of the language that allow for compound operations. Compound queries mirror the means of combination used in forming logical expressions: `and`, `or`, and `not`.

In general,

```scheme
(and <query1> <query2> ... <queryn>)
```

is satisfied by all sets of values for the pattern variables that simultaneously satisfy `<query1> ... <queryn>`.


```scheme
(or <query1> <query2> ... <queryn>)
```

is satisfied by all sets of values for the pattern variables that satisfy at least one of `<query1> ... <queryn>`.

```scheme
(not <query1>)
```

is satisfied by all assignments to the pattern variables that do not satisfy `<query1>`.


For ex.:

```scheme
;; find the addresses of all computer programmers
(and (job ?person (computer programmer))
     (address ?person ?where))

;; find all employees supervised by Ben or Alyssa
(or (supervisor ?x (Bitdiddle Ben))
    (supervisor ?x (Hacker Alyssa P)))

;; find all people supervised by Ben who are not computer programmers
(and (supervisor ?x (Bitdiddle Ben))
     (not (job ?x (computer programmer))))

;; find all people whose salary is greater than 30,000
(and (salary ?person ?amount)
     (lisp-value > ?amount 30000))
```

### Rules

Rules are the means of abstracting queries.

The general form of a rule is

```scheme
(rule <conclusion> <body>)
```

where `<conclusion>` is a pattern and `<body>` is any query.

A rule can be thought of as representing a large (even infinite) set of assertions, namely all instantiations of the rule conclusion with variable assignments that satisfy the rule body.

Rules are allowed to be without bodies, as in `same` below, which will be interpreted to mean that the rule conclusion is satisfied by any values of the variables.

For ex., the following rule specifies that two people live near each other if they live in the same town. The final `not` clause prevents the rule from saying that all people live near themselves:

```scheme
(rule (lives-near ?person-1 ?person-2)
      (and (address ?person-1 (?town . ?rest-1))
           (address ?person-2 (?town . ?rest-2))
           (not (same ?person-1 ?person-2))))
```

The `same` relation is defined by a very simple rule:

```scheme
(rule (same ?x ?x))
```

Although using the same pattern variable in two parts of a query forces the same value to appear in both places, using different pattern variables does not force different values to appear.

### Logic as programs

A rule can be regarded as a kind of logical implication: _If_ an assignment of values to pattern variables satisfies the body, _then_ it satisfies the conclusion. Consequently, we can regard the query language as having the ability to perform _logical deductions_ based upon the rules.

The query system may seem to exhibit quite a bit of intelligence in using the rules to deduce the answer to a query. Actually, the system is following a well-determined algorithm in unraveling the rules. Unfortunately, the general methods may break down in more complex cases.

## How the Query System Works

The query evaluator must perform some kind of search in order to match queries against facts and rules in the data base. One way to do this would be to implement the query system as a nondeterministic program, using the `amb` evaluator of section `4.3`. Another possibility is to manage the search with the aid of streams. The second approach is taken.

The query system is organized around two central operations called _pattern matching_ and _unification_.

### Pattern matching

A _pattern matcher_ is a program that tests whether some datum fits a specified pattern. For ex., the data list `((a b) c (a b))` matches the pattern `(?x c ?x)` with the pattern variable `?x` bound to `(a b)`. 

The pattern matcher used by the query system takes as inputs a pattern, a datum, and a _frame_ that specifies bindings for various pattern variables. It checks whether the datum matches the pattern in a way that is consistent with the bindings already in the frame. If so, it returns the given frame augmented by any bindings that may have been determined by the match. Otherwise, it indicates that the match has failed.

For example, using the pattern `(?x ?y ?x)` to match `(a b a)` given an empty frame will return a frame specifying that `?x` is bound to `a` and `?y` is bound to `b`.

The pattern matcher is all the mechanism that is needed to process simple queries that don't involve rules.

For instance, to process the query

```scheme
(job ?x (computer programmer))
```

all assertions in the data base are scanned and from them selected those that match the pattern with respect to an initially empty frame. For each found match, the frame returned by the match is used to instantiate the pattern with a value for `?x`.

### Streams of frames

The testing of patterns against frames is organized through the use of streams. Given a single frame, the matching process runs through the data-base entries one by one. For each data-base entry, the matcher generates either a special symbol indicating that the match has failed or an extension to the frame.

The results for all the data-base entries are collected into a stream, which is passed through a filter to weed out the failures. The result is a stream of all the frames that extend the given frame via a match to some assertion in the data base.

Because matching is generally very expensive, we would like to avoid applying the full matcher to every element of the data base. This is usually arranged by breaking up the process into a fast, coarse match and the final match. The coarse match filters the data base to produce a small set of candidates for the final match.

The data base can be arranged so that some of the work of coarse matching can be done when the data base is constructed rather then when we want to select the candidates. This is called _indexing_ the data base.

A query takes as input stream of frames and performs the matching operation for every frame in the stream:

```
  input stream   ┌-------------┐  output stream of frames,
  of frames      |    query    |  filtered and extended
---------------->|             |--------------------------->
                 | (job ?x ?y) |
                 └-------------┘
                        ↑
                        |
                 stream of assertions
                 from data base
```

That is, for each frame in the input stream, the query generates a new stream consisting of all extensions to that frame by matches to assertions in the data base. All these streams are then combined to form one huge stream, which contains all possible extensions of every frame in the input stream. This stream is the output of the query.

To answer a simple query, we use the query with an input stream consisting of a single empty frame. The resulting output stream contains all extensions to the empty frame (that is, all answers to our query).

### Compound queries

The processing of compound queries makes use of the ability of the matcher to demand that a match be consistent with a specified frame.

#### And

The `and` of two queries can be viewed as a series combination of the two component queries. The frames that pass through the first query filter are filtered and further extended by the second query:

```
                 ┌----------------┐
  input stream   |   (and A B)    |  output stream
  of frames      |                |  of frames
-----------------|--> A ----> B --|----------------->
                 |    ↑       ↑   |
                 |    └-------┘   |
                 └--------|-------┘
                          |
                          |
                      data base
```

#### Or

The `or` combination of two queries is produced by operating on the stream of frames in parallel and merging the results:

```
                 ┌---------------------┐
                 |      (or A B)       |
                 |                     |
  input stream   | ┌-> A --------┐     |  output stream
  of frames      | |   ↑         ↓     |  of frames
-----------------|-|   |       merge --|----------------->
                 | |   |         ↑     |
                 | └---|---> B --┘     |
                 |     |     ↑         |
                 |     └-----┘         |
                 └--------|------------┘
                          |
                      date base
```

#### Not

From the stream-of-frames viewpoint, the `not` of some query acts as a filter that removes all frames for which the query can be satisfied.

#### lisp-value

The `lisp-value` special form is implemented as a similar filter on frame streams. We use each frame in the stream to instantiate any variables in the pattern, then apply the Lisp predicate. We remove from the input stream all frames for which the predicate fails.

### Unification

In order to handle rules in the query language, we must be able to find the rules whose conclusions match a given query pattern. Rule conclusions are like assertions except that they can contain variables, so we will need a generalization of pattern matching - called _unification_ - in which both the "pattern" and the "datum" may contain variables.

A unifier takes two patterns, each containing constants and variables, and determines whether it is possible to assign values to the variables that will make the two patterns equal. If so, it returns a frame containing these bindings.

For example, unifying `(?x a ?y)` and `(?y ?z a)` will specify a frame in which `?x`, `?y`, and `?z` must all be bound to `a`.

On the other hand, unifying `(?x ?y a)` and `(?x b ?y)` will fail, because there is no value for `?y` that can make the two patterns equal.

With complex patterns, performing unification may seem to require deduction. To unify `(?x ?x)` and `((a ?y c) (a b ?z))`, the algorithm must infer that `?x` should be `(a b c)`, `?y` should be `b`, and `?z` should be `c`.

*In one-sided pattern matching, all the equations that contain pattern variables are explicit and already solved for the unknown (the pattern variable).*

The unifier used in the query system, like the pattern matcher, takes a frame as input and performs unifications that are consistent with this frame.

In a successful pattern match, all pattern variables become bound, and the values to which they are bound contain only constants. This is also true of all the examples of unification we have seen so far. In general, however, a successful unification may not completely determine the variable values; some variables may remain unbound and others may be bound to values that contain variables.

### Applying rules

Unification is the key to the component of the query system that makes inferences from rules.

To process a query the ordinary pattern-match procedure is used to see if there are any assertions in the data base that match this pattern. If there are no any, the following method to apply a rule is used:
- attempt to unify the query pattern with the conclusion of each rule, which results in an extension of the original frame that specifies bindings for some of the pattern variables
- relative to the extended frame, evaluate the (compound) query given by the body of the rule

Notice how similar this is to the method for applying a procedure in the `eval`/`apply` evaluator for Lisp:
- bind the procedure's parameters to its arguments to form a frame that extends the original procedure environment
- relative to the extended environment, evaluate the expression formed by the body of the procedure

Just as procedure definitions are the means of abstraction in Lisp, rule definitions are the means of abstraction in the query language. In each case, we unwind the abstraction by creating appropriate bindings and evaluating the rule or procedure body relative to these.

An example of applying a rule. Consider the following query:

```scheme
(lives-near ?x (Hacker Alyssa P))
```

The ordinary pattern-match procedure shows there are no assertions in the data base that match this pattern. The next step is to attempt to unify the query pattern with the conclusion of each rule. The pattern unifies with the conclusion of the rule

```scheme
(rule (lives-near ?person-1 ?person-2)
      (and (address ?person-1 (?town . ?rest-1))
           (address ?person-2 (?town . ?rest-2))
           (not (same ?person-1 ?person-2))))
```

resulting in a frame specifying that `?person-2` is bound to `(Hacker Alyssa P)` and that `?x` should be bound to `?person-1`. Now, relative to this frame, evaluate the compound query given by the body of the rule. Successful matches will extend this frame by providing a binding for `?person-1`, and consequently a value for `?x`, which can be used to instantiate the original query pattern.

### Simple queries

Given the query pattern and a stream of frames, two streams are produced for each frame in the input stream:
- a stream of extended frames obtained by matching the pattern against all assertions in the data base (using the pattern matcher)
- a stream of extended frames obtained by applying all possible rules (using the unifier)

Appending these two streams produces a stream that consists of all the ways that the given pattern can be satisfied consistent with the original frame.

### The query evaluator and the driver loop

The driver loop reads queries from the terminal. For each query, it calls `qeval` with the query and a stream that consists of a single empty frame. This will produce the stream of all possible matches (all possible extensions to the empty frame). For each frame in the resulting stream, it instantiates the original query using the values of the variables found in the frame. This stream of instantiated queries is then printed.

The driver also checks for the special command `assert!`, which signals that the input is not a query but rather an assertion or rule to be added to the data base.

For instance,

```scheme
(assert! (job (Bitdiddle Ben) (computer wizard)))
(assert! (rule (wheel ?person)
               (and (supervisor ?middle-manager ?person)
                    (supervisor ?x ?middle-manager))))
```

## Is Logic Programming Mathematical Logic?

The identification of the query language with mathematical logic is not really valid, because the query language provides a control structure that interprets the logical statements procedurally, which can often be taken advantage of.

For example, to find all of the supervisors of programmers we could formulate a query in either of two logically equivalent forms:

```scheme
(and (job ?x (computer programmer))
     (supervisor ?x ?y))
```

or

```scheme
(and (supervisor ?x ?y)
     (job ?x (computer programmer)))
```

If a company has many more supervisors than programmers, it is better to use the first form rather than the second because the data base must be scanned for each intermediate result produced by the first clause of the `and`.

The aim of logic programming is to provide the programmer with techniques for decomposing a computational problem into two separate problems: "what" is to be computed, and "how" this should be computed.

This is accomplished by selecting a subset of the statements of mathematical logic that is powerful enough to be able to describe anything one might want to compute, yet weak enough to have a controllable procedural interpretation.

Control ("how" to compute) is effected by using the order of evaluation of the language. At the same time, we should be able to view the result of the computation ("what" to compute) as a simple consequence of the laws of logic.

An assertion represents a simple fact (an atomic proposition). A rule represents the implication that the rule conclusion holds for those cases where the rule body holds. A rule has a natural procedural interpretation: To establish the conclusion of the rule, establish the body of the rule. Rules, therefore, specify computations.

## Resources

- [SICP - Logic Programming](https://mitpress.mit.edu/sites/default/files/sicp/full-text/book/book-Z-H-29.html)
- [CWA - Closed-world assumption](https://en.wikipedia.org/wiki/Closed-world_assumption)
- [OWA - Open-world assumption](https://en.wikipedia.org/wiki/Open-world_assumption)
