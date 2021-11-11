# 3.1 Assignment and Local State

We ordinarily view the world as populated by independent objects, each of which has a state that changes over time. An object is said to 'have state' if its behavior is influenced by its history. We can characterize an object's state by one or more state variables, which among them maintain enough information about history to determine the object's current behavior.

Each computational object must have its own _local state variables_ describing the actual object's state. Since the states of objects in the system being modeled change over time, the state variables of the corresponding computational objects must also change.

If we choose to model the flow of time in the system by the elapsed time in the computer, then we must have a way to construct computational objects whose behaviors change as our programs run. In particular, if we wish to model state variables by ordinary symbolic names in the PL, then the language must provide an _assignment operator_ to enable us to change the value associated with a name.

## Local State Variables

Until now, all our procedures could be viewed as specifications for computing mathematical functions. A call to a procedure computed the value of the function applied to the given arguments, and two calls to the same procedure with the same arguments always produced the same result (except the random-number generator in section 1.2.6 and the operation/type tables introduced in section 2.4.3).

## The Benefits of Introducing Assignment

A general fenomenon: from the point of view of one part of a complex process, the other parts appear to change with time. They have hidden time-varying local state.

If we wish to write computer programs whose structure reflects this decomposition, we make computational objects (such as bank accounts and random-number generators) whose behavior changes with time. We model state with local state variables, and we model the changes of state with assignments to those variables.

By introducing assignment and the technique of hiding state in local variables, we are able to structure systems in a more modular fashion than if all state had to be manipulated explicitly, by passing additional parameters. Unfortunately, the story is not so simple.

## The Cost of Introducing Assignment

The advantage of modelling objects that have local state by means of `set!` comes at a price. Our PL can no longer be interpreted in terms of the substitution model of procedure application (introduced in section `1.1.5`).

As soon as we introduce `set!` and the idea that the value of a variable can change, a variable can no longer be simply a name. Now a variable somehow refers to a place where a value can be stored, and the value stored at this place can change.

So long as we do not use assignments, two evaluations of the same procedure with the same arguments will produce the same result, so that procedures can be viewed as computing mathematical functions. Programming without any use of assignments (the first two chapters of the book) is accordingly known as _functional programming_.

### Sameness and change

A language that supports the concept that 'equals can be substituted for equals' in an expresssion without changing the value of the expression is said to be _referentially transparent_. Referential transparency is violated when we include `set!` in our computer language. This makes it tricky to determine when we can simplify expressions by substituting equivalent expressions. Consequently, reasoning about programs that use assignment becomes drastically more difficult.

Once we forgo referential transparency, the notion of what it means for computational objects to be 'the same' becomes difficult to capture in a formal way. Indeed, the meaning of 'same' in the real world that our programs model is hardly clear in itself.

In general, we can determine that two apparently identical objects are indeed 'the same one' only by modifying one object and then observing whether the other object has changed in the same way. But how can we tell if an object has 'changed' other than by observing the 'same' object twice and seeing whether some property of the object differs from one observation to the next? Thus, we cannot determine 'change' without some _a priori_ notion of 'sameness', and we cannot determine sameness without observing the effects of change.

The phenomenon of a single computational object being accessed by more than one name is known as _aliasing_. Bugs can occur in our programs if we forget that a change to an object may also, as a 'side effect', change a 'different' object because the two 'different' objects are actually a single object appearing under different aliases.

In general, so long as we never modify data objects, we can regard a compound data object to be precisely the totality of its pieces. But this view is no longer valid in the presence of change, where a compound data object has an 'identity' that is something different from the pieces of which it is composed.

### Pitfalls of imperative programming

In contrast to functional programming, programming that makes extensive use of assignment is known as _imperative programming_. In addition to raising complications about computational models, programs written in imperative style are susceptible to bugs that cannot occur in functional programs.

In general, programming with assignment forces us to carefully consider the relative orders of the assignments to make sure that each statement is using the correct version of the variables that have been changed. This issue simply does not arise in functional programs. The complexity of imperative programs becomes even worse if we consider applications in which several processes execute concurrently.

## Resources

- [SICP - Assignment and Local State](https://mitpress.mit.edu/sites/default/files/sicp/full-text/book/book-Z-H-20.html)
