# 5.5 Compilation

This section shows how to run Scheme programs on a register machine whose controller is not a Scheme interpreter.

Commercial general-purpose computers are register machines organized around a collection of registers and operations that constitute an efficient and convenient universal set of data paths.

The controller for a general-purpose machine is an interpreter for a register-machine language. This language is called the _native language_ of the machine, or simply _machine language_. Programs written in machine language are sequences of instructions that use the machine's data paths.

There are two common strategies for bridging the gap between higher-level languages and register-machine languages:
- the strategy of interpretation
- the strategy of compilation

#### Interpretation

An interpreter written in the native language of a machine configures the machine to execute programs written in a language (called the _source language_) that may differ from the native language of the machine performing the evaluation. The primitive procedures of the source language are implemented as a library of subroutines written in the native language of the given machine.

A program to be interpreted (called the _source program_) is represented as a data structure. The interpreter traverses this data structure, analyzing the source program. As it does so, it simulates the intended behavior of the source program by calling appropriate primitive subroutines from the library.

#### Compilation

A compiler for a given source language and machine translates a source program into an equivalent program (called the _object program_) written in the machine's native language.

#### Comparison

Compared with interpretation, compilation can provide a great increase in the efficiency of program execution.

On the other hand, an interpreter provides a more powerful environment for interactive program development and debugging, because the source program being executed is available at run time to be examined and modified. In addition, because the entire library of primitives is present, new programs can be constructed and added to the system during debugging.

#### An overview of the compiler

The mechanisms used by the compiler for analyzing expressions are similar to those used by the interpreter.

To make it easy to interface compiled and interpreted code, the compiler is designed to generate code that obeys the same conventions of register usage as the interpreter:
- the environment is kept in the `env` register
- argument lists get accumulated in `argl`
- a procedure to be applied is be in `proc`
- procedures will return their answers in `val`
- the location to which a procedure should return is kept in `continue`

In general, the compiler translates a source program into an object program that performs essentially the same register operations as would the interpreter in evaluating the same source program.

The compiler traverses an expression in the same way the interpreter does. When it encounters a register instruction that the interpreter would perform in evaluating the expression, the compiler does not execute the instruction but instead accumulates it into a sequence. The resulting sequence of instructions will be the object code.

#### Optimizations

The efficiency advantages of compilation over interpretation:
- each time the interpreter evaluates an expression it performs the work of classifying the expression. With a compiler, the expression is analyzed only once, when the instructions sequence is generated at compile time.
- as the interpreter runs, it follows a process that must be applicable to any expression in the language. In contrast, a given segment of compiled code is meant to execute some particular expression.

A compiler can also optimize access to the environment. Having analyzed the code, the compiler can in many cases know in which frame a particular variable will be located and access that frame directly, rather than performing the `lookup-variable-value` search.

## Lexical Addressing

One of the most common optimizations performed by compilers is the optimization of variable lookup.

The `lookup-variable-value` operation of the evaluator machine searches for a variable by comparing it with each variable that is currently bound, working frame by frame outward through the run-time environment. This search can be expensive if the frames are deeply nested or if there are many variables.

Consider the problem of looking up the value of `x` while evaluating the expression `(* x y z)` in an application of the procedure that is returned by

```scheme
(let ((x 3) (y 4))
  (lambda (a b c d e)
    (let ((y (* a b x))
          (z (+ c d x)))
      (* x y z))))
```

Since a `let` expression is just syntactic sugar for a `lambda` combination, this expression is equivalent to

```scheme
((lambda (x y)
   (lambda (a b c d e)
     ((lambda (y z) (* x y z))
      (* a b x)
      (+ c d x))))
 3
 4)
```

Each time `lookup-variable-value` searches for `x`, it must determine that the symbol `x` is not `eq?` to `y` or `z` (in the first frame), nor to `a`, `b`, `c`, `d`, or `e` (in the second frame).

Because the language is lexically scoped, the run-time environment of any expression will have a structure that parallels the lexical structure of the program in which the expression appears (this is not true if we allow internal definitions, unless we scan them out).

Thus, the compiler can know, when it analyzes the above expression, that each time the procedure is applied the variable `x` in `(* x y z)` will be found two frames out from the current frame and will be the first variable in that frame.

The above mentioned fact can be exploited by inventing a new kind of variable-lookup operation - `lexical-address-lookup` - that takes as arguments an environment and a `lexical address` that consists of two numbers:
- a `frame number`, which specifies how many frames to pass over
- a `displacement number`, which specifies how many variables to pass over in that frame

`lexical-address-lookup` produces the value of the variable stored at that lexical address relative to the current environment. This procedure can be added to the machine operations and become part of the compiled code. Similarly, `lexical-address-set!` can be added to allow for setting the variable value at its lexical address.

The compiler must be able to determine the lexical address of a variable it is about to compile a reference to. The lexical address of a variable in a program depends on where one is in the code.

In the following program, the address of `x` in expression `<e1>` is `(2,0)` - two frames back and the first variable in the frame. At that point `y` is at address `(0,0)` and `c` is at address `(1,2)`. In expression `<e2>`, `x` is at `(1,0)`, `y` is at `(1,1)`, and `c` is at `(0,2)`.

```scheme
((lambda (x y)
   (lambda (a b c d e)
     ((lambda (y z) <e1>)
      <e2>
      (+ c d x))))
 3
 4)
```

One way for the compiler to produce code that uses lexical addressing is to maintain a data structure called a `compile-time environment`. This keeps track of which variables will be at which positions in which frames in the run-time environment when a particular variable-access operation is executed.

## Resources

- [SICP - Compilation](https://mitpress.mit.edu/sites/default/files/sicp/full-text/book/book-Z-H-35.html)
