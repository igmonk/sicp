# 5.1 Designing Register Machines

To design a register machine, we must design its _data paths_ (registers and operations) and the _controller_ that sequences these operations.

Consider Euclid's Algorithm:

```scheme
(define (gcd a b)
  (if (= b 0)
      a
      (gcd b (remainder a b))))
```

The registers and operations required for this machine can be illustrated by using the data-path diagram:

```
┌-----┐        ┌-----┐
|  a  |←X------|  b  |-----→( = )
└-----┘ a<-b   └-----┘        ↑
   |             |  ↑         |
   |             |  X b<-t    |
   └---┐     ┌---┘  |         |
    ___↓_____↓___   |         |
    \           /   |        / \
     \   rem   /    |       / 0 \
      \_______/     |      /_____\
          |         |
          |         |
          X t<-r    |
          ↓         |
       ┌-----┐      |
       |  t  |------┘
       └-----┘
```

In this diagram,
- the registers (`a`, `b`, and `t`) are represented by rectangles
- each way to assign a value to a register is indicated by an arrow with an `X` behind the head, pointing from the source of data to the register
- the label next to each button (`X`) is the name used to refer to the button
- an operation is represented by a trapezoid (`rem` computes the remainder)
- a test is represented by a circle containing a name for the test `( = )`

The source of data for a register can be
- another register (as in the `a<-b` assignment)
- an operation result (as in the `t<-r` assignment)
- a constant (a built-in value that cannot be changed, represented in a data-path diagram by a triangle containing the constant)

Overall, the data-path diagram shows the registers and operations that are required for the machine and how they must be connected.

In order for the data paths to actually compute GCDs, the buttons must be pushed in the correct sequence. This sequence is described in terms of a controller diagram:

```
     start
       |
       ↓
     /   \  yes
┌--→   =   -----→ done
|    \   /
|      | no
|      ↓
|   ┌------┐
|   | t<-r |
|   └------┘
|      |
|      ↓
|   ┌------┐
|   | a<-b |
|   └------┘
|      |
|      ↓
|   ┌------┐
└---| b<-t |
    └------┘
```

The elements of the controller diagram indicate how the data-path components should be operated:
- the rectangular boxes identify data-path buttons to be pushed
- the arrows describe the sequencing from one step to the next
- the diamond in the diagram represents a decision

We start the controller at the place marked `start`, after placing numbers in registers `a` and `b`. When the controller reaches `done`, we will find the value of the GCD in register `a`.

## A Language for Describing Register Machines

Data-path and controller diagrams are adequate for representing simple machines such as GCD, but they are unwieldy for describing large machines such as a Lisp interpreter. To make it possible to deal with complex machines, we will create a language that presents, in textual form, all the information given by the data-path and controller diagrams.

### A notation that directly mirrors the diagrams

The data paths of a machine are defined by describing the registers and the operations. The controller of a machine is defined as a sequence of _instructions_ together with _labels_ that identify _entry points_ in the sequence.

The machine starts at the beginning of the controller instruction sequence and stops when execution reaches the end of the sequence. Except when a branch changes the flow of control, instructions are executed in the order in which they are listed.

```scheme
(data-paths
 (registers
  ((name a)
   (buttons ((name a<-b) (source (register b)))))
  ((name b)
   (buttons ((name b<-t) (source (register t)))))
  ((name t)
   (buttons ((name t<-r) (source (operation rem))))))

 (operations
  ((name rem)
   (inputs (register a) (register b)))
  ((name =)
   (inputs (register b) (constant 0)))))

(controller
 test-b                           ; label
   (test =)                       ; test
   (branch (label gcd-done))      ; conditional branch
   (t<-r)                         ; button push
   (a<-b)                         ; button push
   (b<-t)                         ; button push
   (goto (label test-b))          ; unconditional branch
 gcd-done)                        ; label
```

Unfortunately, it is difficult to read such a description. In order to understand the controller instructions we must constantly refer back to the definitions of the button names and the operation names, and to understand what the buttons do we may have to refer to the definitions of the operation names.

### A combined notation

The information from the data-path and controller descriptions is combined so that it can be seen all together.

```scheme
(controller
 test-b
   (test (op =) (reg b) (const 0))
   (branch (label gcd-done))
   (assign t (op rem) (reg a) (reg b))
   (assign a (reg b))
   (assign b (reg t))
   (goto (label test-b))
 gcd-done)
```

This form of description is easier to read, but it also has disadvantages:
- it is more verbose for large machines, because complete descriptions of the data-path elements are repeated whenever the elements are mentioned in the controller instruction sequence
- because the controller instructions in a machine definition look like Lisp expressions, it is easy to forget that they are not arbitrary Lisp expressions

### Actions

The GCD machine is to be modified so that we can type in the numbers whose GCD we want and get the answer printed at our terminal.

_Read_ is like the operations we have been using in that it produces a value that can be stored in a register.

_Print_ differs from the operations: it does not produce an output value to be stored in a register. Though it has an effect, this effect is not on a part of the machine we are designing. We will refer to this kind of operation as an _action_.

Below is the controller for the new GCD machine.

```scheme
(controller
  gcd-loop
    (assign a (op read))
    (assign b (op read))
  test-b
    (test (op =) (reg b) (const 0))
    (branch (label gcd-done))
    (assign t (op rem) (reg a) (reg b))
    (assign a (reg b))
    (assign b (reg t))
    (goto (label test-b))
  gcd-done
    (perform (op print) (reg a))
    (goto (label gcd-loop)))
```

Instead of having the machine stop after printing the answer, we have made it start over, so that it repeatedly reads a pair of numbers, computes their GCD, and prints the result. This structure is like the driver loops we used in the interpreters of chapter `4`.

## Abstraction in Machine Design

We can always replace the complex "primitives" by simpler primitive operations.

If we want to construct the GCD machine without using a primitive remainder operation, we must specify how to compute remainders in terms of simpler operations, such as subtraction.

A Scheme procedure that finds remainders can be written in this way:

```scheme
(define (remainder n d)
  (if (< n d)
      n
      (remainder (- n d) d)))
```

The remainder operation in the GCD machine's data paths can be replaced with a subtraction operation and a comparison test. The instruction

```scheme
(assign t (op rem) (reg a) (reg b))
```

in the GCD controller definition is replaced by a sequence of instructions that contains a loop:

```scheme
(controller
 test-b
   (test (op =) (reg b) (const 0))
   (branch (label gcd-done))
   (assign t (reg a))
 rem-loop
   (test (op <) (reg t) (reg b))
   (branch (label rem-done))
   (assign t (op -) (reg t) (reg b))
   (goto (label rem-loop))
 rem-done
   (assign a (reg b))
   (assign b (reg t))
   (goto (label test-b))
 gcd-done)
```

## Subroutines

When designing a machine to perform a computation, we would often prefer to arrange for components to be shared by different parts of the computation rather than duplicate the components.

Two equivalent sequences can be replaced by branches to a single sequence - a `subroutine` - at the end of which we branch back to the correct place in the main instruction sequence.

One of the methods for implementing subroutines is to have the `continue` register hold the label of the entry point in the controller sequence at which execution should continue when the subroutine is finished.

Implementing this strategy requires a new kind of connection between the data paths and the controller of a register machine: There must be a way to assign to a register a label in the controller sequence in such a way that this value can be fetched from the register and used to continue execution at the designated entry point.

```scheme
gcd
 (test (op =) (reg b) (const 0))
 (branch (label gcd-done))
 (assign t (op rem) (reg a) (reg b))
 (assign a (reg b))
 (assign b (reg t))
 (goto (label gcd))
gcd-done
 (goto (reg continue))
   
 ;; Before calling gcd, we assign to continue
 ;; the label to which gcd should return.
 (assign continue (label after-gcd-1))
 (goto (label gcd))
after-gcd-1
   
 ;; Here is the second call to gcd, with a different continuation.
 (assign continue (label after-gcd-2))
 (goto (label gcd))
after-gcd-2
```

Notice, the goto instruction is extended to allow execution to continue at the entry point described by the contents of a register rather than only at an entry point described by a constant label.

A machine with more than one subroutine could use multiple continuation registers (e.g., `gcd-continue`, `factorial-continue`) or we could have all subroutines share a single `continue` register.

## Using a Stack to Implement Recursion

Any iterative process can be implemented by specifying a register machine that has a register corresponding to each state variable of the process.

The machine repeatedly executes a controller loop, changing the contents of the registers, until some termination condition is satisfied. At each point in the controller sequence, the state of the machine (representing the state of the iterative process) is completely determined by the contents of the registers (the values of the state variables).

Implementing recursive processes, however, requires an additional mechanism.

Consider the following recursive method for computing factorials:

```scheme
(define (factorial n)
  (if (= n 1)
      1
      (* (factorial (- n 1)) n)))
```

As we see from the procedure, computing `n!` requires computing `(n - 1)!`. The answer to the new factorial subproblem is not the answer to the original problem. The value obtained for `(n - 1)!` must be multiplied by `n` to get the final answer.

We thus need a second factorial machine to work on the subproblem. This second factorial computation itself has a factorial subproblem, which requires a third factorial machine, and so on. Since each factorial machine contains another factorial machine within it, the total machine contains an infinite nest of similar machines and hence cannot be constructed from a fixed, finite number of parts.

Nevertheless, we can implement the factorial process as a register machine if we can arrange to use the same components for each nested instance of the machine. Specifically, the machine that computes `n!` should use the same components to work on the subproblem of computing `(n - 1)!`, on the subproblem for `(n - 2)!`, and so on.

Only one of these machines needs to be active at any given time. When the machine encounters a recursive subproblem, it can suspend work on the main problem, reuse the same physical parts to work on the subproblem, then continue the suspended computation.

In order to be able to continue the suspended computation, the machine must save the contents of any registers that will be needed after the subproblem is solved so that these can be restored to continue the suspended computation.

Since there is no _a priori_ limit on the depth of nested recursive calls, we may need to save an arbitrary number of register values. These values must be restored in the reverse of the order in which they were saved, since in a nest of recursions the last subproblem to be entered is the first to be finished. This dictates the use of a _stack_, or "last in, first out" data structure, to save register values.

The register-machine language can be extended to include a stack by adding two kinds of instructions: values are
- placed on the stack using a `save` instruction
- restored from the stack using a `restore` instruction

Below is the controller for a machine that implements the recursive `factorial` procedure:

```scheme
(controller
   (assign continue (label fact-done))     ; set up final return address
 fact-loop
   (test (op =) (reg n) (const 1))
   (branch (label base-case))
   ;; Set up for the recursive call by saving n and continue.
   ;; Set up continue so that the computation will continue
   ;; at after-fact when the subroutine returns.
   (save continue)
   (save n)
   (assign n (op -) (reg n) (const 1))
   (assign continue (label after-fact))
   (goto (label fact-loop))
 after-fact
   (restore n)
   (restore continue)
   (assign val (op *) (reg n) (reg val))   ; val now contains n(n - 1)!
   (goto (reg continue))                   ; return to caller
 base-case
   (assign val (const 1))                  ; base case: 1! = 1
   (goto (reg continue))                   ; return to caller
 fact-done)
```

When a recursive subproblem is encountered, we save on the stack the registers whose current values will be required after the subproblem is solved, solve the recursive subproblem, then restore the saved registers and continue execution on the main problem.

### A double recursion

Consider a more complex recursive process, the tree-recursive computation of the Fibonacci numbers:

```scheme
(define (fib n)
  (if (< n 2)
      n
      (+ (fib (- n 1)) (fib (- n 2)))))
```

The machine is more complex than the one for factorial, because there are two places in the controller sequence where we need to perform recursive calls:
- once to compute `Fib(n - 1)`, and
- once to compute `Fib(n - 2)`


```scheme
(controller
   (assign continue (label fib-done))
 fib-loop
   (test (op <) (reg n) (const 2))
   (branch (label immediate-answer))
   ;; set up to compute Fib(n - 1)
   (save continue)
   (assign continue (label afterfib-n-1))
   (save n)                           ; save old value of n
   (assign n (op -) (reg n) (const 1)); clobber n to n - 1
   (goto (label fib-loop))            ; perform recursive call
 afterfib-n-1                         ; upon return, val contains Fib(n - 1)
   (restore n)
   (restore continue)
   ;; set up to compute Fib(n - 2)
   (assign n (op -) (reg n) (const 2))
   (save continue)
   (assign continue (label afterfib-n-2))
   (save val)                         ; save Fib(n - 1)
   (goto (label fib-loop))
 afterfib-n-2                         ; upon return, val contains Fib(n - 2)
   (assign n (reg val))               ; n now contains Fib(n - 2)
   (restore val)                      ; val now contains Fib(n - 1)
   (restore continue)
   (assign val                        ;  Fib(n - 1) +  Fib(n - 2)
           (op +) (reg val) (reg n)) 
   (goto (reg continue))              ; return to caller, answer is in val
 immediate-answer
   (assign val (reg n))               ; base case:  Fib(n) = n
   (goto (reg continue))
 fib-done)
```

## Instruction Summary

A controller instruction in the register-machine language has one of the following forms, where each `<input-i>` is either `(reg <register-name>)` or `(const <constant-value>)`.

Basic instructions:

```scheme
(assign <register-name> (reg <register-name>))
(assign <register-name> (const <constant-value>))
(assign <register-name> (op <operation-name>) <input1> ... <inputn>)
(perform (op <operation-name>) <input1> ... <inputn>)
(test (op <operation-name>) <input1> ... <inputn>)
(branch (label <label-name>))
(goto (label <label-name>))
```

The use of registers to hold labels:

```scheme
(assign <register-name> (label <label-name>))
(goto (reg <register-name>))
```

Instructions to use the stack:

```scheme
(save <register-name>)
(restore <register-name>)
```

There are several kinds of `<constant-value>`: numbers, strings, symbols, and lists.

## Resources

- [SICP - Designing Register Machines](https://mitpress.mit.edu/sites/default/files/sicp/full-text/book/book-Z-H-31.html)
