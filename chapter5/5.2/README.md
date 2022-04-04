# 5.2 A Register-Machine Simulator

The simulator is a Scheme program with four interface procedures:

- `(make-machine <register-names> <operations> <controller>)`

  constructs and returns a model of the machine with the given registers, operations, and controller

- `(set-register-contents! <machine-model> <register-name> <value>)`

  stores a value in a simulated register in the given machine

- `(get-register-contents <machine-model> <register-name>)`

  returns the contents of a simulated register in the given machine

- `(start <machine-model>)`

  simulates the execution of the given machine, starting from the beginning of the controller sequence and stopping when it reaches the end of the sequence

As an example of how these procedures are used, `gcd-machine` can be defined to be a model of the GCD machine of section `5.1.1` as follows:

```scheme
(define gcd-machine
  (make-machine
   '(a b t)
   (list (list 'rem remainder) (list '= =))
   '(test-b
       (test (op =) (reg b) (const 0))
       (branch (label gcd-done))
       (assign t (op rem) (reg a) (reg b))
       (assign a (reg b))
       (assign b (reg t))
       (goto (label test-b))
     gcd-done)))
```

The first argument to `make-machine` is a list of register names. The next argument is a table (a list of two-element lists) that pairs each operation name with a Scheme procedure that implements the operation. The last argument specifies the controller as a list of labels and machine instructions.

To compute GCDs with this machine, we set the input registers, start the machine, and examine the result when the simulation terminates:

```scheme
(set-register-contents! gcd-machine 'a 206)
done
(set-register-contents! gcd-machine 'b 40)
done
(start gcd-machine)
done
(get-register-contents gcd-machine 'a)
2
```

## The Assembler

The assembler transforms the sequence of controller expressions for a machine into a corresponding list of machine instructions, each with its execution procedure.

Overall, the assembler is much like the evaluators - there is an input language (in this case, the register-machine language) and we must perform an appropriate action for each type of expression in the language.

As in section `4.1.7`, where much useful analysis of Scheme expressions could be performed without knowing the actual values of variables, analogously, much useful analysis of register-machine-language expressions can be performed without knowing the actual contents of machine registers.

## Monitoring Machine Performance

Simulation is useful not only for verifying the correctness of a proposed machine design but also for measuring the machine's performance.

Some useful monitoring and debugging features are:
- the number of stack operations used in a computation
- the maximum depth reached by the stack
- _instruction counting_ (the number of instructions executed)
- _instruction tracing_
- a _breakpoint_ feature

## Resources

- [SICP - A Register-Machine Simulator](https://mitpress.mit.edu/sites/default/files/sicp/full-text/book/book-Z-H-32.html)
