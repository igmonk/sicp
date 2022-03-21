# Computing with Register Machines

The metacircular evaluator is itself a Lisp program and hence inherits the control structure of the underlying Lisp system. In order to provide a more complete description of the control structure of the Lisp evaluator, we must work at a more primitive level than Lisp itself.

In this chapter processes are described in terms of the step-by-step operation of a traditional computer. Such a computer, or _register machine_, sequentially executes _instructions_ that manipulate the contents of a fixed set of storage elements called _registers_.

In designing register machines, we will develop mechanisms for implementing important programming constructs such as recursion. We will also present a language for describing designs for register machines.

Finally, we will study a simple compiler that translates Scheme programs into sequences of instructions that can be executed directly with the registers and operations of the evaluator register machine.

## Resources

- [SICP - Computing with Register Machines](https://mitpress.mit.edu/sites/default/files/sicp/full-text/book/book-Z-H-30.html)
