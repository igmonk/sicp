# 5.4 The Explicit-Control Evaluator

The _explicit-control evaluator_ shows how the underlying procedure calling and argument-passing mechanisms used in the evaluation process (sections `4.1.1`-`4.1.4`) can be described in terms of operations on registers and stacks.

In addition, the explicit-control evaluator can serve as an implementation of a Scheme interpreter, written in a language that is very similar to the native machine language of conventional computers.

#### Registers and operations

To clarify the presentation, the syntax procedures given in section `4.1.2` and the procedures for representing environments and other run-time data given in sections `4.1.3` and `4.1.4` are included as primitive operations of the register machine.

The Scheme evaluator register machine includes a stack and seven registers:
- `exp` - the expression to be evaluated
- `env` - the environment in which the evaluation is to be performed
- `val` - the value obtained by evaluating the expression in the designated environment
- `continue` - is used to implement recursion
- `proc` - is used in evaluating combinations
- `argl` - is used in evaluating combinations
- `unev` - is used in evaluating combinations

## Resources

- [SICP - The Explicit-Control Evaluator](https://mitpress.mit.edu/sites/default/files/sicp/full-text/book/book-Z-H-34.html)
