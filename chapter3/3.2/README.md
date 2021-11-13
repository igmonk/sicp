# 3.2 The Environment Model of Evaluation

When we introduced compound procedures in chapter 1, we used the substitution model of evaluation (section `1.1.5`) to define what is meant by applying a procedure to arguments:
- To apply a compound procedure to arguments, evaluate the body of the procedure with each formal parameter replaced by the corresponding argument.

Once we admit assignment into our PL, such a definition is no longer adequate. In the presence of assignment, a variable can no longer be considered to be merely a name for a value. Rather, a variable must somehow designate a 'place' in which values can be stored. In our new model of evaluation, these places will be maintained in structures called _environments_.

An environment is a sequence of _frames_. Each frame is a table (possibly empty) of _bindings_, which associate variable names with their corresponding values. (A single frame may contain at most one binding for any variable.) Each frame also has a pointer to its _enclosing environment_, unless, for the purposes of discussion, the frame is considered to be _global_.

The _value of a variable_ with respect to an environment is the value given by the binding of the variable in the first frame in the environment that contains a binding for that variable. If no frame in the sequence specifies a binding for the variable, then the variable is said to be _unbound_ in the environment.

The environment is crucial to the evaluation process, because it determines the context in which an expression should be evaluated. Indeed, one could say that expressions in a PL do not, in themselves, have any meaning. Rather, an expression acquires a meaning only with respect to some environment in which it is evaluated. Thus, in our model of evaluation we will always speak of evaluating an expression with respect to some environment.

### The Rules for Evaluation

The environment model of evaluation replaces the substitution model in specifying what it means to apply a compound procedure to arguments.

In the environment model of evaluation, a procedure is always a pair consisting of some code and a pointer to an environment. Procedures are created in one way only: by evaluating a `lambda` expression. This produces a procedure whose code is obtained from the text of the `lambda` expression and whose environment is the environment in which the `lambda` expression was evaluated to produce the procedure.

Now that we have seen how procedures are created, we can describe how procedures are applied. The environment model specifies: To apply a procedure to arguments, create a new environment containing a frame that binds the parameters to the values of the arguments. The enclosing environment of this frame is the environment specified by the procedure. Now, within this new environment, evaluate the procedure body.

The environment model of procedure application can be summarized by two rules:
- A procedure object is applied to a set of arguments by constructing a frame, binding the formal parameters of the procedure to the arguments of the call, and then evaluating the body of the procedure in the context of the new environment constructed. The new frame has as its enclosing environment the environment part of the procedure object being applied.
- A procedure is created by evaluating a lambda expression relative to a given environment. The resulting procedure object is a pair consisting of the text of the lambda expression and a pointer to the environment in which the procedure was created.

Finally, we specify the behavior of `set!`. Evaluating the expression `(set! <variable> <value>)` in some environment locates the binding of the variable in the environment and changes that binding to indicate the new value. That is, one finds the first frame in the environment that contains a binding for the variable and modifies that frame. If the variable is unbound in the environment, then `set!` signals an error.

### Internal Definitions

The environment model explains the two key properties that make local procedure definitions a useful technique for modularizing programs:
- The names of the local procedures do not interfere with names external to the enclosing procedure, because the local procedure names will be bound in the frame that the procedure creates when it is run, rather than being bound in the global environment.
- The local procedures can access the arguments of the enclosing procedure, simply by using parameter names as free variables. This is because the body of the local procedure is evaluated in an environment that is subordinate to the evaluation environment for the enclosing procedure.

## Resources

- [SICP - The Environment Model of Evaluation](https://mitpress.mit.edu/sites/default/files/sicp/full-text/book/book-Z-H-21.html)
