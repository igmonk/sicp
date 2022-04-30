;; 5.5 Compilation


;; Structure of the Compiler
;;
;; The procedure 'compile' is the top-level dispatch in the compiler.
;; For each type of expression, it dispatches to a specialized
;; code generator.


;; Targets and linkages
;;
;; Target specifies the register in which the compiled code is to return
;; the value of the expression.
;;
;; A linkage descriptor describes how the code resulting from
;; the compilation of the expression should proceed when it has finished
;; its execution.
;;
;; The linkage descriptor can require that the code do one of
;; the following three things:
;;
;; - continue at the next instruction in sequence (next)
;; - return from the procedure being compiled (return)
;; - jump to a named entry point (a designated label)


;; Instruction sequences and stack usage
;;
;; Each code generator returns an instruction sequence containing
;; the object code it has generated for the expression.
;;
;; Code generation for a compound expression is accomplished by
;; combining the output from simpler code generators for component
;; expressions.
;;
;; Methods for combining instruction sequences:
;;
;; - append-instruction-sequences
;;   takes as arguments any number of instruction sequences that
;;   are to be executed sequentially
;;
;; - preserving
;;   appends the sequences in such a way that the contents of each register
;;   in the set is preserved over the execution of the first sequence, if
;;   this is needed for the execution of the second sequence (by wrapping
;;   the stack operations - save and restore - around the first sequence
;;   before appending the sequences)

;; An instruction sequence will contain three pieces of information:
;;
;; - the set of registers that must be initialized before the instructions
;;   in the sequence are executed (needs)
;; - the set of registers whose values are modified by the instructions
;;   in the sequence (modifies)
;; - the actual instructions (statements) in the sequence


;; Lexical Addressing
;;
;; One of the most common optimizations performed by compilers is
;; the optimization of variable lookup.
;;
;; The lookup-variable-value operation of the evaluator machine
;; searches for a variable by comparing it with each variable that
;; is currently bound, working frame by frame outward through the
;; run-time environment. This search can be expensive if the frames
;; are deeply nested or if there are many variables.
;;
;; Because the language is lexically scoped, the run-time environment
;; of any expression will have a structure that parallels the lexical
;; structure of the program in which the expression appears (this is not
;; true if we allow internal definitions, unless we scan them out).
;;
;; The above mentioned fact can be exploited by inventing a new kind of
;; variable-lookup operation - lexical-address-lookup - that takes
;; as arguments an environment and a `lexical address` that consists of
;; two numbers:
;; - a 'frame number', which specifies how many frames to pass over
;; - a 'displacement number', which specifies how many variables to
;;   pass over in that frame
;;
;; The compiler must be able to determine the lexical address of
;; a variable it is about to compile a reference to. The lexical address
;; of a variable in a program depends on where one is in the code.
;;
;; One way for the compiler to produce code that uses lexical addressing
;; is to maintain a data structure called a 'compile-time environment'.
;; This keeps track of which variables will be at which positions
;; in which frames in the run-time environment when a particular
;; variable-access operation is executed.
