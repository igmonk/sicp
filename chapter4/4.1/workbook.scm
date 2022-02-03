;; 4.1 The metacircular Evaluator
;;
;; An evaluator that is written in the same language that it evaluates
;; is said to be 'metacircular'.


;; 4.1.1 The Core of the Evaluator
;;
;; The evaluation process can be described as the interplay between
;; two procedures: 'eval' and 'apply'.


;; In order not to mess up with the built-in eval and apply procedures,
;; their counterparts are to be prefixed with the underscore symbol.


;; Eval
;;
;; eval takes as arguments an expression and an environment.
;; It classifies the expression and directs its evaluation.
;;
;; eval is structured as a case analysis of the syntactic type
;; of the expression to be evaluated.

(define (_eval exp env)
  (cond ((self-evaluating? exp) exp)
        ((variable? exp) (lookup-variable-value exp env))
        ((quoted? exp) (text-of-quotation exp))
        ((assignment? exp) (eval-assignment exp env))
        ((definition? exp) (eval-definition exp env))
        ((if? exp) (eval-if exp env))
        ((lambda? exp)
         (make-procedure (lambda-parameters exp)
                         (lambda-body exp)
                         env))
        ((begin? exp)
         (eval-sequence (begin-actions exp) env))
        ((cond? exp) (_eval (cond->if exp) env))
        ((application? exp)
         (_apply (_eval (operator exp) env)
                 (list-of-values (operands exp) env)))
        (else
         (error "Unknown expression type -- EVAL" exp))))

;; For clarity, eval has been implemented as a case analysis using cond.
;;
;; The disadvantage of this is that our procedure handles only a few
;; distinguishable types of expressions, and no new ones can be defined
;; without editing the definition of eval.
;;
;; In most Lisp implementations, dispatching on the type of an expression
;; is done in a data-directed style. This allows a user to add new types
;; of expressions that eval can distinguish, without modifying
;; the definition of eval itself.


;; Apply
;;
;; apply takes two arguments, a procedure and a list of arguments
;; to which the procedure should be applied.
;;
;; apply classifies procedures into two kinds:
;; - it calls `apply-primitive-procedure` to apply primitives
;; - it applies compound procedures by sequentially evaluating
;;   the expressions that make up the body of the procedure
;;
;; The environment for the evaluation of the body of a compound procedure
;; is constructed by extending the base environment carried by the procedure
;; to include a frame that binds the parameters of the procedure to
;; the arguments to which the procedure is to be applied.

(define (_apply procedure arguments)
  (cond ((primitive-procedure? procedure)
         (apply-primitive-procedure procedure arguments))
        ((compound-procedure? procedure)
         (eval-sequence
          (procedure-body procedure)
          (extend-environment
           (procedure-parameters procedure)
           arguments
           (procedure-environment procedure))))
        (else
         (error "Unknown procedure type -- APPLY" procedure))))


;; Procedure arguments
;;
;; Takes as an argument the operands of the combination,
;; evaluates each operand and returns a list of
;; the corresponding values.

(define (list-of-values exps env)
  (if (no-operands? exps)
      '()
      (cons (_eval (first-operand exps) env)
            (list-of-values (rest-operands exps) env))))

;; We chose not to use 'map' here to emphasize the fact that
;; the evaluator can be implemented without any use of
;; higher-order procedures, even though the language that
;; it supports will include higher-order procedures.


;; Conditionals
;;
;; Evaluates the predicate part of an 'if' expression in the given env.
;; If result is true, eval-if evaluates the consequent, otherwise
;; it evaluates the alternative.

(define (eval-if exp env)
  (if (true? (_eval (if-predicate exp) env))
      (_eval (if-consequent exp) env)
      (_eval (if-alternative exp) env)))

;; The use of true? in eval-if highlights the issue of the connection
;; between an implemented language and an implementation language.


;; Sequences
;;
;; Evaluates the sequence of expressions in a procedure body
;; (or in a 'begin' expression).
;; The value returned is the value of the final expression.

(define (eval-sequence exps env)
  (cond ((last-exp? exps)
         (_eval (first-exp exps) env))
        (else (_eval (first-exp exps) env)
              (eval-sequence (rest-exps exps) env))))


;; Assignments and definitions
;;
;; Calls 'eval' to find the value to be assigned/defined
;; and transmits it together with the variable to be installed
;; in the designated environment.

(define (eval-assignment exp env)
  (set-variable-value! (assignment-variable exp)
                       (_eval (assignment-value exp) env)
                       env)
  'ok)

(define (eval-definition exp env)
  (define-variable! (definition-variable exp)
                    (_eval (definition-value exp) env)
                    env)
  'ok)


;; 4.1.2 Representing Expressions
;;
;; The syntax of the language being evaluated is determined solely
;; by the procedures that classify and extract pieces of expressions.


;; Self-evaluating items: numbers and strings

(define (self-evaluating? exp)
  (cond ((number? exp) true)
        ((string? exp) true)
        (else false)))


;; Variables are represented by symbols:

(define (variable? exp) (symbol? exp))


;; Quotations have the form (quote <text-of-quotation>):

(define (quoted? exp)
  (tagged-list? exp 'quote))

(define (text-of-quotation exp) (cadr exp))


;; tagged-list? identifies lists beginning with a designated symbol:

(define (tagged-list? exp tag)
  (if (pair? exp)
      (eq? (car exp) tag)
      false))


;; Assignments have the form (set! <var> <value>):

(define (assignment? exp)
  (tagged-list? exp 'set!))

(define (assignment-variable exp) (cadr exp))
(define (assignment-value exp) (caddr exp))


;; Definitions have one of the following forms:
;;
;; - (define <var> <value>)
;; - (define (<var> <parameter-1> ... <parameter-n>)
;;     <body>)
;;
;; The latter form is syntactic sugar for
;;
;; (define <var>
;;   (lambda (<parameter-1> ... <parameter-n>)
;;     <body>))

(define (definition? exp)
  (tagged-list? exp 'define))

(define (definition-variable exp)
  (if (symbol? (cadr exp))
      (cadr exp)
      (caadr exp)))

(define (definition-value exp)
  (if (symbol? (cadr exp))
      (caddr exp)
      (make-lambda (cdadr exp)   ; formal parameters
                   (cddr exp)))) ; body


;; Lambda expressions

(define (lambda? exp)
  (tagged-list? exp 'lambda))

(define (lambda-parameters exp) (cadr exp))
(define (lambda-body exp) (cddr exp))

(define (make-lambda parameters body)
  (cons 'lambda (cons parameters body)))


;; Conditionals
;;
;; Conditionals begin with 'if' and have a predicate, a consequent, and
;; an (optional) alternative.
;;
;; If the expression has no alternative part, 'false' is provided as
;; the alternative (unlike Scheme, where the default alternative is unspecified).

(define (if? exp) (tagged-list? exp 'if))

(define (if-predicate exp) (cadr exp))
(define (if-consequent exp) (caddr exp))
(define (if-alternative exp)
  (if (not (null? (cdddr exp)))
      (cadddr exp)
      'false))

(define (make-if predicate consequent alternative)
  (list 'if predicate consequent alternative))


;; begin / sequence
;;
;; 'begin' packages a sequence of expressions into a single expression.

(define (begin? exp) (tagged-list? exp 'begin))

(define (begin-actions exp) (cdr exp))

;; The following procedures are not intended as a data abstraction.
;; They are introduced as mnemonic names for the basic list operations
;; in order to make it easier to understand the explicit-control evaluator
;; (in section 5.4):

(define (last-exp? seq) (null? (cdr seq)))
(define (first-exp seq) (car seq))
(define (rest-exps seq) (cdr seq))


;; sequence->exp constructor (for use by cond->if) that transforms
;; a sequence into a single expression, using 'begin' if necessary:

(define (sequence->exp seq)
  (cond ((null? seq) seq)
        ((last-exp? seq) (first-exp seq))
        (else (make-begin seq))))

(define (make-begin seq) (cons 'begin seq))


;; Procedure application
;;
;; A procedure application is any compound expression that is not
;; one of the above expression types.
;;
;; The car of the expression is the operator, and
;; the cdr is the list of operands.

(define (application? exp) (pair? exp))

(define (operator exp) (car exp))
(define (operands exp) (cdr exp))

(define (no-operands? ops) (null? ops))
(define (first-operand ops) (car ops))
(define (rest-operands ops) (cdr ops))


;; Derived expressions
;;
;; Some special forms in our language can be defined in terms of expressions
;; involving other special forms, rather than being implemented directly.
;;
;; One example is cond, which can be implemented as a nest of if expressions.

;; Implementing the evaluation of 'cond' in this way (nested if expressions)
;; simplifies the evaluator because it reduces the number of special forms
;; for which the evaluation process must be explicitly specified.

(define (cond? exp) (tagged-list? exp 'cond))

(define (cond-clauses exp) (cdr exp))

(define (cond-else-clause? clause)
  (eq? (cond-predicate clause) 'else))

(define (cond-predicate clause) (car clause))
(define (cond-actions clause) (cdr clause))

(define (cond->if exp)
  (expand-clauses (cond-clauses exp)))

(define (expand-clauses clauses)
  (if (null? clauses)
      'false ; no else clause
      (let ((first (car clauses))
            (rest (cdr clauses)))
        (if (cond-else-clause? first)
            (if (null? rest)
                (sequence->exp (cond-actions first))
                (error "ELSE clause isn't last -- COND->IF" clauses))
            (make-if (cond-predicate first)
                     (sequence->exp (cond-actions first))
                     (expand-clauses rest))))))


;; Practical Lisp systems provide a mechanism that allows a user
;; to add new derived expressions and specify their implementation
;; as syntactic transformations without modifying the evaluator.
;;
;; Such a user-defined transformation is called a 'macro'.
;;
;; Although it is easy to add an elementary mechanism for defining macros,
;; the resulting language has subtle name-conflict problems.


;; 4.1.3 Evaluator Data Structures
;;
;; In addition to defining the external syntax of expressions,
;; the evaluator implementation must also define the data structures
;; that the evaluator manipulates internally, as part of the execution
;; of a program, such as the representation of
;;
;; 1) true and false
;; 2) procedures
;; 3) environments


;; 1) Testing of predicates

(define (true? x) (not (eq? x false)))
(define (false? x) (eq? x false))


;; 2) Representing procedures
;;
;; To handle primitives, we assume that we have available the following procedures:
;;
;; - (apply-primitive-procedure <proc> <args>)
;;    applies the given primitive procedure to the argument values
;;    in the list <args> and returns the result of the application.
;;
;; - (primitive-procedure? <proc>)
;;   tests whether <proc> is a primitive procedure.
;;
;; Described in section 4.1.4 below.

;; Compound procedures are constructed from parameters, procedure bodies,
;; and environments using the constructor 'make-procedure':

(define (make-procedure parameters body env)
  (list 'procedure parameters body env))

(define (compound-procedure? p)
  (tagged-list? p 'procedure))

(define (procedure-parameters p) (cadr p))
(define (procedure-body p) (caddr p))
(define (procedure-environment p) (cadddr p))


;; 3) Operations on Environments
;;
;; As explained in section 3.2, an environment is a sequence of frames,
;; where each frame is a table of bindings that associate variables
;; with their corresponding values.
;;
;; The following operations for manipulating environments are used:
;;
;; - (lookup-variable-value <var> <env>)
;;
;;   returns the value that is bound to the symbol <var> in the environment <env>,
;;   or signals an error if the variable is unbound
;;
;; - (extend-environment <variables> <values> <base-env>)
;;
;;   returns a new environment,
;;   consisting of a new frame in which the symbols in the list <variables>
;;   are bound to the corresponding elements in the list <values>,
;;   where the enclosing environment is the environment <base-env>
;; 
;; - (define-variable! <var> <value> <env>)
;;
;;   adds to the first frame in the environment <env> a new binding that
;;   associates the variable <var> with the value <value>
;;
;; - (set-variable-value! <var> <value> <env>)
;;
;;   changes the binding of the variable <var> in the environment <env>
;;   so that the variable is now bound to the value <value>,
;;   or signals an error if the variable is unbound

;; The environment is represented as a list of frames.
;; The enclosing environment of an environment is the cdr of the list.
;; The empty environment is the empty list.

(define (enclosing-environment env) (cdr env))
(define (first-frame env) (car env))
(define the-empty-environment '())

;; Each frame of an environment is represented as a pair of lists:
;; - a list of the variables bound in that frame
;; - a list of the associated values
;;
;; * Frames are not really a data abstraction in the following code:
;;   set-variable-value! and define-variable! use set-car! to
;;   directly modify the values in a frame.
;;   The purpose of the frame procedures is to make
;;   the environment-manipulation procedures easy to read.

(define (make-frame variables values)
  (cons variables values))

(define (frame-variables frame) (car frame))
(define (frame-values frame) (cdr frame))

(define (add-binding-to-frame! var val frame)
  (set-car! frame (cons var (car frame)))
  (set-cdr! frame (cons val (cdr frame))))

;; To extend an environment by a new frame that associates variables
;; with values, we make a frame consisting of the list of variables
;; and the list of values, and we adjoin this to the environment.
;;
;; An error is signalled if the number of variables does not match
;; the number of values.

(define (extend-environment vars vals base-env)
  (if (= (length vars) (length vals))
      (cons (make-frame vars vals) base-env)
      (if (< (length vars) (length vals))
          (error "Too many arguments supplied" vars vals)
          (error "Too few arguments supplied" vars vals))))


;; Look up a variable in an environment.
;; If the variable is not found in the current frame,
;; the enclosing environment is searched, and so on.
;; An 'unbound variable' error is signalled
;; if the empty environment is reached.

(define (lookup-variable-value var env)
  (define (env-loop env)
    (define (scan vars vals)
      (cond ((null? vars)
             (env-loop (enclosing-environment env)))
            ((eq? var (car vars))
             (car vals))
            (else (scan (cdr vars) (cdr vals)))))
    (if (eq? env the-empty-environment)
        (error "Unbound variable" var)
        (let ((frame (first-frame env)))
          (scan (frame-variables frame)
                (frame-values frame)))))
  (env-loop env))


;; Set a variable to a new value in a specified environment.

(define (set-variable-value! var val env)
  (define (env-loop env)
    (define (scan vars vals)
      (cond ((null? vars)
             (env-loop (enclosing-environment env)))
            ((eq? var (car vars))
             (set-car! vals val))
            (else (scan (cdr vars) (cdr vals)))))
    (if (eq? env the-empty-environment)
        (error "Unbound variable -- SET!" var)
        (let ((frame (first-frame env)))
          (scan (frame-variables frame)
                (frame-values frame)))))
  (env-loop env))


;; Define a variable.
;; If found in the first frame, the binding for the variable is changed.
;; If no such binding exists, the requested variable and its value
;; are adjoined to the first frame.

(define (define-variable! var val env)
  (let ((frame (first-frame env)))
    (define (scan vars vals)
      (cond ((null? vars)
             (add-binding-to-frame! var val frame))
            ((eq? var (car vars))
             (set-car! vals val))
            (else (scan (cdr vars) (cdr vals)))))
    (scan (frame-variables frame)
          (frame-values frame))))

;; In a production-quality Lisp system, the speed of
;; the evaluator's environment operations - esp. that of variable lookup -
;; has a major impact on the performance of the system.
;;
;; The representation described here, although conceptually simple,
;; is not efficient and would not ordinarily be used in a production system.

;; The drawback of this representation (as well as the variant in exercise 4.11)
;; is that the evaluator may have to search through many frames
;; in order to find the binding for a given variable (deep binding).
;;
;; One way to avoid this inefficiency is to make use of a strategy
;; called 'lexical addressing' (see section 5.5.6).


;; 4.1.4 Running the Evaluator as a Program
;;
;; The evaluator program reduces expressions ultimately to
;; the application of primitive procedures.
;;
;; Therefore, all that we need to run the evaluator is
;; to create a mechanism that calls on the underlying Lisp system
;; to model the application of primitive procedures.
;;
;; There must be a binding for each primitive procedure name,
;; so that when eval evaluates the operator of an application
;; of a primitive, it will find an object to pass to apply.
;;
;; We thus set up a global environment that associates unique objects
;; with the names of the primitive procedures that can appear
;; in the expressions we will be evaluating.

(define (setup-environment)
  (let ((initial-env
         (extend-environment (primitive-procedure-names)
                             (primitive-procedure-objects)
                             the-empty-environment)))
    (define-variable! 'true true initial-env)
    (define-variable! 'false false initial-env)
    initial-env))


;; It does not matter how the primitive procedure objects are represented,
;; so long as 'apply' can identify and apply them.


;; Primitive procedure representation

(define (primitive-procedure? proc)
  (tagged-list? proc 'primitive))

(define (primitive-implementation proc) (cadr proc))


;; Any procedure defined in the underlying Lisp can be used as
;; a primitive for the metacircular evaluator.
;;
;; The name of a primitive installed in the evaluator need not be
;; the same as the name of its implementation in the underlying Lisp.

(define primitive-procedures
  (list (list 'car car)
        (list 'cdr cdr)
        (list 'cons cons)
        (list 'null? null?)
        ;; <more primitives>
        ))

(define (primitive-procedure-names)
  (map car primitive-procedures))

(define (primitive-procedure-objects)
  (map (lambda (proc) (list 'primitive (cadr proc)))
       primitive-procedures))


;; To apply a primitive procedure, we simply apply the implementation
;; procedure to the arguments, using the underlying Lisp system:

(define (apply-primitive-procedure proc args)
  (apply (primitive-implementation proc) args))


;; REPL - the read-eval-print loop
;;
;; It
;; 1) prints a prompt
;; 2) reads an input expression
;; 3) evaluates this expression in the global environment
;; 4) prints the result

(define input-prompt ";;; M-Eval input:")
(define output-prompt ";;; M-Eval value:")

(define (driver-loop)
  (prompt-for-input input-prompt)
  (let ((input (read)))
    (let ((output (_eval input the-global-environment)))
      (announce-output output-prompt)
      (user-print output)))
  (driver-loop))

(define (prompt-for-input str)
  (newline)
  (newline)
  (display str)
  (newline))

(define (announce-output str)
  (newline)
  (display str)
  (newline))

;; A special printing procedure, user-print, is used
;; to avoid printing the environment part of a compound procedure,
;; which may be a very long list (or may even contain cycles).

(define (user-print object)
  (if (compound-procedure? object)
      (display (list 'compound-procedure
                     (procedure-parameters object)
                     (procedure-body object)
                     '<procedure-env>))
      (display object)))

(define the-global-environment (setup-environment))


;; 4.1.5 Data as Programs
;;
;; The notion of "what can in principle be computed"
;; (ignoring practicalities of time and memory required)
;; is independent of the language or the computer, and
;; instead reflects an underlying notion of 'computability'.
;;
;; The user's programs are the evaluator's data.


;; 4.1.6 Internal Definitions
;;
;; Name-by-name extension of the environment may not be the best way
;; to define local variables.
;;
;; Since the definitions of the internal procedures come first,
;; no calls to these procedures will be evaluated until all of them
;; have been defined.
;;
;; The sequential evaluation mechanism gives the same result as
;; a mechanism that directly implements simultaneous definition
;; for any procedure in which the internal definitions come first
;; in a body and evaluation of the value expressions.
;;
;; See the special form 'letrec'.


;; 4.1.7 Separating Syntactic Analysis from Execution
;;
;; If the syntactic analysis of expressions is interleaved with
;; their execution, an evaluator becomes very inefficient.
;; Thus if a program is executed many times, its syntax is analyzed
;; many times.
;;
;; It's useful to split eval into two parts:
;; 1 - analyze (to perform the syntactic analysis)
;; 2 - exec (to complete the evaluation with the given env)
;;
;; This saves work because 'analyze' will be called only once (!)
;; on an expression, while the execution procedure may be called
;; many times.

;; With the separation into analysis and execution, eval now becomes
;;
;; (define (eval exp env)
;;   ((analyze exp) env))
;;
;; See: evaluator.scm & its extensions
