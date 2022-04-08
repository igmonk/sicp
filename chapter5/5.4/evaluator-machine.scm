;; Explicit-Control Evaluator

;; cd to sicp/chapter5/5.2
(load "machine.scm")
(load "basic-machine-ext.scm")

;; cd to sicp/chapter5/5.4
(load "evaluator-operations.scm")

(define ec-eval-machine
  (make-machine
   '(exp env val continue proc argl unev)
   eceval-operations
   '(
     ;; Driver loop
     ;;
     ;; - print a prompt
     ;; - read an expression
     ;; - evaluate the expression by going to eval-dispatch
     ;; - print the result
     read-eval-print-loop
     (perform (op initialize-stack))
     (perform (op prompt-for-input)
              (const ";;; EC-Eval input:"))
     (assign exp (op read))
     (assign env (op get-global-environment))
     (assign continue (label print-result))
     (goto (label eval-dispatch))

     print-result
     (perform (op print-stack-statistics))
     (perform (op announce-output)
              (const ";;; EC-Eval value:"))
     (perform (op user-print) (reg val))
     (goto (label read-eval-print-loop))

     
     ;; Eval
     ;;
     ;; The controller evaluates the expression specified by exp
     ;; in the environment specified by env.
     ;; When evaluation is complete, the controller will go to
     ;; the entry point stored in 'continue', and the 'val' register
     ;; will hold the value of the expression.
     eval-dispatch
     (test (op self-evaluating?) (reg exp))
     (branch (label ev-self-eval))
     (test (op variable?) (reg exp))
     (branch (label ev-variable))
     (test (op quoted?) (reg exp))
     (branch (label ev-quoted))
     (test (op assignment?) (reg exp))
     (branch (label ev-assignment))
     (test (op definition?) (reg exp))
     (branch (label ev-definition))
     (test (op if?) (reg exp))
     (branch (label ev-if))
     (test (op cond?) (reg exp))
     (branch (label ev-cond))
     (test (op lambda?) (reg exp))
     (branch (label ev-lambda))
     (test (op begin?) (reg exp))
     (branch (label ev-begin))
     (test (op let?) (reg exp))
     (branch (label ev-let))
     (test (op application?) (reg exp))
     (branch (label ev-application))
     (goto (label unknown-expression-type))


     ;; Evaluating simple expressions
     ;;
     ;; The correct value is placed in the 'val' register and
     ;; execution continues at the entry point specified by 'continue'.
     ev-self-eval
     (assign val (reg exp))
     (goto (reg continue))
     
     ev-variable
     (assign val (op lookup-variable-value) (reg exp) (reg env))
     (goto (reg continue))
     
     ev-quoted
     (assign val (op text-of-quotation) (reg exp))
     (goto (reg continue))
     
     ev-lambda
     (assign unev (op lambda-parameters) (reg exp))
     (assign exp (op lambda-body) (reg exp))
     (assign val (op make-procedure)
             (reg unev) (reg exp) (reg env))
     (goto (reg continue))


     ;; Evaluating procedure applications
     ev-application
     (save continue)
     (save env)
     (assign unev (op operands) (reg exp))
     (save unev)
     (assign exp (op operator) (reg exp))
     (assign continue (label ev-appl-did-operator))
     (goto (label eval-dispatch))

     ev-appl-did-operator
     (restore unev) ; the operands
     (restore env)
     (assign argl (op empty-arglist))
     (assign proc (reg val)) ; the operator
     (test (op no-operands?) (reg unev))
     (branch (label apply-dispatch))
     (save proc)

     ev-appl-operand-loop
     (save argl)
     (assign exp (op first-operand) (reg unev))
     (test (op last-operand?) (reg unev))
     (branch (label ev-appl-last-arg)) ; evlis tail recursion
     (save env)
     (save unev)
     (assign continue (label ev-appl-accumulate-arg))
     (goto (label eval-dispatch))

     ev-appl-accumulate-arg
     (restore unev)
     (restore env)
     (restore argl)
     (assign argl (op adjoin-arg) (reg val) (reg argl))
     (assign unev (op rest-operands) (reg unev))
     (goto (label ev-appl-operand-loop))

     ;; evlis tail recursion
     ;;
     ;; There is no need to save the environment or the list of
     ;; unevaluated operands before going to eval-dispatch, since
     ;; they will not be required after the last operand is evaluated.
     ev-appl-last-arg
     (assign continue (label ev-appl-accum-last-arg))
     (goto (label eval-dispatch))

     ev-appl-accum-last-arg
     (restore argl)
     (assign argl (op adjoin-arg) (reg val) (reg argl))
     (restore proc)
     (goto (label apply-dispatch))


     ;; Procedure application
     ;;
     ;; The 'proc' register contains the procedure to apply
     ;; The 'argl' register contains the list of evaluated
     ;;            arguments to which it must be applied
     ;;
     ;; The saved value of 'continue' is on the stack.
     ;;
     ;; Either the procedure to be applied is a primitive
     ;; or it is a compound procedure.
     apply-dispatch
     (test (op primitive-procedure?) (reg proc))
     (branch (label primitive-apply))
     (test (op compound-procedure?) (reg proc))
     (branch (label compound-apply))
     (goto (label unknown-procedure-type))

     primitive-apply
     (assign val (op apply-primitive-procedure)
             (reg proc)
             (reg argl))
     (restore continue)
     (goto (reg continue))

     compound-apply
     (assign unev (op procedure-parameters) (reg proc))
     (assign env (op procedure-environment) (reg proc))
     (assign env (op extend-environment)
             (reg unev) (reg argl) (reg env))
     (assign unev (op procedure-body) (reg proc))
     (goto (label ev-sequence))


     ;; Sequence Evaluation and tail recursion
     ;;
     ;; Handles sequences of expressions in
     ;; - explicit 'begin' expressions
     ;; - procedure bodies
     ev-sequence
     (assign exp (op first-exp) (reg unev))
     (test (op last-exp?) (reg unev))
     (branch (label ev-sequence-last-exp))
     (save unev)
     (save env)
     (assign continue (label ev-sequence-continue))
     (goto (label eval-dispatch))
     
     ev-sequence-continue
     (restore env)
     (restore unev)
     (assign unev (op rest-exps) (reg unev))
     (goto (label ev-sequence))
     
     ev-sequence-last-exp
     (restore continue)
     (goto (label eval-dispatch))


     ;; Sequence Evaluation without tail recursion
     ;; ev-sequence
     ;; (test (op no-more-exps?) (reg unev))
     ;; (branch (label ev-sequence-end))
     ;; (assign exp (op first-exp) (reg unev))
     ;; (save unev)
     ;; (save env)
     ;; (assign continue (label ev-sequence-continue))
     ;; (goto (label eval-dispatch))
     
     ;; ev-sequence-continue
     ;; (restore env)
     ;; (restore unev)
     ;; (assign unev (op rest-exps) (reg unev))
     ;; (goto (label ev-sequence))
     
     ;; ev-sequence-end
     ;; (restore continue)
     ;; (goto (reg continue))


     ;; Special forms
     ;;
     ;; As with the metacircular evaluator, special forms are handled
     ;; by selectively evaluating fragments of the expression.


     ;; Begin
     ev-begin
     (assign unev (op begin-actions) (reg exp))
     (save continue)
     (goto (label ev-sequence))

     
     ;; Conditionals
     ;;
     ;; Evaluate the predicate and decide, based on
     ;; the value of predicate, whether to evaluate
     ;; the consequent or the alternative.
     ev-if
     (save exp)
     (save env)
     (save continue)
     (assign continue (label ev-if-decide))
     (assign exp (op if-predicate) (reg exp))
     (goto (label eval-dispatch)) ; evaluate the predicate

     ev-if-decide
     (restore continue)
     (restore env)
     (restore exp)
     (test (op true?) (reg val))
     (branch (label ev-if-consequent))

     ev-if-alternative
     (assign exp (op if-alternative) (reg exp))
     (goto (label eval-dispatch))

     ev-if-consequent
     (assign exp (op if-consequent) (reg exp))
     (goto (label eval-dispatch))


     ;; Cond (derived)
     ;; ev-cond
     ;; (assign exp (op cond->if) (reg exp))
     ;; (goto (label eval-dispatch)) ; OR: (goto (label ev-if))

     ev-cond
     (assign unev (op cond-clauses) (reg exp))
     (save continue)

     ev-cond-loop
     (test (op null?) (reg unev))
     (branch (label ev-cond-no-clauses))
     (assign exp (op car) (reg unev)) ; first clause
     (test (op cond-else-clause?) (reg exp))
     (branch (label ev-cond-else))
     (save unev) ; save all clauses
     (save env)
     (assign exp (op cond-predicate) (reg exp))
     (assign continue (label ev-cond-did-predicate))
     (goto (label eval-dispatch))

     ev-cond-did-predicate
     (restore env)
     (restore unev) ; restore all clauses
     (assign exp (op car) (reg unev)) ; first clause (again)
     (test (op true?) (reg val))
     (branch (label ev-cond-actions))
     (assign unev (op cdr) (reg unev))
     (goto (label ev-cond-loop))

     ev-cond-else
     (assign unev (op cdr) (reg unev))
     (test (op null?) (reg unev)) ; check if 'else' is the final clause
     (branch (label ev-cond-actions))
     (assign val (const else-clause-is-not-last))
     (goto (label signal-error))

     ev-cond-actions
     (assign unev (op cond-actions) (reg exp))
     (goto (label ev-sequence))

     ev-cond-no-clauses
     (assign val (const false))
     (restore continue)
     (goto (reg continue))


     ;; Let (derived)
     ev-let
     (assign exp (op let->combination) (reg exp))
     (goto (label eval-dispatch)) ; OR: (goto (label ev-application))


     ;; Assignments
     ;;
     ;; Evaluate the value part of the expression and
     ;; then install the new value in the environment.
     ev-assignment
     (assign unev (op assignment-variable) (reg exp))
     (save unev) ; save variable for later
     (assign exp (op assignment-value) (reg exp))
     (save env)
     (save continue)
     (assign continue (label ev-assignment-1))
     (goto (label eval-dispatch)) ; evaluate the assignment value

     ev-assignment-1
     (restore continue)
     (restore env)
     (restore unev)
     (perform (op set-variable-value!)
              (reg unev) (reg val) (reg env))
     (assign val (const ok))
     (goto (reg continue))


     ;; Definitions
     ;;
     ;; Evaluate the value part of the expression and
     ;; then define the new binding in the environment.
     ev-definition
     (assign unev (op definition-variable) (reg exp))
     (save unev) ; save variable for later
     (assign exp (op definition-value) (reg exp))
     (save env)
     (save continue)
     (assign continue (label ev-definition-1))
     (goto (label eval-dispatch)) ; evaluate the definition value
     
     ev-definition-1
     (restore continue)
     (restore env)
     (restore unev)
     (perform (op define-variable!)
              (reg unev) (reg val) (reg env))
     (assign val (const ok))
     (goto (reg continue))


     ;; Error handling
     ;;
     ;; Print an error message and return to the driver loop.
     unknown-expression-type
     (assign val (const unknown-expression-type-error))
     (goto (label signal-error))

     unknown-procedure-type
     (restore continue) ; clean up stack (from apply-dispatch)
     (assign val (const unknown-procedure-type-error))
     (goto (label signal-error))

     signal-error
     (perform (op user-print) (reg val))
     (goto (label read-eval-print-loop))
     
     )))
