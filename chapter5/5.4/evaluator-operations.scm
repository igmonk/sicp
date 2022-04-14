;; Explicit-Control Evaluator Operations

(load "list-utils.scm")
(load "operations.scm")
(load "procedure.scm")
(load "predicates.scm")
(load "environment.scm")
(load "repl-utils.scm")
(load "env-ext.scm")

(define eceval-operations
  (list (list 'self-evaluating? self-evaluating?)
        (list 'variable? variable?)
        (list 'quoted? quoted?)
        (list 'assignment? assignment?)
        (list 'definition? definition?)
        (list 'if? if?)
        (list 'cond? cond?)
        (list 'lambda? lambda?)
        (list 'begin? begin?)
        (list 'let? let?)
        (list 'application? application?)
        (list 'null? null?)
        (list 'car car)
        (list 'cdr cdr)
        (list 'cons cons)
        (list 'list list)
        (list 'append append)
        (list 'reverse reverse)

        (list 'true? true?)
        (list 'false? false?)

        (list 'lookup-variable-value lookup-variable-value)
        (list 'text-of-quotation text-of-quotation)

        (list 'lambda-parameters lambda-parameters)
        (list 'lambda-body lambda-body)
        (list 'make-procedure make-procedure)

        (list 'operands operands)
        (list 'operator operator)
        (list 'empty-arglist empty-arglist)
        (list 'no-operands? no-operands?)
        (list 'first-operand first-operand)
        (list 'last-operand? last-operand?)
        (list 'rest-operands rest-operands)
        (list 'adjoin-arg adjoin-arg)

        (list 'primitive-procedure? primitive-procedure?)
        (list 'compound-procedure? compound-procedure?)

        (list 'apply-primitive-procedure apply-primitive-procedure)

        (list 'procedure-parameters procedure-parameters)
        (list 'procedure-environment procedure-environment)
        (list 'procedure-body procedure-body)

        (list 'get-global-environment get-global-environment)
        (list 'extend-environment extend-environment)
        (list 'set-variable-value! set-variable-value!)
        (list 'define-variable! define-variable!)

        (list 'begin-actions begin-actions)

        (list 'first-exp first-exp)
        (list 'last-exp? last-exp?)
        (list 'rest-exps rest-exps)

        (list 'if-predicate if-predicate)
        (list 'if-alternative if-alternative)
        (list 'if-consequent if-consequent)

        (list 'cond->if cond->if)
        (list 'let->combination let->combination)
        (list 'cond-clauses cond-clauses)
        (list 'cond-predicate cond-predicate)
        (list 'cond-actions cond-actions)
        (list 'cond-else-clause? cond-else-clause?)
        
        (list 'assignment-variable assignment-variable)
        (list 'assignment-value assignment-value)
        (list 'definition-variable definition-variable)
        (list 'definition-value definition-value)

        (list 'make-define make-define)

        (list 'prompt-for-input prompt-for-input)
        (list 'read read)
        (list 'announce-output announce-output)
        (list 'user-print user-print)

        (list 'no-more-exps? no-more-exps?)

        (list 'make-compiled-procedure make-compiled-procedure)
        (list 'compiled-procedure? compiled-procedure?)
        (list 'compiled-procedure-entry compiled-procedure-entry)
        (list 'compiled-procedure-env compiled-procedure-env)

        ))
