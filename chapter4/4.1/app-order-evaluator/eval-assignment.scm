;; Eval Assignment
;;
;; Extends the given evaluator with assignment expressions.

(load "environment.scm")

(define (install-eval-assignment evaluator)
  (let ((_eval (evaluator '_eval))
        (_analyze (evaluator '_analyze))
        (extend-eval (evaluator 'extend-eval))
        (extend-analyze (evaluator 'extend-analyze))
        (def-constructor (evaluator 'def-constructor)))

    (define (eval-assignment exp env)
      (set-variable-value! (assignment-variable exp)
                           (_eval (assignment-value exp) env)
                           env)
      'ok)

    (define (analyze-assignment exp)
      (let ((var (assignment-variable exp))
            (vproc (_analyze (assignment-value exp))))
        (lambda (env)
          (set-variable-value! var (vproc env) env)
          'ok)))

    (define (make-set! var value)
      (cons 'set! (cons var value)))
    
    (define (assignment-variable exp) (cadr exp))
    (define (assignment-value exp) (caddr exp))

    (extend-eval 'set! eval-assignment)
    (extend-analyze 'set! analyze-assignment)
    (def-constructor 'make-set! make-set!)))
