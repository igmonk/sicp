;; Eval Permanent Assignment
;;
;; Extends the given amb evaluator with permanent assignment expressions.

(load "environment.scm")

(define (install-eval-passignment evaluator)
  (let ((_eval (evaluator '_eval))
        (_analyze (evaluator '_analyze))
        (extend-eval (evaluator 'extend-eval))
        (extend-analyze (evaluator 'extend-analyze))
        (def-constructor (evaluator 'def-constructor)))

    (define (eval-assignment exp env succeed fail)
      (set-variable-value! (assignment-variable exp)
                           (_eval (assignment-value exp) env)
                           env)
      (succeed 'ok fail))

    (define (analyze-assignment exp)
      (let ((var (assignment-variable exp))
            (vproc (_analyze (assignment-value exp))))
        (lambda (env succeed fail)
          (vproc env
                 (lambda (val fail2)
                   (set-variable-value! var val env)
                   (succeed 'ok fail2))
                 fail))))

    (define (make-permanent-set! var value)
      (cons 'permanent-set! (cons var value)))
    
    (define (assignment-variable exp) (cadr exp))
    (define (assignment-value exp) (caddr exp))

    (extend-eval 'permanent-set! eval-assignment)
    (extend-analyze 'permanent-set! analyze-assignment)
    (def-constructor 'make-permanent-set! make-permanent-set!)))
