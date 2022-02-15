;; Eval amb
;;
;; Extends the given evaluator with amb expressions.

(define (install-eval-amb evaluator)
  (let ((_eval (evaluator '_eval))
        (_analyze (evaluator '_analyze))
        (extend-eval (evaluator 'extend-eval))
        (extend-analyze (evaluator 'extend-analyze)))

    (define (eval-amb exp env)
      (error "Not supported yet -- EVAL-AMB" exp))

    (define (analyze-amb exp)
      (let ((cprocs (map _analyze (amb-choices exp))))
        (lambda (env succeed fail)
          (define (try-next choices)
            (if (null? choices)
                (fail)
                ((car choices)
                 env
                 succeed
                 (lambda ()
                   (try-next (cdr choices))))))
          (try-next cprocs))))

    (define (amb-choices exp) (cdr exp))

    (extend-eval 'amb eval-amb)
    (extend-analyze 'amb analyze-amb)))
