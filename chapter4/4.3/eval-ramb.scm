;; Eval ramb
;;
;; Extends the given evaluator with ramb expressions.

(load "list-utils.scm")

(define (install-eval-ramb evaluator)
  (let ((_eval (evaluator '_eval))
        (_analyze (evaluator '_analyze))
        (extend-eval (evaluator 'extend-eval))
        (extend-analyze (evaluator 'extend-analyze)))

    (define (eval-ramb exp env)
      (error "Not supported yet -- EVAL-RAMB" exp))

    (define (analyze-ramb exp)
      (let ((cprocs (map _analyze (ramb-choices exp))))
        (lambda (env succeed fail)
          (define (try-next choices)
            (if (null? choices)
                (fail)
                (let ((rand-index (random (length choices))))
                  (let ((first (list-ref choices rand-index))
                        (rest (list-except choices rand-index)))
                    (first env
                           succeed
                           (lambda ()
                             (try-next rest)))))))
          (try-next cprocs))))

    (define (ramb-choices exp) (cdr exp))

    (extend-eval 'ramb eval-ramb)
    (extend-analyze 'ramb analyze-ramb)))
