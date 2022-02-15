;; Eval Require
;;
;; Extends the given amb evaluator with require expressions.

(load "predicates.scm")

(define (install-eval-require evaluator)
  (let ((_eval (evaluator '_eval))
        (_analyze (evaluator '_analyze))
        (extend-eval (evaluator 'extend-eval))
        (extend-analyze (evaluator 'extend-analyze))
        (def-constructor (evaluator 'def-constructor)))

    (define (eval-require exp env succeed fail)
      (if (false? (_eval (require-predicate exp) env))
          (fail)
          (succeed 'ok fail)))

    (define (analyze-require exp)
      (let ((pproc (_analyze (require-predicate exp))))
        (lambda (env succeed fail)
          (pproc env
                 (lambda (pred-value fail2)
                   (if (false? pred-value)
                       (fail2)
                       (succeed 'ok fail2)))
                 fail))))

    (define (make-require predicate)
      (list 'require predicate))

    (define (require-predicate exp) (cadr exp))

    (extend-eval 'require eval-require)
    (extend-analyze 'require analyze-require)
    (def-constructor 'make-require make-require)))
