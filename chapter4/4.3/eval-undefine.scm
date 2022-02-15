;; Eval undefine
;;
;; Removes the binding in the first frame of the environment.

(load "environment.scm")

(define (install-eval-undefine evaluator)
  (let ((extend-eval (evaluator 'extend-eval))
        (extend-analyze (evaluator 'extend-analyze)))

    (define (eval-undefine exp env)
      (undefine-variable! (undefine-var exp) env)
      'ok)

    (define (analyze-undefine exp)
      (let ((var (undefine-var exp)))
        (lambda (env succeed fail)
          (undefine-variable! var env)
          (succeed 'ok fail))))

    (define (undefine-var exp)
      (cadr exp))

    (extend-eval 'undefine eval-undefine)
    (extend-analyze 'undefine analyze-undefine)))
