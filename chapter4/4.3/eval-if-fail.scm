;; Eval if-fail
;;
;; Extends the given amb evaluator with if-fail expressions.
;;
;; if-fail takes two expressions.
;;
;; It evaluates the first expression as usual and returns as usual
;; if the evaluation succeeds.
;;
;; If the evaluation fails, however, the value of the second expression
;; is returned.

(load "predicates.scm")

(define (install-eval-if-fail evaluator)
  (let ((_eval (evaluator '_eval))
        (_analyze (evaluator '_analyze))
        (extend-eval (evaluator 'extend-eval))
        (extend-analyze (evaluator 'extend-analyze))
        (def-constructor (evaluator 'def-constructor)))

    (define (eval-if-fail exp env succeed fail)
      (let ((exp1-value (_eval (if-fail-exp1 exp) env)))
        (if exp1-value
            (succeed exp1-value fail)
            (succeed (_eval (if-fail-exp2 exp) env) fail))))

    (define (analyze-if-fail exp)
      (let ((exp1-proc (_analyze (if-fail-exp1 exp)))
            (exp2-proc (_analyze (if-fail-exp2 exp))))
        (lambda (env succeed fail)
          (exp1-proc env
                     (lambda (exp1-value fail2)
                       (succeed exp1-value fail2))
                     (lambda ()
                       (exp2-proc env succeed fail))))))

    (define (make-if-fail exp1 exp2)
      (list 'if-fail exp1 exp2))

    (define (if-fail-exp1 exp) (cadr exp))
    (define (if-fail-exp2 exp) (caddr exp))

    (extend-eval 'if-fail eval-if-fail)
    (extend-analyze 'if-fail analyze-if-fail)
    (def-constructor 'make-if-fail make-if-fail)))
