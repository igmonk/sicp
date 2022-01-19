;; Eval Lambda
;;
;; Extends the given evaluator with lambda expressions.

(load "procedure.scm")

(define (install-eval-lambda evaluator)
  (let ((_eval (evaluator '_eval))
        (_analyze (evaluator '_analyze))
        (extend-eval (evaluator 'extend-eval))
        (extend-analyze (evaluator 'extend-analyze))
        (def-constructor (evaluator 'def-constructor))
        (analyze-sequence (evaluator '_analyze-seq)))

    (define (eval-lambda exp env)
      (make-procedure (lambda-parameters exp)
                      (lambda-body exp)
                      env))

    (define (analyze-lambda exp)
      (let ((vars (lambda-parameters exp))
            (bproc (analyze-sequence (lambda-body exp))))
        (lambda (env) (make-procedure vars bproc env))))
    
    (define (make-lambda parameters body)
      (cons 'lambda (cons parameters body)))
    
    (define (lambda-parameters exp) (cadr exp))
    (define (lambda-body exp) (cddr exp))
    
    (extend-eval 'lambda eval-lambda)
    (extend-analyze 'lambda analyze-lambda)
    (def-constructor 'make-lambda make-lambda)))
