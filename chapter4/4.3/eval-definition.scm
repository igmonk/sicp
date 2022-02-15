;; Eval Definition
;;
;; Extends the given amb evaluator with definition expressions.

(load "environment.scm")

(define (install-eval-definition evaluator)
  (let ((_eval (evaluator '_eval))
        (_analyze (evaluator '_analyze))
        (extend-eval (evaluator 'extend-eval))
        (extend-analyze (evaluator 'extend-analyze))
        (def-constructor (evaluator 'def-constructor))
        (get-constructor (evaluator 'get-constructor)))

    (define (eval-definition exp env)
      (define-variable! (definition-variable exp)
                        (_eval (definition-value exp) env)
                        env)
      'ok)

    (define (analyze-definition exp)
      (let ((var (definition-variable exp))
            (vproc (_analyze (definition-value exp))))
        (lambda (env succeed fail)
          (vproc env
                 (lambda (val fail2)
                   (define-variable! var val env)
                   (succeed 'ok fail2))
                 fail))))
    
    (define (definition-variable exp)
      (if (symbol? (cadr exp))
          (cadr exp)
          (caadr exp)))

    (define (definition-value exp)
      (if (symbol? (cadr exp))
          (caddr exp)
          (make-lambda (cdadr exp)   ; formal parameters
                       (cddr exp)))) ; body

    (define (make-define var value)
      (cons 'define (cons var value)))

    ;; Dependency constructors
    (define (make-lambda . args)
      (apply (get-constructor 'make-lambda) args))
    
    (extend-eval 'define eval-definition)
    (extend-analyze 'define analyze-definition)
    (def-constructor 'make-define make-define)))
