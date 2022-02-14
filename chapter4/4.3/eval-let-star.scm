;; Eval let*
;;
;; Extends the given evaluator with 'let*' expressions.

(load "list-utils.scm")

(define (install-eval-let* evaluator)
  (let ((_eval (evaluator '_eval))
        (_analyze (evaluator '_analyze))
        (extend-eval (evaluator 'extend-eval))
        (extend-analyze (evaluator 'extend-analyze))
        (get-constructor (evaluator 'get-constructor)))

    (define (eval-let* exp env)
      (_eval (let*->let exp) env))

    (define (analyze-let* exp)
      (_analyze (let*->let exp)))
    
    (define (let*->let exp)
      (define (inner var-exp-pairs)
        (if (last-exp? var-exp-pairs)
            (make-let (list (first-exp var-exp-pairs))
                      (let*-body exp))
            (make-let (list (first-exp var-exp-pairs))
                      (list (inner (rest-exps var-exp-pairs))))))
      (inner (let*-var-exp-pairs exp)))

    (define (let*-var-exp-pairs exp) (cadr exp))
    (define (let*-body exp) (cddr exp))

    ;; Dependency constructors
    (define (make-let . args)
      (apply (get-constructor 'make-let) args))

    (extend-eval 'let* eval-let*)
    (extend-analyze 'let* analyze-let*)))
