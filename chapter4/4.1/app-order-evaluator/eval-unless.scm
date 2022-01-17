;; Eval Unless
;;
;; Extends the given evaluator with 'unless' expressions.
;;
;; 'unless' is a derived expression and is implemented
;; based on the special form 'if'.

(define (install-eval-unless evaluator)
  (let ((_eval (evaluator '_eval))
        (_analyze (evaluator '_analyze))
        (extend-eval (evaluator 'extend-eval))
        (extend-analyze (evaluator 'extend-analyze))
        (get-constructor (evaluator 'get-constructor)))

    (define (eval-unless exp env)
      (_eval (unless->if exp) env))

    (define (analyze-unless exp)
      (_analyze (unless->if exp)))

    (define (unless->if exp)
      (make-if (unless-predicate exp)
               (unless-alternative exp)
               (unless-consequent exp)))

    (define (unless-predicate exp) (cadr exp))
    (define (unless-consequent exp) (caddr exp))
    (define (unless-alternative exp)
      (if (not (null? (cdddr exp)))
          (cadddr exp)
          'false))

    ;; Dependency constructors
    (define (make-if . args)
      (apply (get-constructor 'make-if) args))

    (extend-eval 'unless eval-unless)
    (extend-analyze 'unless analyze-unless)))
