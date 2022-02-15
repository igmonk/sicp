;; Eval If
;;
;; Extends the given amb evaluator with if expressions.

(load "predicates.scm")

(define (install-eval-if evaluator)
  (let ((_eval (evaluator '_eval))
        (_analyze (evaluator '_analyze))
        (extend-eval (evaluator 'extend-eval))
        (extend-analyze (evaluator 'extend-analyze))
        (def-constructor (evaluator 'def-constructor)))

    (define (eval-if exp env)
      (if (true? (_eval (if-predicate exp) env))
          (_eval (if-consequent exp) env)
          (_eval (if-alternative exp) env)))

    (define (analyze-if exp)
      (let ((pproc (_analyze (if-predicate exp)))
            (cproc (_analyze (if-consequent exp)))
            (aproc (_analyze (if-alternative exp))))
        (lambda (env succeed fail)
          (pproc env
                 ;; success continuation for evaluating the predicate
                 ;; to obtain pred-value
                 (lambda (pred-value fail2)
                   (if (true? pred-value)
                       (cproc env succeed fail2)
                       (aproc env succeed fail2)))
                 ;; failure continuation for evaluating the predicate
                 fail))))

    (define (make-if predicate consequent alternative)
      (list 'if predicate consequent alternative))

    (define (make-if-altless predicate consequent)
      (list 'if predicate consequent))
    
    (define (if-predicate exp) (cadr exp))
    (define (if-consequent exp) (caddr exp))
    (define (if-alternative exp)
      (if (not (null? (cdddr exp)))
          (cadddr exp)
          'false))

    (extend-eval 'if eval-if)
    (extend-analyze 'if analyze-if)
    (def-constructor 'make-if make-if)
    (def-constructor 'make-if-altless make-if-altless)))
