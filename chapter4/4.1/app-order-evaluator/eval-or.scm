;; Eval Or
;;
;; Extends the given evaluator with 'or' expressions.

(load "list-utils.scm")


;; 1. 'or' as a new special form

(define (install-eval-or evaluator)
  (let ((_eval (evaluator '_eval))
        (_analyze (evaluator '_analyze))
        (extend-eval (evaluator 'extend-eval))
        (extend-analyze (evaluator 'extend-analyze)))

    (define (eval-or exp env)
      (eval-or-exps (or-exps exp) env))
    
    (define (eval-or-exps exps env)
      (if (null? exps)
          (_eval 'false env)
          (let ((first (_eval (first-exp exps) env)))
            (if first
                first ;; true value (short-circuit eval)
                (eval-or-exps (rest-exps exps) env)))))

    (define (analyze-or exp)
      (analyze-or-exps (or-exps exp)))

    (define (analyze-or-exps exps)
      (if (null? exps)
          (let ((false-proc (_analyze 'false)))
            (lambda (env) (false-proc env)))
          (let ((first-proc (_analyze (first-exp exps)))
                (rest-proc (analyze-or-exps (rest-exps exps))))
            (lambda (env)
              (let ((first (first-proc env)))
                (if first
                    first ;; true value (short-circuit eval)
                    (rest-proc env)))))))

    (define (or-exps exp) (cdr exp))

    (extend-eval 'or eval-or)
    (extend-analyze 'or analyze-or)))


;; 2. 'or' as a derived expression (using cond)
;;
;; a. General form
;; 
;;    The goal is to transform
;;
;;    (or <exp-1> <exp-2> ... <exp-n>)
;;
;;    into
;;
;;    (cond (<exp-1> <exp-1>)
;;          (<exp-2> <exp-1>)
;;          ...
;;          (else <exp-n>))
;;
;; b. Empty argument list
;;
;;    The goal is to transform (or) into (cond (else false))

(define (install-eval-or-d evaluator)
  (let ((_eval (evaluator '_eval))
        (_analyze (evaluator '_analyze))
        (extend-eval (evaluator 'extend-eval))
        (extend-analyze (evaluator 'extend-analyze))
        (get-constructor (evaluator 'get-constructor)))
    
    (define (eval-or exp env)
      (_eval (or->cond exp) env))

    (define (analyze-or exp)
      (_analyze (or->cond exp)))
    
    (define (or->cond exp)
      (let ((exps (or-exps exp)))
        (if (null? exps)
            'false
            (make-cond (or-exps->cond-clauses exps)))))

    (define (or-exps exp) (cdr exp))

    (define (or-exps->cond-clauses exps)
      (if (last-exp? exps)
          (list (make-cond-else-clause (first-exp exps)))
          (cons
           (make-cond-clause (first-exp exps) (first-exp exps)) ; triggers double eval!
           (or-exps->cond-clauses (rest-exps exps)))))

    ;; Dependency constructors
    (define (make-cond . args)
      (apply (get-constructor 'make-cond) args))
    (define (make-cond-clause . args)
      (apply (get-constructor 'make-cond-clause) args))
    (define (make-cond-else-clause . args)
      (apply (get-constructor 'make-cond-else-clause) args))

    (extend-eval 'or-d eval-or)
    (extend-analyze 'or-d analyze-or)))
