;; Eval And
;;
;; Extends the given evaluator with 'and' expressions.

(load "list-utils.scm")


;; 1. 'and' as a new special form

(define (install-eval-and evaluator)
  (let ((_eval (evaluator '_eval))
        (_analyze (evaluator '_analyze))
        (extend-eval (evaluator 'extend-eval))
        (extend-analyze (evaluator 'extend-analyze)))

    (define (eval-and exp env)
      (eval-and-exps (and-exps exp) env))

    (define (eval-and-exps exps env)
      (if (null? exps)
          (_eval 'true env)
          (let ((first (_eval (first-exp exps) env)))
            (cond ((last-exp? exps) first)
                  ((not first) first) ;; false value (short-circuit eval)
                  (else (eval-and-exps (rest-exps exps) env))))))

    (define (analyze-and exp)
      (analyze-and-exps (and-exps exp)))

    (define (analyze-and-exps exps)
      (if (null? exps)
          (let ((true-proc (_analyze 'true)))
            (lambda (env) (true-proc env)))
          (let ((first-proc (_analyze (first-exp exps)))
                (rest-proc (analyze-and-exps (rest-exps exps))))
            (if (last-exp? exps)
                (lambda (env) (first-proc env))
                (lambda (env)
                  (let ((first (first-proc env)))
                    (if (not first)
                        first ;; false value (short-circuit eval)
                        (rest-proc env))))))))
    
    (define (and-exps exp) (cdr exp))

    (extend-eval 'and eval-and)
    (extend-analyze 'and analyze-and)))


;; 2. 'and' as a derived expression (using cond)
;;
;; a. General form
;;
;;    The goal is to transform
;;
;;    (and <exp-1> <exp-2> ... <exp-n>)
;;
;;    into
;;
;;    (cond ((not <exp-1>) false)
;;          ((not <exp-2>) false)
;;          ...
;;          (else <exp-n>))
;;
;; b. Empty argument list
;;
;;    The goal is to transform (and) into (cond (else true))

(define (install-eval-and-d evaluator)
  (let ((_eval (evaluator '_eval))
        (_analyze (evaluator '_analyze))
        (extend-eval (evaluator 'extend-eval))
        (extend-analyze (evaluator 'extend-analyze))
        (get-constructor (evaluator 'get-constructor)))
    
    (define (eval-and exp env)
      (_eval (and->cond exp) env))

    (define (analyze-and exp)
      (_analyze (and->cond exp)))
    
    (define (and->cond exp)
      (let ((exps (and-exps exp)))
        (if (null? exps)
            'true
            (make-cond (and-exps->cond-clauses exps)))))

    (define (and-exps exp) (cdr exp))

    (define (and-exps->cond-clauses exps)
      (if (last-exp? exps)
          (list (make-cond-else-clause (first-exp exps)))
          (cons
           (make-cond-clause (make-not (first-exp exps)) 'false)
           (and-exps->cond-clauses (rest-exps exps)))))

    (define (make-not exp)
      (list 'not exp))

    ;; Dependency constructors
    (define (make-cond . args)
      (apply (get-constructor 'make-cond) args))
    (define (make-cond-clause . args)
      (apply (get-constructor 'make-cond-clause) args))
    (define (make-cond-else-clause . args)
      (apply (get-constructor 'make-cond-else-clause) args))

    (extend-eval 'and-d eval-and)
    (extend-analyze 'and-d analyze-and)))
