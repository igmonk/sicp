;; Eval Cond
;;
;; Extends the given evaluator with cond expressions.
;;
;; cond is a derived expression, it is implemented as a nest
;; of if expressions.

(load "list-utils.scm")

(define (install-eval-cond evaluator)
  (let ((_eval (evaluator '_eval))
        (_analyze (evaluator '_analyze))
        (extend-eval (evaluator 'extend-eval))
        (extend-analyze (evaluator 'extend-analyze))
        (def-constructor (evaluator 'def-constructor))
        (get-constructor (evaluator 'get-constructor)))
    
    (define (eval-cond exp env)
      (_eval (cond->if exp) env))

    (define (analyze-cond exp)
      (_analyze (cond->if exp)))

    (define (make-cond clauses) (cons 'cond clauses))
    (define (cond-clauses exp) (cdr exp))

    (define (cond-else-clause? clause)
      (eq? (cond-predicate clause) 'else))

    (define (cond-predicate clause) (car clause))
    (define (cond-actions clause) (cdr clause))

    (define (cond-extended-clause? clause)
      (and (= (length clause) 3)
           (eq? (cadr clause) '=>)))

    (define (cond-test clause) (car clause))
    (define (cond-recipient clause) (caddr clause))

    (define (cond->if exp)
      (expand-clauses (cond-clauses exp)))

    (define (expand-clauses clauses)
      (if (null? clauses)
          'false ; no else clause
          (let ((first (car clauses))
                (rest (cdr clauses)))
            (cond ((cond-else-clause? first)
                   (if (null? rest)
                       (sequence->exp (cond-actions first))
                       (error "ELSE clause isn't last -- COND->IF" clauses)))
                  ((cond-extended-clause? first)
                   (make-if (cond-test first)
                            (list (cond-recipient first)
                                  (cond-test first))
                            (expand-clauses rest)))
                  (else
                   (make-if (cond-predicate first)
                            (sequence->exp (cond-actions first))
                            (expand-clauses rest)))))))

    (define (sequence->exp seq)
      (cond ((null? seq) seq)
            ((last-exp? seq) (first-exp seq))
            (else (make-begin seq))))

    (define (make-cond-clause predicate consequent)
      (list predicate consequent))

    (define (make-cond-else-clause alternative)
      (make-cond-clause 'else alternative))

    ;; Dependency constructors
    (define (make-if . args) (apply (get-constructor 'make-if) args))
    (define (make-begin . args) (apply (get-constructor 'make-begin) args))

    (extend-eval 'cond eval-cond)
    (extend-analyze 'cond analyze-cond)
    (def-constructor 'make-cond make-cond)
    (def-constructor 'make-cond-clause make-cond-clause)
    (def-constructor 'make-cond-else-clause make-cond-else-clause)))
