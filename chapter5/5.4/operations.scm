(load "list-utils.scm")
(load "procedure.scm")

(define (self-evaluating? exp)
  (cond ((number? exp) true)
        ((string? exp) true)
        (else false)))


(define (variable? exp) (symbol? exp))


(define (application? exp) (pair? exp))
(define (operator exp) (car exp))
(define (operands exp) (cdr exp))
(define (no-operands? ops) (null? ops))
(define (first-operand ops) (car ops))
(define (rest-operands ops) (cdr ops))


(define (apply-primitive-procedure proc args)
  (apply (primitive-implementation proc) args))


(define (quoted? exp) (tagged-list? exp 'quote))
(define (text-of-quotation exp) (cadr exp))


(define (assignment? exp) (tagged-list? exp 'set!))
(define (assignment-variable exp) (cadr exp))
(define (assignment-value exp) (caddr exp))


(define (definition? exp) (tagged-list? exp 'define))
(define (make-define var value)
  (cons 'define (cons var value)))

(define (definition-variable exp)
  (if (symbol? (cadr exp))
      (cadr exp)
      (caadr exp)))

(define (definition-value exp)
  (if (symbol? (cadr exp))
      (caddr exp)
      (make-lambda (cdadr exp)   ; formal parameters
                   (cddr exp)))) ; body


(define (lambda? exp) (tagged-list? exp 'lambda))
(define (lambda-parameters exp) (cadr exp))
(define (lambda-body exp) (cddr exp))
(define (make-lambda parameters body)
  (cons 'lambda (cons parameters body)))


(define (make-if predicate consequent alternative)
  (list 'if predicate consequent alternative))
(define (if? exp) (tagged-list? exp 'if))
(define (if-predicate exp) (cadr exp))
(define (if-consequent exp) (caddr exp))
(define (if-alternative exp)
  (if (not (null? (cdddr exp)))
      (cadddr exp)
      'false))


(define (cond? exp) (tagged-list? exp 'cond))

(define (cond->if exp)
  (expand-clauses (cond-clauses exp)))

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


(define (let->combination exp)
  (if (symbol? (cadr exp)) ; if named let
      (named-let->combination exp)
      (ordinary-let->combination exp)))

(define (named-let->combination exp)
  (let ((name (cadr exp))
        (var-exp-pairs (caddr exp))
        (body (cdddr exp)))
    (let ((params (car (decouple-pairs var-exp-pairs))))
      (make-begin
       (list (make-define (cons name params) body)
             (make-let var-exp-pairs body))))))

(define (ordinary-let->combination exp)
  (let ((var-exps (decouple-pairs (let-var-exp-pairs exp)))
        (body (let-body exp)))
    (append (list (make-lambda (car var-exps) body))
            (cadr var-exps))))

(define (let? exp) (tagged-list? exp 'let))
(define (make-let var-exp-pairs body)
  (cons 'let (cons var-exp-pairs body)))
(define (let-var-exp-pairs exp) (cadr exp))
(define (let-body exp) (cddr exp))


(define (begin? exp) (tagged-list? exp 'begin))
(define (make-begin seq) (cons 'begin seq))
(define (begin-actions exp) (cdr exp))


(define (empty-arglist) '())

(define (adjoin-arg arg arglist)
  (append arglist (list arg)))

(define (last-operand? ops)
  (null? (cdr ops)))

(define (no-more-exps? seq) (null? seq))
