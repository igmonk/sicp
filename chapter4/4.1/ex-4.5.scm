;; Exercise 4.5
;;
;; Scheme allows an additional syntax for cond clauses,
;; 
;; (<test> => <recipient>).
;;
;; If <test> evaluates to a true value, then <recipient> is evaluated.
;; Its value must be a procedure of one argument;
;; this procedure is then invoked on the value of the <test>, and
;; the result is returned as the value of the cond expression.
;;
;; For example

(cond ((assoc 'b '((a 1) (b 2))) => cadr)
      (else false))

;; returns 2.
;;
;; Modify the handling of cond so that it supports this extended syntax.


;; See: eval-cond.scm & evaluator-tests.scm
;;
;; In short, the cond expansion process is enriched with
;; the required extended form represented as follows:

(define (cond-extended-clause? clause)
  (and (= (length clause) 3)
       (eq? (cadr clause) '=>)))

(define (cond-test clause) (car clause))
(define (cond-recipient clause) (caddr clause))

;; The following case has been installed to the cond case analysis:

(cond ((cond-extended-clause? first)
       (make-if (cond-test first)
                (list (cond-recipient first)
                      (cond-test first))
                (expand-clauses rest)))

      ;; <rest...>
      
      )

;; Note the way the cond recipient and test are combined together.
;;
;; The list is used in order to be evaluated as
;; the function application during the evaluation process.
;;
;; So that
;;
;; (<test> => recipient)
;;
;; becomes
;;
;; (<recipient> <test>)
;;
;; when it's time to evaluate the form.


;; See: eval-cond.scm
