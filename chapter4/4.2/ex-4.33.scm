;; Exercise 4.33
;;
;; Ben Bitdiddle tests the lazy list implementation given above
;; by evaluating the expression

(car '(a b c))

;; To his surprise, this produces an error.
;;
;; After some thought, he realizes that the "lists" obtained by
;; reading in quoted expressions are different from the lists
;; manipulated by the new definitions of cons, car, and cdr.
;;
;; Modify the evaluator's treatment of quoted expressions so that
;; quoted lists typed at the driver loop will produce true lazy lists.


;; When run in the driver loop:

;; Before the changes to eval-quote:
(car '(a b c))        ; Unknown procedure type -- APPLY (a b c)
(car (quote (a b c))) ; Unknown procedure type -- APPLY (a b c)

;; After the quoted lists support has been added to eval-quote:
(car '(a b c))        ; Unbound variable a
(car (quote (a b c))) ; Unbound variable a
(car '(1 2 3))        ; 1
(car (quote (1 2 3))) ; 1


;; Solution
;;
;; The solution to the task is to extend the evaluation of quoted
;; expressions to allow for lists.
;;
;; 1. eval-quote starts to differentiate between lists and symbols

(define (eval-quote exp env)
  (let ((quoted (text-of-quotation exp)))
    (if (pair? quoted)
        ((evaluator '_eval) (make-list quoted) env)
        quoted)))

;; 2. make-list recursively produces a nested structure build up
;;    of the quoted list elements

(define (make-list exps)
  (if (null? exps)
      '()
      (list 'cons
            (car exps)
            (make-list (cdr exps)))))


;; See: eval-quote.scm
;;      evaluator-test.scm
