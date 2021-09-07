;; Exercise 2.18
;;
;; Define a procedure 'reverse' that takes a list as argument and
;; returns a list of the same elements in reverse order:
;;
;; (reverse (list 1 4 9 16 25))
;; (25 16 9 4 1)


;; Below, 'reverse' is implemented using an iterative plan.
;; To reverse list list1, do the following:
;; - if list1 is the empty list, then the result is the accumulator 'result'
;; - otherwise, cdr list1 and append the car of list1 onto the accumulator 'result'
;;
;; The initial value of the accumulator 'result' is the empty list.

(define (reverse items)
  (define (iter l result)
    (if (null? l)
        result
        (iter (cdr l) (cons (car l) result))))
  (iter items '()))

;; (reverse (list 1 4 9 16 25)) ; (25 16 9 4 1)
