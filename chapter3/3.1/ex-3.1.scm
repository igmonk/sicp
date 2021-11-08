;; Exercise 3.1
;;
;; An 'accumulator' is a procedure that is called repeatedly with
;; a single numeric argument and accumulates its arguments into a sum.
;;
;; Each time it is called, it returns the currently accumulated sum.
;; Write a procedure 'make-accumulator' that generates accumulators,
;; each maintaining an independent sum.
;;
;; The input to 'make-accumulator' should specify the initial value of the sum;
;; for example
;;
;; (define A (make-accumulator 5))
;; (A 10)
;; 15
;; (A 10)
;; 25


(define (make-accumulator sum)
  (lambda (amount)
    (set! sum (+ sum amount))
    sum))


;; (define a1 (make-accumulator 5))

;; (a1 10) ; 15
;; (a1 10) ; 25

;; (define a2 (make-accumulator 5))

;; (a2 10) ; 15
;; (a2 10) ; 25
