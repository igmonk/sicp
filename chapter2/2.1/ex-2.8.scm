;; Exercise 2.8
;;
;; Using reasoning analogous to Alyssa's,
;; describe how the difference of two intervals may be computed.
;; Define a corresponding subtraction procedure, called 'sub-interval'.

(load "workbook.scm")
(load "ex-2.7.scm")

;; The difference of two intervals.
;; The minimum value the difference could be is the difference of the lower bound of x
;; and the upper bound of y.
;; The maximum value the difference could be is the difference of the upper bound of x
;; and the lower bound of y.

(define (sub-interval x y)
  (make-interval (- (lower-bound x) (upper-bound y))
                 (- (upper-bound x) (lower-bound y))))

;; (define i1 (make-interval 1 5))
;; (define i2 (make-interval 11 15))

;; (print-interval (sub-interval i1 i2)) ; [-14,-6]
;; (print-interval (sub-interval i2 i1)) ; [6,14]


;; An alternative solution is addition with a negated second term.

(define (negate-interval i)
  (make-interval (* -1 (lower-bound i))
                 (* -1 (upper-bound i))))

;; (print-interval (negate-interval i1)) ; [-5,-1]
;; (print-interval (negate-interval i2)) ; [-15,-11]

(define (sub-interval i1 i2)
  (add-interval i1 (negate-interval i2)))

;; (print-interval (sub-interval i1 i2)) ; [-14,-6]
;; (print-interval (sub-interval i2 i1)) ; [6,14]
