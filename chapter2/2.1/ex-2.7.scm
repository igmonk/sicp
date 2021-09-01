;; Exercise 2.7
;;
;; Alyssa's program is incomplete because she has not specified the implementation of
;; the interval abstraction. Here is a definition of the interval constructor:

(define (make-interval a b)
  (cons a b))

;; Define selectors 'upper-bound' and 'lower-bound' to complete the implementation.

(define (lower-bound i)
  (min (car i) (cdr i)))

(define (upper-bound i)
  (max (car i) (cdr i)))

;; (print-interval (make-interval 1 5)) ; [1,5]
;; (print-interval (make-interval 5 1)) ; [1,5]

;; (print-interval (make-interval -5 -1)) ; [-5,-1]
;; (print-interval (make-interval -1 -5)) ; [-5,-1]

(define (interval-width i)
  (/ (abs (- (upper-bound i)
             (lower-bound i)))
     2))

;; (interval-width (make-interval 1 5))  ; 2
;; (interval-width (make-interval -1 5)) ; 3

;; Utils
(define (print-interval i)
  (newline)
  (display "[")
  (display (lower-bound i))
  (display ",")
  (display (upper-bound i))
  (display "]"))
