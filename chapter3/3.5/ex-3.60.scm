;; Exercise 3.60
;;
;; With power series represented as streams of coefficients as in exercise 3.59,
;; adding series is implemented by 'add-streams'.
;;
;; Complete the definition of the following procedure for multiplying series:

(load "workbook.scm")
(load "ex-3.59.scm")

;; We can get the power series for the sum/product of two infinite series
;; as if they were polynomials:

(define (mul-series s1 s2)
  (cons-stream (* (stream-car s1)
                  (stream-car s2))
               (add-streams
                (scale-stream (stream-cdr s2) (stream-car s1))
                (mul-series (stream-cdr s1) s2))))

;; Test:  sin^x + cos^x = 1
;;
;; (define one (add-streams
;;              (mul-series sine-series sine-series)
;;              (mul-series cosine-series cosine-series)))
;;
;; (stream-ref one 0) ; 1
;; (stream-ref one 1) ; 0
;; (stream-ref one 2) ; 0
;; (stream-ref one 3) ; 0
;; (stream-ref one 4) ; 0, etc.

;; See: https://en.wikipedia.org/wiki/Cauchy_product
;; See: https://web.ma.utexas.edu/users/m408s/AS/LM11-10-5.html
