;; Exercise 2.5
;;
;; Show that we can represent pairs of nonnegative integers using only numbers and
;; arithmetic operations if we represent the pair a and b as the integer that is
;; the product (2^a)*(3^b).
;; Give the corresponding definitions of the procedures cons, car, and cdr.

(define (cons a b)
  (* (expt 2 a) (expt 3 b)))

;; The idea behind car implementation is getting rid of factors equal to 3
;; followed by computing the logarithm base 2.

(define (car c)
  (if (= (remainder c 3) 0)
      (car (/ c 3))
      (to-exact (log2 c))))

;; The idea behind cdr implementation is getting rid of factors equal to 2
;; followed by computing the logarithm base 3.

(define (cdr c)
  (if (= (remainder c 2) 0)
      (cdr (/ c 2))
      (to-exact (log3 c))))

(define (log2 n)
  (/ (log n) (log 2)))

(define (log3 n)
  (/ (log n) (log 3)))

(define (to-exact n)
  (inexact->exact (round n)))

;; (car (cons 1 2))   ; 1
;; (car (cons 3 4))   ; 3
;; (car (cons 10 20)) ; 10

;; (cdr (cons 1 2))   ; 2
;; (cdr (cons 3 4))   ; 4
;; (cdr (cons 10 20)) ; 20
