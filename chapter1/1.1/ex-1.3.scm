;; Exercise 1.3
;;
;; Define a procedure that takes three numbers as arguments
;; and returns the sum of the squares of the two larger numbers.

(define (square x) (* x x))

;; (square 5)   ; 25
;; (square 10)  ; 100
;; (square -12) ; 144

(define (sum-of-squares x y)
  (+ (square x) (square y)))

;; (sum-of-squares 3 4)   ; 25
;; (sum-of-squares -3 -4) ; 25

(define (first-two-are-gt-or-eq a b c)
  (and (>= a c) (>= b c)))

;; (first-two-are-gt-or-eq 1 2 3) ; false
;; (first-two-are-gt-or-eq 2 3 1) ; true
;; (first-two-are-gt-or-eq 3 1 2) ; false

(define (complex-sum a b c)
  (cond ((first-two-are-gt-or-eq a b c) (sum-of-squares a b))
	((first-two-are-gt-or-eq b c a) (sum-of-squares b c))
	((first-two-are-gt-or-eq c a b) (sum-of-squares c a))))

;; (complex-sum 1 2 3)     ; 13
;; (complex-sum 2 3 1)     ; 13
;; (complex-sum 3 1 2)     ; 13
;; (complex-sum 5 -100 10) ; 125
