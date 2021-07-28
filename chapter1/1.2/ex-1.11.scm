;; Exercise 1.11
;;
;; A function f is defined by the rule that
;; f(n) = n if n<3 and
;; f(n) = f(n - 1) + 2f(n - 2) + 3f(n - 3) if n>=3.
;;
;; Write a procedure that computes f by means of a recursive process.
;; Write a procedure that computes f by means of an iterative process.

;; 1. Recursive

(define (f n)
  (if (< n 3)
      n
      (+ (f (- n 1))
	 (* 2 (f (- n 2)))
	 (* 3 (f (- n 3))))))

;; (f 0) ; 0
;; (f 1) ; 1
;; (f 2) ; 2
;; (f 3) ; 4
;; (f 4) ; 11
;; (f 5) ; 25

;; 2. Iterative
;;
;; The idea is to use a triple of integers a, b, and c,
;; initialized to f(0) = 0, f(1) = 1 and f(2) = 2, respectively,
;; and to repeatedly apply the simultaneous transformations:
;; a <- b
;; b <- c
;; c <- c + 2b + 3a
;;
;; After applying this transformation n times, a, b, and c
;; will be equal, respectively, to f(n), f(n+1) and f(n+2).
;;
;; Thus, we can compute f(n) iteratively using the procedure:

(define (f n)
  (f-iter 0 1 2 n))

(define (f-iter a b c n)
  (if (> n 0)
      (f-iter b c (+ c (* 2 b) (* 3 a)) (- n 1))
      a))

;; (f 0) ; 0
;; (f 1) ; 1
;; (f 2) ; 2
;; (f 3) ; 4
;; (f 4) ; 11
;; (f 5) ; 25
