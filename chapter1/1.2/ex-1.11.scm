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

;; (f -100) ; -100
;; (f -10)  ; -10
;; (f 0)    ; 0
;; (f 1)    ; 1
;; (f 2)    ; 2

;; (f 3) ; 4
;; (f 4) ; 11
;; (f 5) ; 25

;; 2. Iterative
;;
;; Since the procedure describes behaviour of a piecewise function,
;; it is reasonable to split its implementation in two, one for each range:
;; 1) n<3
;; 2) the rest (n>=3)
;;
;; Whereas, the first sub-function is as trivial as to return its argument,
;; the second one requires the introduction of new state variables.
;;
;; The idea is to use a triple of integers a, b, and c, initialized to f(0) = 0,
;; f(1) = 1 and f(2) = 2, respectively, and apply the following transformation
;; until the counter reaches 0:
;; a <- b
;; b <- c
;; c <- c + 2b + 3a
;;
;; After applying this transformation n-2 times, a, b, and c will be equal to
;; f(n-2), f(n-1) and f(n), respectively.
;;
;; Thus, we can compute f(n) iteratively using the procedure:

(define (f n)
  (if (< n 3)
      n
      (f-iter 0 1 2 (- n 2))))

(define (f-iter a b c count)
  (if (= count 0)
      c
      (f-iter b c (+ c (* 2 b) (* 3 a)) (- count 1))))

;; (f -100) ; -100
;; (f -10)  ; -10
;; (f 0)    ; 0
;; (f 1)    ; 1
;; (f 2)    ; 2

;; (f 3)  ; 4
;; (f 4)  ; 11
;; (f 5)  ; 25
