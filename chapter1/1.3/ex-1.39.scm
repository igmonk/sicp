;; Exercise 1.39
;;
;; A continued fraction representation of the tangent function was published in 1770
;; by the German mathematician J.H. Lambert:
;;
;; tan(x) = x / (1 - x^2 / (3 - x^2 / (5 - ...)))
;;
;; where x is in radians.
;; Define a procedure (tan-cf x k) that computes an approximation to the tangent function
;; based on Lambert's formula.
;; k specifies the number of terms to compute, as in exercise 1.37.

;; Let's find out the dependency between the index i and Ni
;;
;; i = 1 => Ni = x
;; i > 1 => Ni = x^2
;;
;; Let's find out the dependency between the index i and Di
;;
;; i = 1 => Di = 1 = 2i-1
;; i = 2 => Di = 3 = 2i-1
;; i = 3 => Di = 5 = 2i-1

(define (tan-cf x k)
  (cont-frac (lambda (i)
	       (if (= i 1)
		   x
		   (square x)))
	     (lambda (i)
	       (- (* 2.0 i) 1))
	     k))

;; cont-frac has a slightly different form in comparison to what it had in
;; exercises 1.37 and 1.38: it uses subtraction rather than summation.
;;
;; That could be achieved by making the existing version of cont-frac even more generic
;; so that it would accept one more parameter - denominator operation.
;;
;; For the sake of simplicity, it was decised to slightly adjust this implementation.

;; a. recursive process

(define (cont-frac n d k)
  (define (inner i)
    (let ((n-term (n i))
	  (d-term (d i)))
      (if (< i k)
	  (/ n-term (- d-term (inner (+ i 1))))
	  (/ n-term d-term))))
  (inner 1))

;; b. iterative process

(define (cont-frac n d k)
  (define (iter i result)
    (let ((n-term (n i))
	  (d-term (d i)))
      (if (> i 0)
	  (iter (- i 1) (/ n-term (- d-term result)))
	  result)))
  (iter k 0))

;; (tan-cf 1 5)   ; 1.5574074074074076
;; (tan-cf 1 10)  ; 1.557407724654902
;; (tan-cf 1 100) ; 1.557407724654902

;; (tan-cf 10 100) ; .6483608274590866
;; (tan-cf 11 100) ; -225.95084645419442
;; (tan-cf 99 100) ; 3.6985877190032097


;; Utils

(define (square x)
  (* x x))
