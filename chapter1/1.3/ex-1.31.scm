;; Exercise 1.31
;;
;; a. The sum procedure is only the simplest of a vast number of similar abstractions
;; that can be captured as higher-order procedures.
;; Write an analogous procedure called product that returns the product of the values
;; of a function at points over a given range.
;; Show how to define factorial in terms of product.
;; Also use product to compute approximations to pi using the formula
;;
;; pi/4 = 2/3 * 4/3 * 4/5 * 6/5 * 6/7 * 8/7 ...
;;
;; b. If your product procedure generates a recursive process,
;; write one that generates an iterative process.
;; If it generates an iterative process, write one that generates a recursive process

(define (identity x) x)

(define (inc n) (+ n 1))

;; a. The product procedure (recursive process).

(define (product term a next b)
  (if (> a b)
      1
      (* (term a)
	 (product term (next a) next b))))

;; b. The product procedure (iterative process).

(define (product term a next b)
  (define (iter a result)
    (if (> a b)
	result
	(iter (next a) (* (term a) result))))
  (iter a 1))


;; Integers product

(define (product-integers a b)
  (product identity a inc b))

;; (product-integers 1 5)  ; 120
;; (product-integers 1 10) ; 3628800


;; Factorial

(define (factorial n)
  (product identity 1 inc n))

;; (factorial 1)  ; 1
;; (factorial 5)  ; 120
;; (factorial 10) ; 3628800


;; Approximations to pi
;;
;; 1: 2/3 = n+1/n+2
;; 2: 4/3 = n+2/n+1
;; 3: 4/5 = n+1/n+2
;; 4: 6/5 = n+2/n+1
;; 5: 6/7 = n+1/n+2
;; 6: 8/7 = n+2/n+1
;;
;; n is odd  => term = n+1/n+2
;; n is even => term = n+2/n+1

(define (pi-product a b)
  (define (pi-term x)
    (if (odd? x)
	(/ (+ x 1) (+ x 2))
	(/ (+ x 2) (+ x 1))))
  (product pi-term a inc b))

;; (* 4.0 (pi-product 1 10))   ; 3.2751010413348074
;; (* 4.0 (pi-product 1 100))  ; 3.1570301764551676
;; (* 4.0 (pi-product 1 1000)) ; 3.1431607055322663
