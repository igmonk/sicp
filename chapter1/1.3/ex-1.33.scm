;; Exercise 1.33
;;
;; You can obtain an even more general version of accumulate (exercise 1.32)
;; by introducing the notion of a filter on the terms to be combined. That is,
;; combine only those terms derived from values in the range that satisfy a specified condition.
;; The resulting 'filtered-accumulate' abstraction takes the same arguments as accumulate,
;; together with an additional predicate of one argument that specifies the filter.
;; Write filtered-accumulate as a procedure.
;;
;; Show how to express the following using filtered-accumulate:
;;
;; a. the sum of the squares of the prime numbers in the interval a to b
;; (assuming that you have a prime? predicate already written)
;;
;; b. the product of all the positive integers less than n that are relatively prime to n
;; (i.e., all positive integers i < n such that GCD(i,n) = 1).


;; filtered-accumulate (recursive process)

(define (filtered-accumulate combiner null-value term a next b predicate)
  (define (self-invoke)
    (filtered-accumulate combiner null-value term (next a) next b predicate))
  (cond ((> a b) null-value)
	((predicate a) (combiner (term a) (self-invoke)))
	(else (self-invoke))))

;; filtered-accumulate (iterative process)

(define (filtered-accumulate combiner null-value term a next b predicate)
  (define (iter a result)
    (cond ((> a b) result)
	  ((predicate a) (iter (next a) (combiner (term a) result)))
	  (else (iter (next a) result))))
  (iter a null-value))


;; filtered-accumulate-based sum

(define (sum term a next b predicate)
  (filtered-accumulate + 0 term a next b predicate))

(define (sum-integers a b)
  (sum identity a inc b integer?))

;; (sum-integers 1 10)  ; 55
;; (sum-integers 1 100) ; 5050

(define (sum-even-integers a b)
  (define (even-integer? x)
    (and (integer? x) (even? x)))
  (sum identity a inc b even-integer?))

;; (sum-even-integers 1 10)  ; 30
;; (sum-even-integers 1 100) ; 2550


;; filtered-accumulate-based product

(define (product term a next b predicate)
  (filtered-accumulate * 1 term a next b predicate))

(define (product-integers a b)
  (product identity a inc b integer?))

;; (product-integers 1 5)  ; 120
;; (product-integers 1 10) ; 3628800


;; a. the sum of the squares of the prime numbers in the interval a to b

(define (sum-prime-squares a b)
  (sum square a inc b prime?))

;; (sum-prime-squares 1 10)  ; 88
;; (sum-prime-squares 1 100) ; 65797


;; b. the product of all the positive integers less than n that are relatively prime to n

(define (product-pos-integers-prime-n n)
  (define (pos-integer-prime-to-n? x)
    (and (positive? x) (= (gcd x n) 1))
  (product identity 1 inc (- n 1) pos-integer-prime-to-n?))

;; (product-pos-integers-prime-n 0)  ; 1
;; (product-pos-integers-prime-n 1)  ; 1
;; (product-pos-integers-prime-n 2)  ; 1
;; (product-pos-integers-prime-n 3)  ; 2    = 1 * 2
;; (product-pos-integers-prime-n 4)  ; 3    = 1 * 3
;; (product-pos-integers-prime-n 5)  ; 24   = 1 * 2 * 3 * 4
;; (product-pos-integers-prime-n 6)  ; 5    = 1 * 5
;; (product-pos-integers-prime-n 7)  ; 720  = 1 * 2 * 3 * 4 * 5 * 6
;; (product-pos-integers-prime-n 8)  ; 105  = 1 * 3 * 5 * 7
;; (product-pos-integers-prime-n 9)  ; 2240 = 1 * 2 * 4 * 5 * 7 * 8
;; (product-pos-integers-prime-n 10) ; 189  = 1 * 3 * 7 * 9


;; Utils
(define (identity x) x)
(define (inc n) (+ n 1))
(define (square x) (* x x))

(define (prime? n)
  (not (div-iter n 2)))

(define (div-iter n divisor)
  (cond ((< n (square divisor)) false)
	((divisible? n divisor) true)
	(else (div-iter n (+ divisor 1)))))

(define (divisible? n divisor)
  (= (remainder n divisor) 0))

(define (gcd a b)
  (if (= b 0)
      a
      (gcd b (remainder a b))))
