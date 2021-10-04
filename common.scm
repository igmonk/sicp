;; Some common definitions used globally across all the chapters.

(define (square x)
  (* x x))

(define (cube x)
  (* x x x))

(define (inc n)
  (+ n 1))

(define (dec n)
  (- n 1))

(define (identity x) x)

(define (fib-iter a b n)
  (if (> n 0)
      (fib-iter (+ a b) a (- n 1))
      b))

(define (fib n)
  (fib-iter 1 0 n))

(define (prime? n)
  (not (div-iter n 2)))

(define (div-iter n divisor)
  (cond ((< n (square divisor)) false)
	((divisible? n divisor) true)
	(else (div-iter n (+ divisor 1)))))

(define (divisible? n divisor)
  (= (remainder n divisor) 0))
