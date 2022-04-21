;; Exercise 5.46
;;
;; Carry out an analysis like the one in exercise 5.45
;; to determine the effectiveness of compiling the
;; tree-recursive Fibonacci procedure

(define (fib n)
  (if (< n 2)
      n
      (+ (fib (- n 1)) (fib (- n 2)))))

;; compared to the effectiveness of using the special-purpose
;; Fibonacci machine of figure 5.12.
;;
;; (For measurement of the interpreted performance,
;; see exercise 5.29.)
;;
;; For Fibonacci, the time resource used is not linear in n;
;; hence the ratios of stack operations will not approach
;; a limiting value that is independent of n.


(load "compiler-factory.scm")

(define compiler1 (create-compiler))
(define compile (compiler1 'compile))

(load "compile-and-go.scm")

(compile-and-go
 '(define (fib n)
    (if (< n 2)
        n
        (+ (fib (- n 1)) (fib (- n 2))))))

(fib 2) ; (total-pushes = 17 maximum-depth = 5)
(fib 3) ; (total-pushes = 27 maximum-depth = 8)
(fib 4) ; (total-pushes = 47 maximum-depth = 11)
(fib 5) ; (total-pushes = 77 maximum-depth = 14)
(fib 6) ; (total-pushes = 127 maximum-depth = 17)

;; total-pushes
;;
;; S(n) = a * Fib(n+1) + b
;;
;; S(4) = a * Fib(5) + b
;; S(5) = a * Fib(6) + b
;;
;; 47 = a * 5 + b
;; 77 = a * 8 + b
;;
;;  a = 10
;;  b = -3
;;
;; => S(n) = 10 * Fib(n+1) - 3

;; maximum-depth
;;
;; 3n - 1


;; Recall the following formulas to compute Fib(n)
;; from exercise 5.29 and fib-machine.scm:
;;
;; interpreted:
;; - total pushes: S(n) = 56 * Fib(n+1) - 40
;; - maximum depth: 5n + 3
;;
;; special purpose machine:
;; - total pushes:
;; - maximum depth:
;;
;; and build the comparison table as shown below.


;; -----------------------|--------------------|---------------|
;;                        |     total pushes   | maximum depth |
;; -----------------------|--------------------|---------------|
;; interpreted            | 56 * Fib(n+1) - 40 |    5n + 3     |
;; special purpose (s.p.) |  3 * Fib(n+1) - 3  |    2n - 2     |
;; compiled               | 10 * Fib(n+1) - 3  |    3n - 1     |
;; -----------------------|--------------------|---------------|
;; compiled / interpreted |        5/28 (*)    |      3/5      | as n -> Inf+
;; s.p. / interpreted     |        3/56 (*)    |      2/5      | as n -> Inf+
;; -----------------------|--------------------|---------------|
;; s.p. / compiled        |        3/10 (*)    |      2/3      | as n -> Inf+
;; -----------------------|--------------------|---------------|


;; (*) The time resource used is not linear in n;
;;     hence the ratios of stack operations will not approach
;;     a limiting value that is independent of n.
