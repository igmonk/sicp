;; Exercise 5.29
;;
;; Monitor the stack operations in the tree-recursive Fibonacci computation:

(load "evaluator-machine.scm")

;; Start the evaluator machine and run the driver loop:
(start ec-eval-machine)

(define (fib n)
  (if (< n 2)
      n
      (+ (fib (- n 1)) (fib (- n 2)))))

;; a. Give a formula in terms of n for the maximum depth of the stack
;;    required to compute Fib(n) for n >= 2.
;;    Hint: In section 1.2.2 we argued that the space used by this process
;;          grows linearly with n.
;;
;; b. Give a formula for the total number of pushes used to compute
;;    Fib(n) for n >= 2.
;;    You should find that the number of pushes (which correlates well with
;;    the time used) grows exponentially with n.
;;    Hint: Let S(n) be the number of pushes used in computing Fib(n).
;;          You should be able to argue that there is a formula that
;;          expresses S(n) in terms of S(n - 1), S(n - 2), and some fixed
;;          "overhead" constant k that is independent of n.
;;          Give the formula, and say what k is.
;;    Then show that S(n) can be expressed as a Fib(n + 1) + b
;;    and give the values of a and b.


(fib 2) ; (total-pushes = 72 maximum-depth = 13)
(fib 3) ; (total-pushes = 128 maximum-depth = 18)
(fib 4) ; (total-pushes = 240 maximum-depth = 23)
(fib 5) ; (total-pushes = 408 maximum-depth = 28)
(fib 6) ; (total-pushes = 688 maximum-depth = 33)
(fib 7) ; (total-pushes = 1136 maximum-depth = 38)
(fib 8) ; (total-pushes = 1864 maximum-depth = 43)
(fib 9) ; (total-pushes = 3040 maximum-depth = 48)


;; a. Max depth = 5n + 3


;; b. total-pushes
;;
;; S(n) = S(n-1) + S(n-2) + k
;;
;; S(4) = S(3) + S(2) + k
;;  240 = 128 + 72 + k
;;    k = 40
;;
;; => S(n) = S(n-1) + S(n-2) + 40


;; b. total-pushes (in terms of Fib)
;;
;; S(n) = a * Fib(n+1) + b
;;
;; S(2) = a * Fib(3) + b
;; S(3) = a * Fib(4) + b
;;
;;  72 = a * 2 + b
;; 128 = a * 3 + b
;;
;;   a = 56
;;   b = -40
;;
;; => S(n) = 56 * Fib(n+1) - 40
