;; Exercise 5.26
;;
;; Use the monitored stack to explore the tail-recursive property
;; of the evaluator (section 5.4.2).
;;
;; Start the evaluator and define the iterative factorial procedure
;; from section 1.2.1:

(define (factorial n)
  (define (iter product counter)
    (if (> counter n)
        product
        (iter (* counter product)
              (+ counter 1))))
  (iter 1 1))

;; Run the procedure with some small values of n.
;; Record the maximum stack depth and the number of pushes
;; required to compute n! for each of these values.

;; Start the evaluator machine and run the driver loop:
(start ec-eval-machine)


;; a. You will find that the maximum depth required to evaluate n!
;;    is independent of n. What is that depth?

(factorial 0) ; (total-pushes = 29 maximum-depth = 8)
(factorial 1) ; (total-pushes = 64 maximum-depth = 10)
(factorial 2) ; (total-pushes = 99 maximum-depth = 10)
(factorial 3) ; (total-pushes = 134 maximum-depth = 10)
(factorial 4) ; (total-pushes = 169 maximum-depth = 10)
(factorial 5) ; (total-pushes = 204 maximum-depth = 10)


;; b. Determine from your data a formula in terms of n for
;;    the total number of push operations used in evaluating n!
;;    for any n > 1.
;;    Note that the number of operations used is a linear function
;;    of n and is thus determined by two constants.


;; Empirically, the line equation can be found by taking 2 points,
;; where x is the number whose factorial is to be calculated
;;       y is the total number of stack pushes:
;; 1. (1, 64)
;; 2. (2, 99)
;;
;;       slope: (99 - 64) / (2 - 1) = 35
;; y-intercept: 35 * 1 + y0 = 64
;;              y0 = 29
;;
;; Line equation: y = 35x + 29
;;
;; Notice, the y-intercept could be found by taking (0, 29).

;; Hence, the total number of push operations used in
;; evaluating n! for any n > 1 can be found as follows:
;;
;; total-pushes = 35 * n + 29


;; Below is the way to define the same dependency based on
;; the actual instructions the machine executes when it
;; calculates n!
;;
;;
;; Happens once (y-intercept):
;;
;; ev-app[factorial] -> 3 + 1 + 1
;;            ev-seq -> 2
;;            define -> 3
;;      ev-app[iter] -> 3 + 1 + 3 + 1
;;                if -> 3
;;          if[pred] -> 3 + 1 + 3 + 1
;;
;;
;; Repeats (the slope):
;; 
;;           if -> 3
;;     if[pred] -> 3 + 1 + 3 + 1
;; ev-app[iter] -> 3 + 1 + 3 + 1
;;    ev-app[*] -> 3 + 1 + 3 + 1
;;    ev-app[+] -> 3 + 1 + 3 + 1
;;
;;
;; Total[happens once] = 29
;; Total[repeats]      = 35
;;
;; Hence, for any n > 1 the line equation is same as above:
;;
;; total-pushes = 35 * n + 29
