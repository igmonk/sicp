;; Exercise 5.27
;;
;; For comparison with exercise 5.26, explore the behavior of
;; the following procedure for computing factorials recursively:

(define (factorial n)
  (if (= n 1)
      1
      (* (factorial (- n 1)) n)))

;; By running this procedure with the monitored stack, determine,
;; as a function of n, the maximum depth of the stack and
;; the total number of pushes used in evaluating n! for n > 1.

;; Start the evaluator machine and run the driver loop:
(set-register-contents! ec-eval-machine 'flag false)
(start ec-eval-machine)

(factorial 1) ; (total-pushes = 16 maximum-depth = 8)
(factorial 2) ; (total-pushes = 48 maximum-depth = 13)
(factorial 3) ; (total-pushes = 80 maximum-depth = 18)
(factorial 4) ; (total-pushes = 112 maximum-depth = 23)
(factorial 5) ; (total-pushes = 144 maximum-depth = 28)

;; total-pushes
;;
;;       slope: (48 - 16) / (2 - 1) = 32
;; y-intercept: 32 * 1 + y0 = 16
;;              y0 = -16
;;
;; Line equation: y = 32x - 16

;; maximum-depth
;;
;;       slope: (13 - 8) / (2 - 1) = 5
;; y-intercept: 5 * 1 + y0 = 8
;;              y0 = 3
;;
;; Line equation: y = 5x + 3


;; (Again, these functions will be linear.)
;;
;; Summarize your experiments by filling in the following table
;; with the appropriate expressions in terms of n:
;;
;;           | Maximum depth | Number of pushes
;; ---------------------------------------------
;; Recursive |     5n + 3    |    32n - 16
;; factorial |               |
;; ---------------------------------------------
;; Iterative |       10      |    35n + 29
;; factorial |               |


;; The maximum depth is a measure of the amount of space
;; used by the evaluator in carrying out the computation,
;; and the number of pushes correlates well with the time required.

