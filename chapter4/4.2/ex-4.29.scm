;; Exercise 4.29
;;
;; Exhibit a program that you would expect to run much more slowly
;; without memoization than with memoization.

;; Such a program could be the good old recursive factorial:

(define (factorial n)
  (if (= n 1)
      1
      (* (factorial (- n 1)) n)))

;; The results of time measurements are represented in the table below:
;;
;;      thunk type     |    expression   | run time | gc time | real time
;; --------------------|-----------------|----------|---------|-----------
;;     non-memoized    | (factorial 100) |      .48 |     .01 |      .497
;;         memoized    | (factorial 100) |      .03 |      0. |      .029
;;     non-memoized    | (factorial 200) |     1.89 |     .01 |     1.919
;;         memoized    | (factorial 200) |      .05 |      0. |      .051
;;     non-memoized    | (factorial 500) |    11.78 |      .1 |    11.975
;;         memoized    | (factorial 500) |      .11 |      0. |      .115
;;
;; As the table above indicates, the computation time spent by
;; the evaluator with non-memoized thunks many times longer than
;; that spent by the evaluator with memoized thunks, and it
;; grows quickly as its input grows.


;; Also, consider the following interaction,
;; where the id procedure is defined as in exercise 4.27

(define count 0)
(define (id x)
  (set! count (+ count 1))
  x)

;; and count starts at 0:

(define (square x)
  (* x x))
;;; L-Eval input:
(square (id 10))
;;; L-Eval value:
<response>
;;; L-Eval input:
count
;;; L-Eval value:
<response>

;; Give the responses both when the evaluator memoizes and
;; when it does not.


;; 1. Memoized thunks

;;; L-Eval input:
(square (id 10))
;;; L-Eval value:
100
;;; L-Eval input:
count
;;; L-Eval value:
1


;; 2. Non-memoized thunks

;;; L-Eval input:
(square (id 10))
;;; L-Eval value:
100
;;; L-Eval input:
count
;;; L-Eval value:
2


;; The difference in ouputs can be easily explained:
;; upon the application of the procedure square,
;; its argument (x) gets transformed into a thunk, which,
;; when accessed in the procedure body is forced
;; - two times in case the evaluator does not memoize its thunks
;; - only once in case the evaluator memoizes its thunks
;;
;; Each evaluation (forced) of the thunk makes a call
;; to the procedure id, which increments the counter.
