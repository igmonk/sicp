;; Exercise 3.8
;;
;; When we defined the evaluation model in section 1.1.3,
;; we said that the first step in evaluating an expression is
;; to evaluate its subexpressions.
;;
;; But we never specified the order in which the subexpressions
;; should be evaluated (e.g., left to right or right to left).
;;
;; When we introduce assignment, the order in which the arguments
;; to a procedure are evaluated can make a difference to the result.
;;
;; Define a simple procedure f such that evaluating (+ (f 0) (f 1))
;; will return 0 if the arguments to + are evaluated from left to right
;; but will return 1 if the arguments are evaluated from right to left.


;; Define a function that updates its internal state to be
;; the same as the given argument and returns the previous
;; version of the state.

(define f
  (let ((n 0))
    (lambda (x)
      (let ((old-n n))
        (set! n x)
        old-n))))


;; (f 1) ; 0
;; (f 2) ; 1
;; (f 3) ; 2
;; (f 7) ; 3
;; (f 1) ; 7
;; (f 0) ; 0
;; (f 0) ; 0


;; Test 1: with the given order of arguments to +
;;
;; The initial value of the hidden state of f is 0
;;
;; (+ (f 0) (f 1)) ; 1 => arguments are evaluated from right to left:
;;                        a) (f 1) changes the internal value to 1 and returns 0
;;                        b) (f 0) changes the internal value to 0 and returns 1
;;                        c) (+ 1 0)
;;
;; Check the hidden state:
;; (f 0) ; 0 => (f 0) was invoked first in the expression above.


;; Test 2: with the opposite order of the arguments to +
;;
;; The initial value of the hidden state of f is 0
;;
;; (+ (f 1) (f 0)) ; 0 => arguments are evaluated from right to left:
;;                        a) (f 0) changes the internal value to 0 and returns 0
;;                        b) (f 1) changes the internal value to 1 and returns 0
;;                        c) (+ 0 0)
;;
;; Check the hidden state:
;; (f 0) ; 1
