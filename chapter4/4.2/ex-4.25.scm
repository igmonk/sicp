;; Exercise 4.25
;;
;; Suppose that (in ordinary applicative-order Scheme)
;; we define unless as shown above and then define factorial
;; in terms of unless as

(load "workbook.scm")

(define (factorial n)
  (unless (= n 1)
          (* n (factorial (- n 1)))
          1))

;; What happens if we attempt to evaluate (factorial 5)?

;; This won't work because both all the arguments to 'unless',
;; one of which is a recursive call, will be evaluated
;; before 'unless' is called.
;;
;; That will lead to the infinite recursion, which is to be
;; abruptly aborted due to the max depth limit:
;;
;; (factorial 5) ; Aborting!: maximum recursion depth exceeded


;; Will our definitions work in a normal-order language?

;; Yes.
;;
;; All the arguments to 'unless' will be delayed and, if
;; the condition evaluates to true (n = 1 as in the code above),
;; the recursive call won't be evaluated.
