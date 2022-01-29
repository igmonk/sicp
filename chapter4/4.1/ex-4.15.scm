;; Exercise 4.15
;;
;; Given a one-argument procedure p and an object a,
;; p is said to "halt" on a
;; if evaluating the expression (p a) returns a value
;; (as opposed to terminating with an error message or running forever).
;;
;; Show that it is impossible to write a procedure halts? that
;; correctly determines whether p halts on a
;; for any procedure p and object a.
;;
;; Use the following reasoning:
;; If you had such a procedure halts?, you could implement
;; the following program:

(define (run-forever) (run-forever))

(define (try p)
  (if (halts? p p)
      (run-forever)
      'halted))

;; Now consider evaluating the expression (try try) and show that
;; any possible outcome (either halting or running forever)
;; violates the intended behavior of halts?.


;; halts? must either return true or false.
;;
;; If halts? returns true, then 'try' will call run-forever
;; and never halt, which is a contradiction.
;;
;; If halts? returns false, then 'try' will halt (return 'halted),
;; because it will not call run-forever; this is also a contradiction.
;;
;; Overall, 'try' does the opposite of what halts? says 'try' should do,
;; so halts? can not return a truth value that is consistent with
;; whether 'try' halts.
;;
;; Therefore, the initial assumption that halts? is
;; a computable function must be false.


;; See the Proof concept of the Halting Problem by Christopher Strachey:
;;
;; https://en.wikipedia.org/wiki/Halting_problem#Proof_concept
