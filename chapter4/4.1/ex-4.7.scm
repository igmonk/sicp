;; Exercise 4.7
;;
;; let* is similar to let,
;; except that the bindings of the let variables
;; are performed sequentially from left to right,
;; and each binding is made in an environment in which
;; all of the preceding bindings are visible.
;;
;; For example

(let* ((x 3)
       (y (+ x 2))
       (z (+ x y 5)))
  (* x z))

;; returns 39.
;;
;; Explain how a let* expression can be rewritten as a set of
;; nested let expressions, and write a procedure let*->nested-lets
;; that performs this transformation.

;; Below is the form that is equivalent to the one above:

(let ((x 3))
  (let ((y (+ x 2)))
    (let ((z (+ x y 5)))
      (* x z)))) ; 39

;; If we have already implemented let (exercise 4.6) and
;; we want to extend the evaluator to handle let*,
;; is it sufficient to add a clause to eval whose action is

(eval (let*->nested-lets exp) env)

;; or must we explicitly expand let* in terms of non-derived expressions?


;; It is sufficient to either add a clause to eval or
;; add a corresponding record to the evaluator's dispatch table.
;;
;; See: eval-let-star.scm
;;      evaluator-tests.scm
