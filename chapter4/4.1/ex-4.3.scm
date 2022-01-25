;; Exercise 4.3
;;
;; Rewrite eval so that the dispatch is done in data-directed style.
;;
;; Compare this with the data-directed differentiation procedure
;; of exercise 2.73.
;;
;; (You may use the car of a compound expression as the type
;; of the expression, as is appropriate for the syntax implemented
;; in this section.).


;; In data-directed style, the evaluator handles special forms
;; by looking up their corresponding in the dispatch table.
;;
;; The evaluator exposes a few procedures that get imported
;; by the majority of special forms:
;; - eval and analyze to allow for recursive evaluation
;; - extend-eval and extend-analyze to extend the evaluator with
;;   the evaluation/analysis procedures of a special form
;; - special form constructors (make-lambda, make-if) to avoid
;;   code repititions. Useful in derived expressions.
;;
;; See: evaluator.scm
;;      evaluator-tests.scm
;;      eval-lambda.scm, etc.
