;; Exercise 4.22
;;
;; Extend the evaluator in this section to support the special form let.
;; (See exercise 4.6.)


;; The analysis of the derived form let boils down to the analysis
;; of the combination it produces:
;;
;; (define (analyze-let exp)
;;   (_analyze (let->combination exp)))
;;
;; See: eval-let.scm
