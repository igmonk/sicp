;; Exercise 4.46
;;
;; The evaluators in sections 4.1 and 4.2 do not determine
;; what order operands are evaluated in.
;;
;; We will see that the amb evaluator evaluates them
;; from left to right.
;;
;; Explain why our parsing program wouldn't work if
;; the operands were evaluated in some other order.


;; Had it not been for the order the amb evaluator evaluates
;; operands, the parsing program wouldn't necessarily have
;; conformed to the order of the clauses in a given sentence.
