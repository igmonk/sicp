;; Exercise 5.23
;;
;; Extend the evaluator to handle derived expressions such as
;; cond, let, and so on (section 4.1.2).
;;
;; You may "cheat" and assume that the syntax transformers
;; such as cond->if are available as machine operations.


;; Extend the entrypoint eval-dispatch as follows:

(test (op cond?) (reg exp))
(branch (label ev-cond))

(test (op let?) (reg exp))
(branch (label ev-let))


;; Add the corresponding instruction sequences:
;;
;; (we can short-circuit directly to the underlying form)

;; Cond (derived)
ev-cond
(assign exp (op cond->if) (reg exp))
(goto (label eval-dispatch)) ; OR: (goto (label ev-if))

;; Let (derived)
ev-let
(assign exp (op let->combination) (reg exp))
(goto (label eval-dispatch)) ; OR: (goto (label ev-application))


;; Install the necessary procedures as machine operations
;; (cond->if and let->combination from section 4.1).
