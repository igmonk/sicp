;; Exercise 4.12
;;
;; The procedures set-variable-value!, define-variable!, and
;; lookup-variable-value can be expressed in terms of
;; more abstract procedures for traversing the environment structure.
;;
;; Define abstractions that capture the common patterns and
;; redefine the three procedures in terms of these abstractions.


;; Erecting an abstraction barrier between environments
;; and their frames (and bindings) - see exercise 4.11 -
;; made the procedures set-variable-value!, define-variable!, and
;; lookup-variable-value more concise (due to the binding lookup
;; has become part of frames' responsibilities).
;;
;; Moreover, an additional step was taken to move the logic
;; answerable for looking up a variable in an environment.
;; That allowed to simplify the above mentioned methods even further.
;;
;; See: environment.scm
