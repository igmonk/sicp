;; Exercise 4.13
;;
;; Scheme allows us to create new bindings for variables
;; by means of define, but provides no way to get rid of bindings.
;;
;; Implement for the evaluator a special form 'make-unbound!' that
;; removes the binding of a given symbol from the environment
;; in which the make-unbound! expression is evaluated.
;;
;; This problem is not completely specified.
;;
;; For example, should we remove only the binding in the first frame
;; of the environment?
;;
;; Complete the specification and justify any choices you make.


;; It's been decided to complete the specification as follows:
;;
;;   Define a form that removes the binding in the first frame
;;   of the environment - undefine - so that the chain of
;;   enclosing environments is not affected.
;;
;; Such a choice is justified by unwillingness to alter
;; the outer state that can potentially be depended upon
;; by some other environment frames.
;;
;; See: eval-undefine.scm
;;      evaluator-tests.scm
