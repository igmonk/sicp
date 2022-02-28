;; Pattern instantiator
;;
;; To instantiate an expression, we copy it, replacing any variables
;; in the expression by their values in a given frame.
;;
;; The values are themselves instantiated, since they could contain
;; variables (for example, if ?x in exp is bound to ?y as the result
;; of unification and ?y is in turn bound to 5).
;;
;; The action to take if a variable cannot be instantiated
;; is given by a procedural argument to instantiate.

(load "vars.scm")
(load "frame.scm")

(define (instantiate exp frame unbound-var-handler)
  (define (copy exp)
    (cond ((var? exp)
           (let ((binding (binding-in-frame exp frame)))
             (if binding
                 (copy (binding-value binding))
                 (unbound-var-handler exp frame))))
          ((pair? exp)
           (cons (copy (car exp)) (copy (cdr exp))))
          (else exp)))
  (copy exp))
