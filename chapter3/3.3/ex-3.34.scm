;; Exercise 3.34
;;
;; Louis Reasoner wants to build a squarer,
;; a constraint device with two terminals such that
;; the value of connector b on the second terminal
;; will always be the square of the value a on the first terminal.
;;
;; He proposes the following simple device made from a multiplier:

(define (squarer a b)
  (multiplier a a b))

;; There is a serious flaw in this idea. Explain


;; The squarer constriant implemented as given above is not able to
;; compute the value of the square root.
;;
;; Upon setting value on the connector corresponding to the product,
;; its internal multiplier won't have enough information to compute
;; neither of its factors, since both of them refer to the same
;; uninitialised connector a.
;;
;; What the generic multiplier constraint is not aware of
;; is the fact that both its factors are bound to be equal.

(load "workbook.scm")

;; (define a (make-connector))
;; (define b (make-connector))

;; (probe "a" a)
;; (probe "b" b)

;; (squarer a b)

;; (set-value! a 5 'user)

;; Probe: b = 25
;; Probe: a = 5

;; (forget-value! a 'user)

;; Probe: b = ?
;; Probe: a = ?

;; (set-value! b 36 'user)

;; Probe: b = 36

;; (has-value? a) ; false
