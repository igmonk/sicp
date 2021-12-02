;; Exercise 3.33
;;
;; Using primitive multiplier, adder, and constant constraints,
;; define a procedure averager that takes three connectors a, b,
;; and c as inputs and establishes the constraint that
;; the value of c is the average of the values of a and b.


;; Start with the equation representing the original relation:
;;
;; (a + b) / 2 = c
;;
;; Next, tranform the equation so that it contains only those operations
;; that can be modeled by the multiplier, adder and constant constraints:
;;
;; a + b = c * 2
;;
;; The diagram of the corresponding constraint network is represented below.
;;
;;        ┌--------┐       ┌--------┐   
;; a -----| a1     |   u   |     m1 |------- c
;;        |    + s |-------| p *    |
;; b -----| a2     |       |     m2 |--┐
;;        └--------┘       └--------┘  |
;;                                   v |
;;                            ┌---┐    |
;;                            | 2 |----┘
;;                            └---┘

(load "workbook-pc.scm")

(define (averager a b c)
  (let ((u (make-connector))
        (v (make-connector)))
    (adder a b u)
    (multiplier c v u)
    (constant 2 v)
    'ok))


;; Tests
;;
;; (define a (make-connector))
;; (define b (make-connector))
;; (define c (make-connector))

;; (averager a b c)

;; (probe "a" a)
;; (probe "b" b)
;; (probe "c" c)

;; (set-value! a 10 'user)
;;
;; Probe: a = 10

;; (set-value! b 20 'user)
;;
;; Probe: b = 20
;; Probe: c = 15

;; (forget-value! b 'user)
;;
;; Probe: b = ?
;; Probe: c = ?

;; (set-value! b 120 'user)
;;
;; Probe: b = 120
;; Probe: c = 65

;; (forget-value! a 'user)
;;
;; Probe: a = ?
;; Probe: c = ?

;; (set-value! a 110 'user)
;;
;; Probe: a = 110
;; Probe: c = 115


;; (forget-value! a 'user)
;; (forget-value! b 'user)

;; (set-value! c 100 'user)
;;
;; Probe: c = 100

;; (set-value! a 10 'user)
;;
;; Probe: a = 10
;; Probe: b = 190

;; (forget-value! a 'user)
;;
;; Probe: a = ?
;; Probe: b = ?

;; (set-value! b 20 'user)
;;
;; Probe: b = 20
;; Probe: a = 180
