;; Exercise 3.37
;;
;; The celsius-fahrenheit-converter procedure is cumbersome when compared with
;; a more expression-oriented style of definition, such as

(define (celsius-fahrenheit-converter x)
  (c+ (c* (c/ (cv 9) (cv 5))
          x)
      (cv 32)))

(define C (make-connector))
(define F (celsius-fahrenheit-converter C))

;; Here c+, c*, etc. are the "constraint" versions of the arithmetic operations.
;; For example, c+ takes two connectors as arguments and returns a connector
;; that is related to these by an adder constraint:

(define (c+ x y)
  (let ((z (make-connector)))
    (adder x y z)
    z))

;; Define analogous procedures c-, c*, c/, and cv (constant value) that
;; enable us to define compound constraints as in the converter example above.


;; The expression-oriented format is convenient because it avoids
;; the need to name the intermediate expressions in a computation.
;;
;; Since Lisp allows us to return compound objects as values of procedures,
;; we can transform our imperative-style constraint language into
;; an expression-oriented style as shown in this exercise.
;;
;; The relationship between Fahrenheit and Celsius temperatures is
;;
;; 9C = 5(F - 32)
;;
;; To make use of the expression-oriented style, rewrite the equation to:
;;
;; F = 9/5 * C + 32
;;
;; The equation above reflects the celsius-fahrenheit-converter procedure.

(load "workbook.scm")

(define (cv value)
  (let ((c (make-connector)))
    (constant value c)
    c))

(define (c- x y)
  (let ((diff (make-connector)))
    (adder y diff x)
    diff))

(define (c* x y)
  (let ((p (make-connector)))
    (multiplier x y p)
    p))

(define (c/ x y)
  (let ((q (make-connector)))
    (multiplier y q x)
    q))


;; Tests
;;
;; (probe "Celsius temp" C)
;; (probe "Fahrenheit temp" F)

;; (set-value! C 25 'user)
;;
;; Probe: Celsius temp = 25
;; Probe: Fahrenheit temp = 77

;; (forget-value! C 'user)
;;
;; Probe: Celsius temp = ?
;; Probe: Fahrenheit temp = ?

;; (set-value! F 212 'user)
;;
;; Probe: Fahrenheit temp = 212
;; Probe: Celsius temp = 100
