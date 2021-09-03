;; Exercise 2.12
;;
;; After debugging her program, Alyssa shows it to a potential user,
;; who complains that her program solves the wrong problem.
;; He wants a program that can deal with numbers represented as a center value
;; and an additive tolerance; for example, he wants to work with intervals such as:
;; 3.5 +- 0.15 rather than [3.35, 3.65].
;;
;; Alyssa returns to her desk and fixes this problem by supplying
;; an alternate constructor and alternate selectors:

(load "workbook.scm")
(load "ex-2.7.scm")

(define (make-center-width c w)
  (make-interval (- c w) (+ c w)))

(define (center i)
  (/ (+ (lower-bound i) (upper-bound i)) 2))

(define (width i)
  (/ (- (upper-bound i) (lower-bound i)) 2))

;; (define i1 (make-center-width 3.5 0.15))

;; (print-interval i1) ; [3.35,3.65]

;; (center i1) ; 3.5
;; (width i1)  ; .1499999999999999


;; Unfortunately, most of Alyssa's users are engineers.
;; Real engineering situations usually involve measurements with only a small uncertainty,
;; measured as the ratio of the width of the interval to the midpoint of the interval.
;;
;; Engineers usually specify percentage tolerances on the parameters of devices,
;; as in the resistor specifications given earlier.
;;
;; Define a constructor 'make-center-percent' that takes a center and a percentage tolerance
;; and produces the desired interval.
;; You must also define a selector 'percent' that produces the percentage tolerance
;; for a given interval.
;; The 'center' selector is the same as the one shown above.

(define (make-center-percent c pt)
  (make-center-width c (percent->num (* c pt))))

(define (percent i)
  (num->percent (/ (width i) (abs (center i)))))

;; (define i2 (make-center-percent 6.8 10))

;; (print-interval i2) ; [6.12,7.4799999999999995]

;; (center i2)  ; 6.8
;; (width i2)   ; .6799999999999997
;; (percent i2) ; 9.999999999999996


;; (define i3 (make-center-percent -20 10))

;; (print-interval i3) ; [-22,-18]

;; (center i3)  ; -20
;; (width i3)   ; 2
;; (percent i3) ; 10

;; Utils

(define (num->percent n)
  (* n 100))

(define (percent->num n)
  (/ n 100))
