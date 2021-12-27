;; Exercise 3.79
;;
;; Generalize the solve-2nd procedure of exercise 3.78 so that
;; it can be used to solve general second-order differential equations
;;
;; d^2 * y
;; ------- = f(dy/dt, y)
;; d * t^2


(load "workbook.scm")

(define (solve-2nd-general f dt y0 dy0)
  (define y (integral-delayed (delay dy) y0 dt))
  (define dy (integral-delayed (delay ddy) dy0 dt))
  (define ddy (stream-map f dy y))
  y)
