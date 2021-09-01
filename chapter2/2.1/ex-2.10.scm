;; Exercise 2.10
;;
;; Ben Bitdiddle, an expert systems programmer, looks over Alyssa's shoulder and comments
;; that it is not clear what it means to divide by an interval that spans zero.
;;
;; Modify Alyssa's code to check for this condition and to signal an error if it occurs.

(load "workbook.scm")
(load "ex-2.7.scm")

(define (div-interval x y)
  (if (<= (* (lower-bound y) (upper-bound y)) 0)
      (error "Division by an interval that spans zero" y)
      (mul-interval x
                (make-interval (/ 1.0 (upper-bound y))
                               (/ 1.0 (lower-bound y))))))

;; (define i1 (make-interval 1 5))
;; (define i2 (make-interval 2 7))
;; (define i3 (make-interval -2 7))

;; (print-interval (div-interval i1 i2)) ; [.14285714285714285,2.5]
;; (print-interval (div-interval i1 i3)) ; Division by an interval that spans zero
