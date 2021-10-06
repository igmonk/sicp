;; Exercise 2.57
;;
;; Extend the differentiation program to handle sums and products
;; of arbitrary numbers of (two or more) terms.
;;
;; Then the example below
;;
;; (deriv '(* (* x y) (+ x 3)) 'x)
;;
;; could be expressed as
;;
;; (deriv '(* x y (+ x 3)) 'x)
;;
;; Try to do this by changing only the representation for sums and products,
;; without changing the deriv procedure at all.
;;
;; For example, the addend of a sum would be the first term, and
;; the augend would be the sum of the rest of the terms.

(load "../../common.scm")
(load "workbook.scm")
(load "ex-2.56.scm")

(define (augend s)
  (accumulate make-sum 0 (cddr s)))

(define (multiplicand p)
  (accumulate make-product 1 (cddr p)))


;; (deriv '(* (* x y) (+ x 3)) 'x) ; (+ (* x y) (* (+ x 3) y))
;; (deriv '(* x y (+ x 3)) 'x)     ; (+ (* x y) (* y (+ x 3)))
;;
;; (deriv '(+ (+ x y) (* x 3)) 'x) ; 4
;; (deriv '(+ x y (* x 3)) 'x)     ; 4
