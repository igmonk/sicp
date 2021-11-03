;; Exercise 2.88
;;
;; Extend the polynomial system to include subtraction of polynomials.
;; (Hint: You may find it helpful to define a generic negation operation.)


(load "arith_num_package.scm")

;; (define p0 (make-polynomial 'x (list (list 0 0))))

;; ;; x^5 + 2x^4 + 3x^2 - 2x - 5
;; (define p1
;;   (make-polynomial 'x (list (list 5 1)
;;                             (list 4 2)
;;                             (list 2 3)
;;                             (list 1 -2)
;;                             (list 0 -5))))

;; (neg p0)       ; (polynomial x (0 0))
;; (neg p1)       ; (polynomial x (5 -1) (4 -2) (2 -3) (1 2) (0 5)) = -x^5 - 2x^4 - 3x^2 + 2x + 5
;; (neg (neg p1)) ; (polynomial x (5 1) (4 2) (2 3) (1 -2) (0 -5))  = x^5 + 2x^4 + 3x^2 - 2x - 5

;; (add p1 p1) ; (polynomial x (5 2) (4 4) (2 6) (1 -4) (0 -10)) = 2x^5 + 4x^4 + 6x^2 - 4x -10
;; (add p1 p0) ; (polynomial x (5 1) (4 2) (2 3) (1 -2) (0 -5))  = x^5 + 2x^4 + 3x^2 - 2x - 5

;; (sub p1 p1) ; (polynomial x)
;; (sub p1 p0) ; (polynomial x (5 1) (4 2) (2 3) (1 -2) (0 -5))

;; (=zero? (sub p1 p0)) ; false
;; (=zero? (sub p1 p1)) ; true

;; See arith_num_package.scm
