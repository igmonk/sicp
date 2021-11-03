;; Exercise 2.87
;;
;; Install =zero? for polynomials in the generic arithmetic package.
;; This will allow adjoin-term to work for polynomials with coefficients
;; that are themselves polynomials.


(load "arith_num_package.scm")

;; (define p0 (make-polynomial 'x (list (list 0 0))))

;; ;; x^5 + 2x^4 + 3x^2 - 2x - 5
;; (define p1
;;   (make-polynomial 'x (list (list 5 1)
;;                             (list 4 2)
;;                             (list 2 3)
;;                             (list 1 -2)
;;                             (list 0 -5))))

;; (=zero? p0) ; true
;; (=zero? p1) ; false

;; See arith_num_package.scm
