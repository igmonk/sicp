;; Arithmetic package - tests

(load "arith_num_package.scm")


;; (define int1 5)
;; (define rat1 (make-rational 1 5))
;; (define real1 5.1)
;; (define z1 (make-complex-from-real-imag 5 1))
;; (define z2 (make-complex-from-mag-ang 5 1))


;; Test coercion with add5 and mul5

;; (add5 1 2 3 4 5) ; 15
;; (mul5 1 2 3 4 5) ; 120

;; (add5 1 2 3 4 rat1) ; (rational 51 . 5)
;; (mul5 1 2 3 4 rat1) ; (rational 24 . 5)

;; (add5 1 2 3 rat1 real1) ; (rational 113 . 10)
;; (mul5 1 2 3 rat1 real1) ; 6.12

;; (add5 1 2 rat1 real1 z1) ; (complex rectangular (rational 113 . 10) . 1)
;; (mul5 1 2 rat1 real1 z1) ; (complex polar 10.40199980772928 . .19739555984988075)

;; (add5 1 2 3 4 z1)     ; (complex rectangular 15 . 1)
;; (add5 z1 z1 z1 z1 z1) ; (complex rectangular 25 . 5)
;; (mul5 z2 z2 z2 z2 z2) ; (complex polar 3125 . 5)


;; Accessing real and imaginary parts of the numbers of different types

;; (real-part int1) ; 5
;; (imag-part int1) ; 0

;; (real-part rat1) ; (rational 1 . 5)
;; (imag-part rat1) ; 0

;; (real-part real1) ; (rational 51 . 10)
;; (imag-part real1) ; 0

;; (real-part z1) ; 5
;; (imag-part z1) ; 1

;; (real-part z2) ; 2.701511529340699
;; (imag-part z2) ; 4.207354924039483


;; equ?

;; (equ? 1 1) ; true
;; (equ? 1 2) ; false

;; (equ? rat1 (make-rational 1 5)) ; true
;; (equ? rat1 (make-rational 1 3)) ; false

;; (equ? 1.1 1.1) ; true
;; (equ? 1.1 1.5) ; false

;; (equ? z1 (make-complex-from-real-imag 5 1)) ; true
;; (equ? z1 (make-complex-from-real-imag 7 2)) ; false

;; (equ? z2 (make-complex-from-mag-ang 5 1)) ; true
;; (equ? z2 (make-complex-from-mag-ang 7 2)) ; false

;; (equ? z1 z2) ; false


;; equ? with coercion

;; (equ? 1 (make-rational 1 1))               ; true
;; (equ? 1 1.0)                               ; true
;; (equ? 1 (make-complex-from-real-imag 1 0)) ; true

;; (equ? rat1 0.2)                                 ; true
;; (equ? rat1 (make-complex-from-real-imag 0.2 0)) ; true

;; (equ? real1 (make-complex-from-real-imag 5.1 0)) ; true


;; =zero?

;; (=zero? 0) ; true
;; (=zero? 1) ; false

;; (=zero? (make-rational 0 5)) ; true
;; (=zero? (make-rational 1 5)) ; false

;; (=zero? 0.0) ; true
;; (=zero? 0.1) ; false

;; (=zero? (make-complex-from-real-imag 0 0)) ; true
;; (=zero? (make-complex-from-real-imag 1 1)) ; false


;; neg

;; (neg 1)  ; -1
;; (neg -1) ; 1

;; (neg (make-rational 1 5))  ; (rational -1 . 5)
;; (neg (make-rational -1 5)) ; (rational 1 . 5)

;; (neg 1.1)  ; (rational -11 . 10)
;; (neg -1.1) ; (rational 11 . 10)

;; (neg (make-complex-from-real-imag 2 1))   ; (complex rectangular -2 . -1)
;; (neg (make-complex-from-real-imag -2 -1)) ; (complex rectangular 2 . 1)

;; (neg (make-complex-from-real-imag
;;       (make-rational 1 3)
;;       (make-rational 1 7))) ; (complex rectangular (rational -1 . 3) rational -1 . 7)
