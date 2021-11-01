;; Arithmetic operations package.
;;
;; The package incorporates all the arithmetic packages constructed
;; through the sections (and the corresponding exercises):
;; - 2.5.1 Generic Arithmetic Operations
;; - 2.5.2 Combining Data of Different Types
;;
;; The structure of the system is shown below:
;;
;; ----------------------------------------------------------------------------
;;
;;                           Programs that use numbers
;;
;; -------------------------------| add sub mul div |--------------------------
;;
;;                             Generic arithmetic package
;;
;;    | add-rat sub-rat |     | add-complex sub-complex |     |            |
;; ---|                 |-----|                         |-----|   + - * /  |---
;;    | mul-rat div-rat |  |  | mul-complex div-complex |  |  |            |
;;                         |                               |
;;                         |                               |
;;         Rational        |       Complex arithmetic      |     Ordinary
;;        Arithmetic       |-------------------------------|    Arithmetic
;;                         |   Rectangular |     Polar     |
;;-----------------------------------------------------------------------------
;;
;;
;; A tower of types is illustrated below:
;;
;;    complex
;;       ↑
;;       |
;;      real
;;       ↑
;;       |
;;    rational
;;       ↑
;;       |
;;    integer


(load "../export-defs.scm")
(load "./arith_lib.scm")
(load "./arith_integer_package.scm")
(load "./arith_rational_package.scm")
(load "./arith_real_package.scm")
(load "./arith_complex_rect_package.scm")
(load "./arith_complex_polar_package.scm")
(load "./arith_complex_package.scm")
(load "./arith_polynomial_package.scm")


;; Generic Arithmetic Operations

(define (add x y) (apply-generic 'add x y))
(define (sub x y) (apply-generic 'sub x y))
(define (mul x y) (apply-generic 'mul x y))
(define (div x y) (apply-generic 'div x y))

(define (add5 n1 n2 n3 n4 n5) (apply-generic 'add5 n1 n2 n3 n4 n5))
(define (mul5 n1 n2 n3 n4 n5) (apply-generic 'mul5 n1 n2 n3 n4 n5))

(define (equ? x y) (apply-generic 'equ? x y))
(define (=zero? x) (apply-generic '=zero? x))
(define (neg x) (apply-generic 'neg x))

(define (sine x) (apply-generic 'sine x))      ;; Not defined for complex
(define (cosine x) (apply-generic 'cosine x))  ;; Not defined for complex
(define (square x) (apply-generic 'square x))  ;; Not defined for complex
(define (sqrt x) (apply-generic 'sqrt x))      ;; Not defined for complex
(define (atan2 x y) (apply-generic 'atan x y)) ;; Not defined for complex

(define (raise x) (apply-generic 'raise x))
(define (project x) (apply-generic 'project x))


;; Import Number representation packages

(define integer-package-exports (install-integer-package))
(define rational-package-exports (install-rational-package))
(define real-package-exports (install-real-package))
(define complex-rect-package-exports (install-complex-rect-package))
(define complex-polar-package-exports (install-complex-polar-package))
(define complex-package-exports (install-complex-package))
(define polynomial-package-exports (install-polynomial-package))


;; Constructors

(define (make-rational n d)
  ((get 'make 'rational) n d))

(define (make-complex-from-real-imag x y)
  ((get 'make-from-real-imag 'complex) x y))

(define (make-complex-from-mag-ang r a)
  ((get 'make-from-mag-ang 'complex) r a))

(define (make-polynomial var terms)
  ((get 'make 'polynomial) var terms))


;; Selectors

(define (real-part x) (apply-generic 'real-part x))
(define (imag-part x) (apply-generic 'imag-part x))
(define (magnitude x) (apply-generic 'magnitude x))
(define (angle x) (apply-generic 'angle x))


;; Get exports

(define (get op type)
   (get-export-def op
                   type
                   (append integer-package-exports
                           rational-package-exports
                           real-package-exports
                           complex-rect-package-exports
                           complex-polar-package-exports
                           complex-package-exports
                           polynomial-package-exports)))
