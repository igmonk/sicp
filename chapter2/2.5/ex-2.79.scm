;; Exercise 2.79
;;
;; Define a generic equality predicate equ? that tests the equality of two numbers,
;; and install it in the generic arithmetic package.
;;
;; This operation should work for ordinary numbers, rational numbers,
;; and complex numbers.

(load "workbook.scm")
(load "ex-2.77.scm")
(load "ex-2.78.scm")

;; Start with the generic 'equ?' operation:

(define (equ? x y) (apply-generic 'equ? x y))


;; Next, install package extensions for each concrete
;; number representation:


;; 1. Scheme numbers

(define (install-scheme-number-package-ext-2-79)
  (list (export-def 'equ? '(scheme-number scheme-number)
                    (lambda (x y) (= x y)))))

(define scheme-number-ext-2-79-exports
  (install-scheme-number-package-ext-2-79))


;; 2. Rational numbers

(define (install-rational-package-ext-2-79)
  ;; internal procedures (duplicated)
  (define (numer x) (car x))
  (define (denom x) (cdr x))
  ;; interface to rest of the system
  (list (export-def 'equ? '(rational rational)
                    (lambda (x y)
                      (and (= (numer x) (numer y))
                           (= (denom x) (denom y)))))))

(define rational-ext-2-79-exports
  (install-rational-package-ext-2-79))


;; 3. Complex numbers

(define (install-complex-package-ext-2-79)
  (list (export-def 'equ? '(complex complex)
                    (lambda (z1 z2)
                      (and (= (real-part z1) (real-part z2))
                           (= (imag-part z1) (imag-part z2)))))))

(define complex-ext-2-79-exports
  (install-complex-package-ext-2-79))


;; Tests
;;
;; 1. Scheme numbers
;;
;; (equ? 1 1) ; true
;; (equ? 1 2) ; false
;;
;; (equ? (make-scheme-number 1)
;;       (make-scheme-number 1)) ; true
;;
;; (equ? (make-scheme-number 1)
;;       (make-scheme-number 2)) ; false
;;
;;
;; 2. Rational numbers
;;
;; (equ? (make-rational 1 2)
;;       (make-rational 1 2)) ; true
;;
;; (equ? (make-rational 1 2)
;;       (make-rational 3 4)) ; false
;;
;;
;; 3. Complex numbers
;;
;; (equ? (make-complex-from-real-imag 10 20)
;;       (make-complex-from-real-imag 10 20)) ; true
;;
;; (equ? (make-complex-from-real-imag 10 20)
;;       (make-complex-from-real-imag 30 40)) ; false


;; Utils

(define (get op type)
   (get-export-def op
                   type
                   (append scheme-number-package-exports
                           rational-package-exports
                           rectangular-package-exports
                           polar-package-exports
                           complex-package-exports
                           additional-complex-exports
                           scheme-number-ext-2-79-exports
                           rational-ext-2-79-exports
                           complex-ext-2-79-exports)))
