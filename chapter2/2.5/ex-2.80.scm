;; Exercise 2.80
;;
;; Define a generic predicate =zero? that tests if its argument is zero,
;; and install it in the generic arithmetic package.
;;
;; This operation should work for ordinary numbers, rational numbers,
;; and complex numbers.

(load "workbook.scm")
(load "ex-2.77.scm")
(load "ex-2.78.scm")

;; Start with the generic '=zero?' operation:

(define (=zero? x) (apply-generic '=zero? x))


;; Next, install package extensions for each concrete
;; number representation:


;; 1. Scheme numbers

(define (install-scheme-number-package-ext-2-80)
  (list (export-def '=zero? '(scheme-number)
                    (lambda (x) (= x 0)))))

(define scheme-number-ext-2-80-exports
  (install-scheme-number-package-ext-2-80))


;; 2. Rational numbers

(define (install-rational-package-ext-2-80)
  ;; internal procedures (duplicated)
  (define (numer x) (car x))
  ;; interface to rest of the system
  (list (export-def '=zero? '(rational)
                    (lambda (x) (= (numer x) 0)))))

(define rational-ext-2-80-exports
  (install-rational-package-ext-2-80))


;; 3. Complex numbers

(define (install-complex-package-ext-2-80)
  (list (export-def '=zero? '(complex)
                    (lambda (z)
                      (and (= (real-part z) 0)
                           (= (imag-part z) 0))))))

(define complex-ext-2-80-exports
  (install-complex-package-ext-2-80))


;; Tests
;;
;; 1. Scheme numbers
;;
;; (=zero? 0) ; true
;; (=zero? 1) ; false
;;
;; (=zero? (make-scheme-number 0)) ; true
;; (=zero? (make-scheme-number 1)) ; false
;;
;;
;; 2. Rational numbers
;;
;; (=zero? (make-rational 0 1)) ; true
;; (=zero? (make-rational 1 2)) ; false
;;
;;
;; 3. Complex numbers
;;
;; (=zero? (make-complex-from-real-imag 0 0)) ; true
;; (=zero? (make-complex-from-real-imag 1 1)) ; false


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
                           complex-ext-2-79-exports
                           scheme-number-ext-2-80-exports
                           rational-ext-2-80-exports
                           complex-ext-2-80-exports)))
