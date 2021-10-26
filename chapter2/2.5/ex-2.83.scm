;; Exercise 2.83
;;
;; Suppose you are designing a generic arithmetic system
;; for dealing with the tower of types shown in figure 2.25:
;; integer, rational, real, complex.
;;
;; For each type (except complex), design a procedure that
;; raises objects of that type one level in the tower.
;;
;; Show how to install a generic raise operation that will work
;; for each type (except complex).

(load "../export-defs.scm")
(load "workbook.scm")
(load "ex-2.78.scm")


;; Start with the generic 'raise' operation:

(define (raise x) (apply-generic 'raise x))


;; Next, install package extensions for each concrete
;; number representation:


;; 1. Integer numbers

(define (install-integer-package)
  ;; internal procedures
  (define (raise x) (make-rational x 1))
  ;; interface to rest of the system
  (list (export-def 'raise '(integer) raise)
        (export-def 'add '(integer integer)
                    (lambda (x y) (+ x y)))
        (export-def 'sub '(integer integer)
                    (lambda (x y) (- x y)))
        (export-def 'mul '(integer integer)
                    (lambda (x y) (* x y)))
        (export-def 'div '(integer integer)
                    (lambda (x y) (/ x y)))
        (export-def 'equ? '(integer integer)
                    (lambda (x y) (= x y)))
        (export-def 'make 'integer
                    (lambda (x) x))))

(define integer-package-exports
  (install-integer-package))


;; 2. Rational numbers

(define (install-rational-ext-2-83)
  ;; internal procedures
  (define (numer x) (car x))
  (define (denom x) (cdr x))
  (define (raise x)
    (exact->inexact (/ (numer x) (denom x))))
  ;; interface to rest of the system
  (list (export-def 'raise '(rational) raise)))

(define rational-ext-2-83-exports
  (install-rational-ext-2-83))


;; 3. Real numbers

(define (install-real-package)
  ;; internal procedures
  (define (raise x)
    (make-complex-from-real-imag x 0))
  ;; interface to rest of the system
  (list (export-def 'raise '(real) raise)
        (export-def 'add '(real real)
                    (lambda (x y) (+ x y)))
        (export-def 'sub '(real real)
                    (lambda (x y) (- x y)))
        (export-def 'mul '(real real)
                    (lambda (x y) (* x y)))
        (export-def 'div '(real real)
                    (lambda (x y) (/ x y)))
        (export-def 'equ? '(real real)
                    (lambda (x y) (= x y)))
        (export-def 'make 'real
                    (lambda (x) x))))

(define real-package-exports
  (install-real-package))


;; Adjustments for the type-tag procedure to account for
;; both integer and real primitive types:

(define (type-tag datum)
  (cond ((exact-integer? datum) 'integer)
        ((real? datum) 'real)
        ((pair? datum) (car datum))
        (else
         (error "Bad tagged datum -- TYPE-TAG" datum))))


;; Note that together with the definitions from ex. 2.78 and
;; redefined type-tag it becomes unnecessary to wrap the value
;; returned from the arithmetic operations of the integer and
;; real packages.
;;
;; See: https://groups.csail.mit.edu/mac/ftpdir/scheme-7.4/doc-html/scheme_5.html


;; Tests
;;
;; (raise 5)                 ; (rational 5 . 1)
;; (raise (raise 5))         ; 5.
;; (raise (raise (raise 5))) ; (complex rectangular 5. . 0)


;; Utils

(define (get op type)
   (get-export-def op
                   type
                   (append integer-package-exports
                           rational-package-exports
                           rational-ext-2-83-exports
                           real-package-exports
                           rectangular-package-exports
                           polar-package-exports
                           complex-package-exports)))
