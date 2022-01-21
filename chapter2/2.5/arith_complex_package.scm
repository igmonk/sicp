;; Complex numbers package
;;
;; The package combines both Rectangular and Polar representations
;; of complex numbers and interfaces a more general abstraction.
;;
;; The procedure project will fail if the necessary real numbers
;; package is not installed.


(load "../../common.scm")
(load "../export-defs.scm")
(load "./arith_lib.scm")

(define (install-complex-package)
  ;; utils
  (define (add x y) (apply-generic 'add x y))
  (define (sub x y) (apply-generic 'sub x y))
  (define (mul x y) (apply-generic 'mul x y))
  (define (div x y) (apply-generic 'div x y))
  (define (real-part z) (apply-generic 'real-part z))
  (define (imag-part z) (apply-generic 'imag-part z))
  (define (magnitude z) (apply-generic 'magnitude z))
  (define (angle z) (apply-generic 'angle z))

  ;; imported procedures from rectangular and polar packages
  (define (make-from-real-imag x y)
    ((get 'make-from-real-imag 'rectangular) x y))
  (define (make-from-mag-ang r a)
    ((get 'make-from-mag-ang 'polar) r a))

  ;; internal procedures
  (define (add-complex z1 z2)
    (make-from-real-imag (add (real-part z1) (real-part z2))
                         (add (imag-part z1) (imag-part z2))))
  (define (sub-complex z1 z2)
    (make-from-real-imag (sub (real-part z1) (real-part z2))
                         (sub (imag-part z1) (imag-part z2))))
  (define (mul-complex z1 z2)
    (make-from-mag-ang (mul (magnitude z1) (magnitude z2))
                       (add (angle z1) (angle z2))))
  (define (div-complex z1 z2)
    (make-from-mag-ang (div (magnitude z1) (magnitude z2))
                       (sub (angle z1) (angle z2))))
  (define (add5 z1 z2 z3 z4 z5)
    (accumulate add (tag z1)
                (map tag (list z2 z3 z4 z5))))
  (define (mul5 z1 z2 z3 z4 z5)
    (accumulate mul (tag z1)
                (map tag (list z2 z3 z4 z5))))
  (define (equ? z1 z2)
    (and (apply-generic 'equ? (real-part z1) (real-part z2))
         (apply-generic 'equ? (imag-part z1) (imag-part z2))))
  (define (zero? z)
    (and (apply-generic 'equ? (real-part z) 0)
         (apply-generic 'equ? (imag-part z) 0)))
  (define (neg-complex z)
    (make-from-real-imag (neg (real-part z))
                         (neg (imag-part z))))
  (define (project z) (real-part z))

  ;; interface to rest of the system
  (define types-arity-5
    '(complex complex complex complex complex))
  (define (tag z) (attach-tag 'complex z))
  (list (export-def 'real-part '(complex) real-part)
        (export-def 'imag-part '(complex) imag-part)
        (export-def 'magnitude '(complex) magnitude)
        (export-def 'angle '(complex) angle)
        (export-def 'add '(complex complex)
                    (lambda (z1 z2) (tag (add-complex z1 z2))))
        (export-def 'sub '(complex complex)
                    (lambda (z1 z2) (tag (sub-complex z1 z2))))
        (export-def 'mul '(complex complex)
                    (lambda (z1 z2) (tag (mul-complex z1 z2))))
        (export-def 'div '(complex complex)
                    (lambda (z1 z2) (tag (div-complex z1 z2))))
        (export-def 'add5 types-arity-5 add5)
        (export-def 'mul5 types-arity-5 mul5)
        (export-def 'equ? '(complex complex) equ?)
        (export-def '=zero? '(complex) zero?)
        (export-def 'neg '(complex)
                    (lambda (x) (tag (neg-complex x))))
        (export-def 'project '(complex) project)
        (export-def 'make-from-real-imag 'complex
                    (lambda (x y) (tag (make-from-real-imag x y))))
        (export-def 'make-from-mag-ang 'complex
                    (lambda (r a) (tag (make-from-mag-ang r a))))))
